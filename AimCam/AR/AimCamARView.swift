import UIKit
import RealityKit

final class AimCamARView: ARView {
    var onTap: ((CGPoint, CGFloat) -> Void)?
    var onZoomChanged: ((CGFloat) -> Void)?

    private(set) var zoomScale: CGFloat = 1.0
    private var zoomScaleAtStart: CGFloat = 1.0

    private let minZoom: CGFloat = 1.0
    private let maxZoom: CGFloat = 3.0

    private let detectionLayer: CAShapeLayer = {
        let layer = CAShapeLayer()
        layer.strokeColor = UIColor.systemYellow.cgColor
        layer.fillColor = UIColor.clear.cgColor
        layer.lineWidth = 2.0
        layer.lineJoin = .round
        layer.lineCap = .round
        layer.opacity = 0.9
        return layer
    }()

    private let detectionCenterLayer: CAShapeLayer = {
        let layer = CAShapeLayer()
        layer.strokeColor = UIColor.systemPink.cgColor
        layer.fillColor = UIColor.clear.cgColor
        layer.lineWidth = 2.0
        layer.opacity = 0.9
        return layer
    }()

    private let roiLayer: CAShapeLayer = {
        let layer = CAShapeLayer()
        layer.strokeColor = UIColor.systemTeal.cgColor
        layer.fillColor = UIColor.clear.cgColor
        layer.lineWidth = 2.0
        layer.lineDashPattern = [6, 4]
        layer.opacity = 0.9
        return layer
    }()

    private let trackingCircleLayer: CAShapeLayer = {
        let layer = CAShapeLayer()
        layer.strokeColor = UIColor.systemGreen.cgColor
        layer.fillColor = UIColor.clear.cgColor
        layer.lineWidth = 2.0
        layer.opacity = 0.95
        return layer
    }()

    @MainActor required init(frame frameRect: CGRect) {
        super.init(frame: frameRect, cameraMode: .ar, automaticallyConfigureSession: false)
        setupGestures()
        layer.addSublayer(roiLayer)
        layer.addSublayer(detectionLayer)
        layer.addSublayer(detectionCenterLayer)
        layer.addSublayer(trackingCircleLayer)
    }

    @MainActor required dynamic init?(coder decoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupGestures() {
        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTap(_:)))
        tap.numberOfTapsRequired = 1
        addGestureRecognizer(tap)

        let pinch = UIPinchGestureRecognizer(target: self, action: #selector(handlePinch(_:)))
        addGestureRecognizer(pinch)
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        roiLayer.frame = bounds
        detectionLayer.frame = bounds
        detectionCenterLayer.frame = bounds
        trackingCircleLayer.frame = bounds
    }

    /// Draw detected edge points (and optionally a fitted circle) in view coordinates.
    @MainActor
    func setBallDetectionOverlay(contourPointsInView: [CGPoint], circleCenter: CGPoint?, circleRadius: CGFloat?) {
        let path = UIBezierPath()
        if !contourPointsInView.isEmpty {
            // Draw points as tiny dots (these are RANSAC inliers on the edge map).
            for p in contourPointsInView {
                path.append(UIBezierPath(ovalIn: CGRect(x: p.x - 1.2, y: p.y - 1.2, width: 2.4, height: 2.4)))
            }
        }
        detectionLayer.path = path.cgPath

        let centerPath = UIBezierPath()
        if let c = circleCenter {
            // Crosshair
            centerPath.move(to: CGPoint(x: c.x - 10, y: c.y))
            centerPath.addLine(to: CGPoint(x: c.x + 10, y: c.y))
            centerPath.move(to: CGPoint(x: c.x, y: c.y - 10))
            centerPath.addLine(to: CGPoint(x: c.x, y: c.y + 10))

            if let r = circleRadius, r.isFinite, r > 2 {
                centerPath.append(UIBezierPath(ovalIn: CGRect(x: c.x - r, y: c.y - r, width: 2 * r, height: 2 * r)))
            }
        }
        detectionCenterLayer.path = centerPath.cgPath
    }

    @MainActor
    func clearBallDetectionOverlay() {
        roiLayer.path = nil
        detectionLayer.path = nil
        detectionCenterLayer.path = nil
    }

    @MainActor
    func setTrackingCircle(center: CGPoint?, radius: CGFloat?) {
        guard let center, let radius, radius.isFinite, radius > 2 else {
            trackingCircleLayer.path = nil
            return
        }
        let rect = CGRect(x: center.x - radius,
                          y: center.y - radius,
                          width: radius * 2,
                          height: radius * 2)
        trackingCircleLayer.path = UIBezierPath(ovalIn: rect).cgPath
    }

    /// Flash the green tracking circle a few times as confirmation feedback.
    @MainActor
    func flashTrackingCircle(center: CGPoint, radius: CGFloat) {
        let circlePath = UIBezierPath()
        if radius.isFinite, radius > 2 {
            circlePath.append(UIBezierPath(ovalIn: CGRect(x: center.x - radius, y: center.y - radius, width: 2 * radius, height: 2 * radius)))
        }
        trackingCircleLayer.path = circlePath.cgPath
        trackingCircleLayer.lineWidth = 3.0
        trackingCircleLayer.opacity = 1.0

        // Animate opacity: flash 3 times over ~0.9s, then fade out.
        let flash = CAKeyframeAnimation(keyPath: "opacity")
        flash.values   = [1.0, 0.0, 1.0, 0.0, 1.0, 0.0]
        flash.keyTimes = [0.0, 0.17, 0.33, 0.5, 0.67, 1.0]
        flash.duration = 0.9
        flash.isRemovedOnCompletion = false
        flash.fillMode = .forwards
        trackingCircleLayer.add(flash, forKey: "flash")

        // Keep circle after flash; remove animation only.
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.trackingCircleLayer.removeAnimation(forKey: "flash")
            self?.trackingCircleLayer.opacity = 0.95
        }
    }

    /// Draw the ROI square used for Vision detection.
    @MainActor
    func setBallDetectionROIOverlay(rectInView: CGRect) {
        roiLayer.path = UIBezierPath(rect: rectInView).cgPath
    }

    @objc private func handleTap(_ recognizer: UITapGestureRecognizer) {
        guard recognizer.state == .ended else { return }
        let point = recognizer.location(in: self)
        onTap?(point, zoomScale)
    }

    @objc private func handlePinch(_ recognizer: UIPinchGestureRecognizer) {
        switch recognizer.state {
        case .began:
            zoomScaleAtStart = zoomScale
        case .changed:
            let proposed = zoomScaleAtStart * recognizer.scale
            let clamped = min(max(proposed, minZoom), maxZoom)
            zoomScale = clamped

            // Digital zoom approximation: scale the view around its center.
            transform = CGAffineTransform(scaleX: zoomScale, y: zoomScale)
            onZoomChanged?(zoomScale)
        default:
            break
        }
    }
}

