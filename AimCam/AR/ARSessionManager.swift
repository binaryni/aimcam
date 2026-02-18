import Foundation
import SwiftUI
import ARKit
import RealityKit
import simd

@MainActor
final class ARSessionManager: NSObject, ObservableObject {
    enum SelectionState {
        case pickBall
        case pickPocket
        case done
    }

    // MARK: - Published UI state
    @Published var selectionState: SelectionState = .pickBall
    @Published var statusText: String?
    @Published var debugEnabled: Bool = false
    @Published var zoomScale: CGFloat = 1.0
    @Published var cameraImageResolution: CGSize = CGSize(width: 4, height: 3)
    @Published var sessionRunning: Bool = true

    // MARK: - Tuning
    /// Default pool ball radius in meters (57.15mm diameter / 2).
    var ballRadiusMeters: Float = 0.05715 / 2.0

    // MARK: - AR references
    private weak var arView: ARView?
    private var didStartSession = false
    private var shouldResumeAfterBackground = false

    private let ballDetector = BallDetector()

    // MARK: - World state
    private var ballCenter: SIMD3<Float>?
    private var pocketCenter: SIMD3<Float>?
    private var tableSurfaceY: Float?

    private var ballMarkerAnchor: AnchorEntity?
    private var pocketMarkerAnchor: AnchorEntity?
    private var ghostBallAnchor: AnchorEntity?
    private weak var ballOverlayEntity: ModelEntity?

    /// Auto-reset: when in `.done` state and tracking is lost/blocked for this long, reset automatically.
    private var trackingLostSince: Date?
    private let autoResetDelay: TimeInterval = 5.0

    var instructionsText: String {
        switch selectionState {
        case .pickBall:
            "Tap the object ball"
        case .pickPocket:
            "Tap the pocket"
        case .done:
            "Ghost ball placed. Tap screen to re-lock the object ball"
        }
    }

    func attach(arView: ARView) {
        self.arView = arView
        arView.session.delegate = self
    }

    func startSessionIfNeeded() {
        guard let arView, !didStartSession else { return }
        didStartSession = true
        runSession(options: [.resetTracking, .removeExistingAnchors], arView: arView)
    }

    func applyDebugOptions() {
        guard let arView else { return }
        if debugEnabled {
            arView.debugOptions = [
                .showFeaturePoints,
                .showAnchorOrigins,
                .showAnchorGeometry
            ]
        } else {
            arView.debugOptions = []
        }
    }

    func setDebugEnabled(_ enabled: Bool) {
        debugEnabled = enabled
        applyDebugOptions()
        if !enabled {
            (arView as? AimCamARView)?.clearBallDetectionOverlay()
            statusText = nil
        }
    }

    func toggleSession() {
        guard let arView else { return }
        if sessionRunning {
            arView.session.pause()
            setSessionRunning(false)
            shouldResumeAfterBackground = false
        } else {
            runSession(arView: arView)
        }
    }

    func handleScenePhaseChange(_ phase: ScenePhase) {
        switch phase {
        case .active:
            if shouldResumeAfterBackground {
                shouldResumeAfterBackground = false
                if let arView {
                    runSession(arView: arView)
                }
            }
        case .inactive, .background:
            if sessionRunning, let arView {
                shouldResumeAfterBackground = true
                arView.session.pause()
                setSessionRunning(false)
            }
        @unknown default:
            break
        }
    }

    private func makeConfiguration() -> ARWorldTrackingConfiguration {
        let config = ARWorldTrackingConfiguration()
        config.planeDetection = [.horizontal]
        config.environmentTexturing = .automatic

        if ARWorldTrackingConfiguration.supportsSceneReconstruction(.meshWithClassification) {
            config.sceneReconstruction = .meshWithClassification
        } else if ARWorldTrackingConfiguration.supportsSceneReconstruction(.mesh) {
            config.sceneReconstruction = .mesh
        }

        if ARWorldTrackingConfiguration.supportsFrameSemantics(.sceneDepth) {
            config.frameSemantics.insert(.sceneDepth)
        }
        return config
    }

    private func runSession(options: ARSession.RunOptions = [], arView: ARView) {
        let config = makeConfiguration()
        arView.session.run(config, options: options)
        setSessionRunning(true)
    }

    private func setSessionRunning(_ running: Bool) {
        // Avoid publishing during view updates.
        DispatchQueue.main.async { [weak self] in
            self?.sessionRunning = running
        }
    }

    func reset() {
        selectionState = .pickBall
        statusText = nil
        ballCenter = nil
        pocketCenter = nil
        tableSurfaceY = nil
        ballOverlayEntity = nil

        (arView as? AimCamARView)?.clearBallDetectionOverlay()

        if let anchor = ballMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        if let anchor = pocketMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        if let anchor = ghostBallAnchor {
            arView?.scene.removeAnchor(anchor)
        }

        ballMarkerAnchor = nil
        pocketMarkerAnchor = nil
        ghostBallAnchor = nil
    }

    private func autoUpdateBall() {
        guard let arView, let ballCenter else {
            reset()
            return
        }

        // Project the current ball center into screen space.
        let projected = arView.project(ballCenter)
        let pt = projected

        // Check if it's visible on screen.
        guard let pt, arView.bounds.contains(pt) else {
            showTemporaryStatus("Object ball is not on screen")
            return
        }

        let preservePocketXZ: SIMD2<Float>? = {
            guard let pocketCenter, selectionState == .done else { return nil }
            return SIMD2<Float>(pocketCenter.x, pocketCenter.z)
        }()

        // Clear existing ball/pocket/ghost but keep tableSurfaceY.
        selectionState = .pickBall
        self.ballCenter = nil
        self.pocketCenter = nil
        ballOverlayEntity = nil
        (arView as? AimCamARView)?.clearBallDetectionOverlay()

        if let anchor = ballMarkerAnchor { arView.scene.removeAnchor(anchor) }
        if let anchor = pocketMarkerAnchor { arView.scene.removeAnchor(anchor) }
        if let anchor = ghostBallAnchor { arView.scene.removeAnchor(anchor) }
        ballMarkerAnchor = nil
        pocketMarkerAnchor = nil
        ghostBallAnchor = nil

        // Re-detect at the projected screen position.
        Task { @MainActor in
            await self.handleBallSelection(tapPoint: pt,
                                           zoomScale: (arView as? AimCamARView)?.zoomScale ?? 1.0,
                                           arView: arView,
                                           onBallSet: { [weak self] center, radius in
                                               guard let self else { return }
                                               if let pocketXZ = preservePocketXZ {
                                                   self.updatePocketAndGhostKeepingXZ(pocketXZ: pocketXZ, ballCenter: center, radius: radius)
                                               }
                                           })
        }
    }

    private func showTemporaryStatus(_ message: String) {
        statusText = message
        Task { @MainActor in
            try? await Task.sleep(nanoseconds: 3_000_000_000)
            if self.statusText == message {
                self.statusText = nil
            }
        }
    }

    private func updatePocketAndGhostKeepingXZ(pocketXZ: SIMD2<Float>, ballCenter: SIMD3<Float>, radius: Float) {
        let pocket = SIMD3<Float>(pocketXZ.x, ballCenter.y, pocketXZ.y)
        pocketCenter = pocket

        if let anchor = pocketMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        pocketMarkerAnchor = makeMarkerAnchor(kind: .pocket, at: pocket, radius: radius * 0.30)
        if let pocketMarkerAnchor {
            arView?.scene.addAnchor(pocketMarkerAnchor)
        }

        let ghost = computeGhostBallCenter(ballCenter: ballCenter, pocketCenter: pocket, radius: radius)
        if let anchor = ghostBallAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        ghostBallAnchor = makeMarkerAnchor(kind: .ghost, at: ghost, radius: radius)
        if let ghostBallAnchor {
            arView?.scene.addAnchor(ghostBallAnchor)
        }

        selectionState = .done
        statusText = nil
    }

    func reselectPocket() {
        // Keep ball; clear pocket + ghost.
        selectionState = .pickPocket
        statusText = debugEnabled ? "Reselect pocket." : nil
        pocketCenter = nil

        if let anchor = pocketMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        if let anchor = ghostBallAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        pocketMarkerAnchor = nil
        ghostBallAnchor = nil
    }

    func handleTap(point: CGPoint, zoomScale: CGFloat, in arView: ARView) {
        if selectionState == .done {
            autoUpdateBall()
            return
        }

        switch selectionState {
        case .pickBall:
            // Ball selection: try boundary + depth first, then fall back to plane raycast.
            Task { @MainActor in
                await self.handleBallSelection(tapPoint: point, zoomScale: zoomScale, arView: arView, onBallSet: nil)
            }
        case .pickPocket:
            // Prefer intersecting the tap ray with the already-established table plane.
            if let y = tableSurfaceY,
               let p = intersectWorldRayWithHorizontalPlaneY(screenPoint: point, zoomScale: zoomScale, arView: arView, planeY: y) {
                handlePocketSelection(surfacePoint: p)
                return
            }

            // Fallback: raycast to any horizontal surface.
            guard let surfacePoint = raycastToHorizontalSurface(screenPoint: point, zoomScale: zoomScale, arView: arView) else {
                statusText = "No horizontal surface found. Move phone to detect the table."
                return
            }
            handlePocketSelection(surfacePoint: surfacePoint)
        case .done:
            break
        }
    }

    // MARK: - Selection steps
    private enum MarkerKind {
        case ball
        case pocket
        case ghost
    }

    private func setBall(center: SIMD3<Float>, radius: Float, note: String?) {
        ballRadiusMeters = radius
        ballCenter = center

        // Replace marker if needed.
        if let anchor = ballMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        ballOverlayEntity = nil
        ballMarkerAnchor = makeMarkerAnchor(kind: .ball, at: center, radius: radius)
        if let ballMarkerAnchor {
            arView?.scene.addAnchor(ballMarkerAnchor)
        }

        selectionState = .pickPocket

        var suffix = ""
        if debugEnabled, let arView, let frame = arView.session.currentFrame {
            let cameraPos = SIMD3<Float>(frame.camera.transform.columns.3.x,
                                         frame.camera.transform.columns.3.y,
                                         frame.camera.transform.columns.3.z)
            let dist = simd_length(center - cameraPos)
            let rMM = radius * 1000
            suffix = String(format: " (r=%.1fmm, d=%.2fm, anchors=%d)", rMM, dist, arView.scene.anchors.count)
        }
        statusText = (note ?? "Ball locked. Now tap the pocket.") + suffix
    }

    private func handleBallSelection(tapPoint: CGPoint,
                                     zoomScale: CGFloat,
                                     arView: ARView,
                                     onBallSet: ((SIMD3<Float>, Float) -> Void)?) async {
        guard let arView = self.arView else { return }

        if debugEnabled {
            statusText = "Detecting ball boundary…"
        }

        // Use the unzoomed point for mapping into the AR frame display transform.
        let unzoomedPoint = unzoomScreenPoint(tapPoint, zoomScale: zoomScale, bounds: arView.bounds)

        if debugEnabled, let av = arView as? AimCamARView {
            av.setBallDetectionROIOverlay(
                rectInView: CGRect(x: unzoomedPoint.x - 60,
                                   y: unzoomedPoint.y - 60,
                                   width: 120,
                                   height: 120)
            )
        }

        if let frame = arView.session.currentFrame {
            let orientation = arView.window?.windowScene?.interfaceOrientation ?? .portrait
            if let obs2D = ballDetector.observeBall2D(frame: frame,
                                                      tapPointInView: unzoomedPoint,
                                                      viewportSize: arView.bounds.size,
                                                      interfaceOrientation: orientation),
               let tablePoint = raycastToHorizontalSurface(screenPoint: tapPoint, zoomScale: zoomScale, arView: arView),
               let ray = arView.ray(through: obs2D.centerPointInView) {
                // Cache the table surface Y so pocket selection can use the same plane.
                tableSurfaceY = tablePoint.y
                let K = frame.camera.intrinsics
                let fx = K.columns.0.x
                let fy = K.columns.1.y
                let fxAvg = max((fx + fy) * 0.5, 1e-4)

                // Estimate radius from depth if we have it; otherwise use default.
                var radius = ballRadiusMeters
                if let z = obs2D.depthMetersAtCenter, z.isFinite, z > 0.05, z < 8 {
                    // Depth at the circle center pixel is (usually) the *front surface* of the sphere:
                    // z_surface = z_center - r. With projection r_px = f * r / z_center.
                    // Solve: r = (alpha * z_surface) / (1 - alpha) where alpha = r_px / f.
                    let alpha = Float(obs2D.radiusPixels) / fxAvg
                    if alpha > 0.001, alpha < 0.7 {
                        radius = (alpha * z) / (1 - alpha)
                    } else {
                        radius = Float(obs2D.radiusPixels) * z / fxAvg
                    }
                }
                // Sanity: pool ball radius ~ 28.6mm. If our estimate is off, use default rather than clamping to extremes.
                if radius < 0.015 || radius > 0.060 {
                    radius = 0.05715 / 2.0
                }
                // Snap to standard ball sizes when we're already close (helps reduce jitter).
                radius = snapRadiusToStandardIfClose(radiusMeters: radius)

                // Place the ball center on a horizontal plane at (tableY + r), using the *world ray*.
                let planeY = tablePoint.y + radius
                let dir = ray.direction
                let origin = ray.origin
                if abs(dir.y) > 1e-4 {
                    let t = (planeY - origin.y) / dir.y
                    if t.isFinite, t > 0.05, t < 10 {
                        let center = origin + dir * t
                        // Extra sanity: distance should roughly match your working range.
                        let dist = simd_length(center - origin)
                        if dist > 0.15, dist < 5.0 {
                            let displayTransform = frame.displayTransform(for: orientation, viewportSize: arView.bounds.size)
                            let rView = imageRadiusPixelsToViewPixels(
                                radiusPixels: obs2D.radiusPixels,
                                centerImageNormTL: obs2D.centerImageNormalizedTL,
                                displayTransform: displayTransform,
                                viewportSize: arView.bounds.size,
                                imageWidthPixels: CGFloat(frame.camera.imageResolution.width)
                            )

                            // Flash the green tracking circle as confirmation.
                            if let av = arView as? AimCamARView {
                                av.flashTrackingCircle(center: obs2D.centerPointInView, radius: rView)
                            }

                            // Draw debug overlays only when debug is on.
                            if debugEnabled, let av = arView as? AimCamARView {
                                av.setBallDetectionROIOverlay(rectInView: obs2D.roiRectInView)
                                av.setBallDetectionOverlay(
                                    contourPointsInView: obs2D.contourPointsInView,
                                    circleCenter: obs2D.centerPointInView,
                                    circleRadius: rView
                                )
                            }
                            setBall(center: center,
                                    radius: radius,
                                    note: obs2D.usedSceneDepth
                                        ? String(format: "Ball locked (%@, conf=%.2f, depth radius). Now tap the pocket.", obs2D.channelUsed, obs2D.confidence)
                                        : String(format: "Ball locked (%@, conf=%.2f). Now tap the pocket.", obs2D.channelUsed, obs2D.confidence))
                            onBallSet?(center, radius)
                            return
                        }
                    }
                }

                statusText = "Ball boundary found but placement looked off. Falling back…"
                _ = fy // keep fy in scope for future tuning
            }
        }

        // Fallback: plane raycast + assumed radius.
        guard let surfacePoint = raycastToHorizontalSurface(screenPoint: tapPoint, zoomScale: zoomScale, arView: arView) else {
            statusText = "No surface/depth found. Move phone to detect the table."
            return
        }
        tableSurfaceY = surfacePoint.y
        let center = SIMD3<Float>(surfacePoint.x, surfacePoint.y + ballRadiusMeters, surfacePoint.z)
        setBall(center: center, radius: ballRadiusMeters, note: "Ball locked (plane fallback). Now tap the pocket.")
        onBallSet?(center, ballRadiusMeters)
    }

    private func handlePocketSelection(surfacePoint: SIMD3<Float>) {
        guard let ballCenter else {
            selectionState = .pickBall
            statusText = "Ball not set. Tap the target ball first."
            return
        }

        var pocket = surfacePoint
        // Per requirement: pocket center is assumed at same elevation as ball center (perfectly level table).
        pocket.y = ballCenter.y
        pocketCenter = pocket

        if let anchor = pocketMarkerAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        pocketMarkerAnchor = makeMarkerAnchor(kind: .pocket, at: pocket, radius: ballRadiusMeters * 0.30)
        if let pocketMarkerAnchor {
            arView?.scene.addAnchor(pocketMarkerAnchor)
        }

        let ghost = computeGhostBallCenter(ballCenter: ballCenter, pocketCenter: pocket, radius: ballRadiusMeters)
        if let anchor = ghostBallAnchor {
            arView?.scene.removeAnchor(anchor)
        }
        ghostBallAnchor = makeMarkerAnchor(kind: .ghost, at: ghost, radius: ballRadiusMeters)
        if let ghostBallAnchor {
            arView?.scene.addAnchor(ghostBallAnchor)
        }

        selectionState = .done
        if debugEnabled, let arView {
            statusText = "Ghost ball placed (anchors=\(arView.scene.anchors.count)). Move around to view it from different angles."
        } else {
            statusText = nil
        }
    }

    private func computeGhostBallCenter(ballCenter: SIMD3<Float>, pocketCenter: SIMD3<Float>, radius: Float) -> SIMD3<Float> {
        // Keep the computation on the (level) table plane.
        var direction = pocketCenter - ballCenter
        direction.y = 0
        let len = simd_length(direction)
        guard len > 1e-4 else { return ballCenter }
        let dir = direction / len

        // Ghost ball center is 2r behind the target ball along the ball->pocket line.
        return ballCenter - dir * (2 * radius)
    }

    // MARK: - Raycast helpers
    private func raycastToHorizontalSurface(screenPoint: CGPoint, zoomScale: CGFloat, arView: ARView) -> SIMD3<Float>? {
        let unzoomedPoint = unzoomScreenPoint(screenPoint, zoomScale: zoomScale, bounds: arView.bounds)

        let cameraY = arView.session.currentFrame?.camera.transform.columns.3.y

        // Prefer real plane geometry and avoid infinite-plane hits (which can jump to wrong planes).
        let targets: [ARRaycastQuery.Target] = [
            .existingPlaneGeometry,
            .estimatedPlane
        ]

        for target in targets {
            let results = arView.raycast(from: unzoomedPoint, allowing: target, alignment: .horizontal)
            if let cameraY {
                // Prefer hits below the camera (table) to avoid ceiling hits.
                if let hit = results.first(where: { $0.worldTransform.columns.3.y < cameraY - 0.05 }) {
                    let t = hit.worldTransform
                    return SIMD3<Float>(t.columns.3.x, t.columns.3.y, t.columns.3.z)
                }
            }
            if let first = results.first {
                let t = first.worldTransform
                return SIMD3<Float>(t.columns.3.x, t.columns.3.y, t.columns.3.z)
            }
        }
        return nil
    }

    private func unzoomScreenPoint(_ point: CGPoint, zoomScale: CGFloat, bounds: CGRect) -> CGPoint {
        // We scale ARView around its center. Undo that scaling to map taps to the underlying unscaled render.
        guard zoomScale > 1.0001 else { return point }

        let mid = CGPoint(x: bounds.midX, y: bounds.midY)
        let dx = point.x - mid.x
        let dy = point.y - mid.y
        return CGPoint(x: mid.x + dx / zoomScale, y: mid.y + dy / zoomScale)
    }

    private func intersectWorldRayWithHorizontalPlaneY(screenPoint: CGPoint,
                                                       zoomScale: CGFloat,
                                                       arView: ARView,
                                                       planeY: Float) -> SIMD3<Float>? {
        let unzoomedPoint = unzoomScreenPoint(screenPoint, zoomScale: zoomScale, bounds: arView.bounds)
        guard let ray = arView.ray(through: unzoomedPoint) else { return nil }
        let dir = ray.direction
        let origin = ray.origin
        guard abs(dir.y) > 1e-5 else { return nil }
        let t = (planeY - origin.y) / dir.y
        guard t.isFinite, t > 0.05, t < 20 else { return nil }
        return origin + dir * t
    }

    // MARK: - Rendering helpers
    private func makeMarkerAnchor(kind: MarkerKind, at position: SIMD3<Float>, radius: Float) -> AnchorEntity {
        let anchor = AnchorEntity(world: position)

        switch kind {
        case .ball:
            // Revert to a simple translucent overlay ball.
            let model = ModelEntity(
                mesh: .generateSphere(radius: radius),
                materials: [SimpleMaterial(color: UIColor.white.withAlphaComponent(0.28), isMetallic: false)]
            )
            model.name = "ball_overlay"
            model.generateCollisionShapes(recursive: true)
            ballOverlayEntity = model
            anchor.addChild(model)

        case .pocket:
            // Lighter, obvious pocket indicator.
            let model = ModelEntity(
                mesh: .generateSphere(radius: radius),
                materials: [UnlitMaterial(color: UIColor.systemYellow.withAlphaComponent(0.95))]
            )
            model.name = "pocket_marker"
            anchor.addChild(model)

        case .ghost:
            let model = ModelEntity(
                mesh: .generateSphere(radius: radius),
                materials: [SimpleMaterial(color: UIColor.white.withAlphaComponent(0.45), isMetallic: false)]
            )
            model.name = "ghost_ball"
            anchor.addChild(model)

            // Red vertical line through the ghost ball center.
            let lineHeight = max(radius * 8.0, 0.25)
            let lineRadius = max(radius * 0.06, 0.002)
            let line = ModelEntity(
                mesh: .generateBox(size: [lineRadius * 2, lineHeight, lineRadius * 2]),
                materials: [UnlitMaterial(color: UIColor.systemRed.withAlphaComponent(0.95))]
            )
            line.name = "ghost_vertical_line"
            // Offset slightly upward so most of the line is above the table.
            line.position = [0, lineHeight * 0.25, 0]
            anchor.addChild(line)
        }

        if debugEnabled {
            let beacon = ModelEntity(
                mesh: .generateBox(size: max(radius * 0.18, 0.004)),
                materials: [UnlitMaterial(color: UIColor.yellow)]
            )
            beacon.name = "marker_beacon"
            beacon.position = [0, max(radius * 1.4, 0.03), 0]
            anchor.addChild(beacon)
        }
        return anchor
    }

    // MARK: - Ball radius utilities

    private func snapRadiusToStandardIfClose(radiusMeters: Float) -> Float {
        // Standard sizes:
        // - Pool: 57.15mm diameter
        // - Snooker: 52.5mm diameter
        let poolR: Float = 0.05715 / 2.0
        let snookerR: Float = 0.0525 / 2.0
        let tol: Float = 0.00115 // 1.15mm

        let dPool = abs(radiusMeters - poolR)
        let dSnk = abs(radiusMeters - snookerR)

        if dPool <= tol && dPool <= dSnk { return poolR }
        if dSnk <= tol && dSnk < dPool { return snookerR }
        return radiusMeters
    }

    private func imageRadiusPixelsToViewPixels(radiusPixels: CGFloat,
                                               centerImageNormTL: CGPoint,
                                               displayTransform: CGAffineTransform,
                                               viewportSize: CGSize,
                                               imageWidthPixels: CGFloat) -> CGFloat {
        guard radiusPixels.isFinite, radiusPixels > 0, imageWidthPixels > 0 else { return radiusPixels }
        let dx = radiusPixels / imageWidthPixels
        let p1 = centerImageNormTL
        let p2 = CGPoint(x: min(max(p1.x + dx, 0), 1), y: p1.y)

        let v1 = p1.applying(displayTransform)
        let v2 = p2.applying(displayTransform)

        let x1 = v1.x * viewportSize.width
        let y1 = v1.y * viewportSize.height
        let x2 = v2.x * viewportSize.width
        let y2 = v2.y * viewportSize.height

        return hypot(x2 - x1, y2 - y1)
    }
}

extension ARSessionManager: ARSessionDelegate {
    nonisolated func session(_ session: ARSession, cameraDidChangeTrackingState camera: ARCamera) {
        Task { @MainActor [weak self] in
            guard let self else { return }
            switch camera.trackingState {
            case .normal:
                self.trackingLostSince = nil
                if self.statusText?.hasPrefix("Tracking") == true {
                    self.statusText = nil
                }
            case .notAvailable:
                self.statusText = "Tracking not available."
                self.startAutoResetTimerIfNeeded()
            case .limited(let reason):
                self.startAutoResetTimerIfNeeded()
                switch reason {
                case .initializing:
                    self.statusText = "Tracking limited: initializing…"
                case .excessiveMotion:
                    self.statusText = "Tracking limited: move slower."
                case .insufficientFeatures:
                    self.statusText = "Tracking limited: point at the table surface."
                case .relocalizing:
                    self.statusText = "Tracking limited: relocalizing…"
                @unknown default:
                    self.statusText = "Tracking limited."
                }
            }
        }
    }

    nonisolated func session(_ session: ARSession, didUpdate frame: ARFrame) {
        let res = CGSize(width: CGFloat(frame.camera.imageResolution.width),
                         height: CGFloat(frame.camera.imageResolution.height))
        let tracking = frame.camera.trackingState
        Task { @MainActor [weak self] in
            guard let self else { return }
            if self.cameraImageResolution != res {
                self.cameraImageResolution = res
            }
            // Check auto-reset on every frame update.
            self.checkAutoReset(trackingState: tracking)

            // Update the always-on ball tracking circle.
            guard let arView = self.arView as? AimCamARView else { return }
            guard let ballCenter = self.ballCenter else {
                arView.setTrackingCircle(center: nil, radius: nil)
                return
            }

            guard let centerPt = arView.project(ballCenter),
                  arView.bounds.contains(centerPt) else {
                arView.setTrackingCircle(center: nil, radius: nil)
                return
            }

            // Compute screen radius using camera right/up directions for better match.
            if let frame = arView.session.currentFrame {
                let camT = frame.camera.transform
                let right = SIMD3<Float>(camT.columns.0.x, camT.columns.0.y, camT.columns.0.z)
                let up = SIMD3<Float>(camT.columns.1.x, camT.columns.1.y, camT.columns.1.z)
                let edgeRight = ballCenter + right * self.ballRadiusMeters
                let edgeUp = ballCenter + up * self.ballRadiusMeters
                if let rightPt = arView.project(edgeRight),
                   let upPt = arView.project(edgeUp) {
                    let r1 = hypot(rightPt.x - centerPt.x, rightPt.y - centerPt.y)
                    let r2 = hypot(upPt.x - centerPt.x, upPt.y - centerPt.y)
                    let radiusPx = max(r1, r2)
                    arView.setTrackingCircle(center: centerPt, radius: radiusPx)
                    return
                }
            }

            // Fallback: use world X axis projection.
            let edgeWorld = ballCenter + SIMD3<Float>(self.ballRadiusMeters, 0, 0)
            let edgePt = arView.project(edgeWorld)
            if let edgePt {
                let radiusPx = hypot(edgePt.x - centerPt.x, edgePt.y - centerPt.y)
                arView.setTrackingCircle(center: centerPt, radius: radiusPx)
            } else {
                arView.setTrackingCircle(center: centerPt, radius: nil)
            }
        }
    }

    @MainActor
    private func startAutoResetTimerIfNeeded() {
        guard selectionState == .done else {
            trackingLostSince = nil
            return
        }
        if trackingLostSince == nil {
            trackingLostSince = Date()
        }
    }

    @MainActor
    private func checkAutoReset(trackingState: ARCamera.TrackingState) {
        guard selectionState == .done else {
            trackingLostSince = nil
            return
        }

        switch trackingState {
        case .normal:
            trackingLostSince = nil
        case .notAvailable, .limited:
            if trackingLostSince == nil {
                trackingLostSince = Date()
            }
            if let since = trackingLostSince, Date().timeIntervalSince(since) >= autoResetDelay {
                reset()
            }
        }
    }
}

