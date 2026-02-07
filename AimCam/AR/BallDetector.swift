import Foundation
import UIKit
import ARKit
import Accelerate
import simd

struct BallObservation2D {
    let centerImageNormalizedTL: CGPoint   // normalized in camera image space, origin top-left
    let centerPointInView: CGPoint         // point in ARView coordinates (pixels)
    let radiusPixels: CGFloat
    let contourPointsInView: [CGPoint]     // detected contour in ARView coordinates (pixels)
    let roiRectInView: CGRect              // the ROI square in view coordinates
    let depthMetersAtCenter: Float?        // scene depth (Z) at center, if available
    let usedSceneDepth: Bool
    let channelUsed: String
    let confidence: Float                 // 0..1-ish (heuristic)
}

/// Detects a ball boundary in the camera image (ROI around a tap), and combines it with ARKit depth
/// (when available) to estimate the 3D center and size.
final class BallDetector {
    struct Options {
        /// Desired ROI size in *view points* (UIKit points), centered on tap.
        /// Approximation for "2cm x 2cm on screen"; tuned smaller than before.
        var roiSizeViewPoints: CGFloat = 120
        // Edge/circle detection tuning
        var ransacIterations: Int = 260
        var inlierThresholdPixels: Float = 2.5
        var maxEdgePoints: Int = 4500
    }

    private let options: Options

    init(options: Options = Options()) {
        self.options = options
    }

    /// Returns the observed circle (2D) and optional depth at its center.
    func observeBall2D(frame: ARFrame, tapPointInView: CGPoint, viewportSize: CGSize, interfaceOrientation: UIInterfaceOrientation) -> BallObservation2D? {
        let imageResolution = CGSize(width: CGFloat(frame.camera.imageResolution.width),
                                     height: CGFloat(frame.camera.imageResolution.height))

        // Map view point -> normalized image point (origin top-left) using ARFrame display transform.
        let viewNorm = CGPoint(x: tapPointInView.x / viewportSize.width,
                               y: tapPointInView.y / viewportSize.height)

        let displayTransform = frame.displayTransform(for: interfaceOrientation, viewportSize: viewportSize)
        let invDisplay = displayTransform.inverted()
        let imageNormTL = viewNorm.applying(invDisplay)

        let roiRectInView = CGRect(x: tapPointInView.x - options.roiSizeViewPoints / 2,
                                   y: tapPointInView.y - options.roiSizeViewPoints / 2,
                                   width: options.roiSizeViewPoints,
                                   height: options.roiSizeViewPoints)

        let roiTLNorm = makeImageTLROIFromViewTap(tapPointInView: tapPointInView,
                                                  viewportSize: viewportSize,
                                                  invDisplayTransform: invDisplay,
                                                  roiSizeViewPoints: options.roiSizeViewPoints)

        // Depth at the tap helps narrow expected radius in pixels.
        let depthAtTap = sampleSceneDepth(frame: frame, imagePointNormTL: imageNormTL)?.0

        guard let circle = detectCircleEdgesRansac(frame: frame,
                                                   roiTLNorm: roiTLNorm,
                                                   expectedDepthMeters: depthAtTap) else {
            return nil
        }

        let centerImageNormTL = circle.centerImageNormTL
        let inliersInView: [CGPoint] = circle.inlierPointsImageNormTL.map { p in
            let viewNorm = p.applying(displayTransform)
            return CGPoint(x: viewNorm.x * viewportSize.width, y: viewNorm.y * viewportSize.height)
        }

        let centerViewNorm = centerImageNormTL.applying(displayTransform)
        let centerPointInView = CGPoint(x: centerViewNorm.x * viewportSize.width, y: centerViewNorm.y * viewportSize.height)

        let depthInfo = sampleSceneDepth(frame: frame, imagePointNormTL: centerImageNormTL)
        let usedSceneDepth = (depthInfo != nil)
        let depthMeters = depthInfo?.0

        return BallObservation2D(centerImageNormalizedTL: centerImageNormTL,
                                 centerPointInView: centerPointInView,
                                 radiusPixels: circle.radiusPixels,
                                 contourPointsInView: inliersInView,
                                 roiRectInView: roiRectInView,
                                 depthMetersAtCenter: depthMeters,
                                 usedSceneDepth: usedSceneDepth,
                                 channelUsed: circle.channelUsed,
                                 confidence: circle.confidence)
    }

    // MARK: - Circle detection (edges + RANSAC)

    private struct CircleRansacResult {
        let centerImageNormTL: CGPoint
        let radiusPixels: CGFloat
        let inlierPointsImageNormTL: [CGPoint]
        let channelUsed: String
        let confidence: Float
    }

    private func detectCircleEdgesRansac(frame: ARFrame, roiTLNorm: CGRect, expectedDepthMeters: Float?) -> CircleRansacResult? {
        let pixelBuffer = frame.capturedImage
        CVPixelBufferLockBaseAddress(pixelBuffer, .readOnly)
        defer { CVPixelBufferUnlockBaseAddress(pixelBuffer, .readOnly) }

        // Planes: 0 = Y (full res), 1 = CbCr (half res, interleaved).
        let yPlane = 0
        guard let yBase = CVPixelBufferGetBaseAddressOfPlane(pixelBuffer, yPlane) else { return nil }
        let yBytes = CVPixelBufferGetBytesPerRowOfPlane(pixelBuffer, yPlane)
        let yW = CVPixelBufferGetWidthOfPlane(pixelBuffer, yPlane)
        let yH = CVPixelBufferGetHeightOfPlane(pixelBuffer, yPlane)

        // Convert ROI (image TL normalized) -> pixel rect in Y plane.
        let x0 = max(0, min(yW - 2, Int(round(roiTLNorm.origin.x * CGFloat(yW)))))
        let y0 = max(0, min(yH - 2, Int(round(roiTLNorm.origin.y * CGFloat(yH)))))
        let w = max(6, min(yW - x0, Int(round(roiTLNorm.size.width * CGFloat(yW)))))
        let h = max(6, min(yH - y0, Int(round(roiTLNorm.size.height * CGFloat(yH)))))

        let rRange = estimateRadiusRangePixels(frame: frame, zSurface: expectedDepthMeters, roiW: w, roiH: h)
        let rMin = rRange.minR
        let rMax = rRange.maxR

        let roiCenter = SIMD2<Float>(Float(w) * 0.5, Float(h) * 0.5)
        let expectedRadiusPx = expectedRadiusPixels(frame: frame, zSurface: expectedDepthMeters)

        var candidates: [ChannelCandidate] = []
        candidates.reserveCapacity(3)

        // Y channel
        if let cand = detectOnChannel(name: "Y",
                                      points: edgePointsLuma(yBase: yBase, x0: x0, y0: y0, w: w, h: h, bytesPerRow: yBytes),
                                      roiCenter: roiCenter,
                                      rMin: rMin, rMax: rMax,
                                      expectedRadiusPx: expectedRadiusPx) {
            candidates.append(cand)
        }

        // Chroma channels (Cb and Cr) — half res. Scale points by 2 into Y coordinates.
        let uvPlane = 1
        if CVPixelBufferGetPlaneCount(pixelBuffer) > 1,
           let uvBase = CVPixelBufferGetBaseAddressOfPlane(pixelBuffer, uvPlane) {
            let uvBytes = CVPixelBufferGetBytesPerRowOfPlane(pixelBuffer, uvPlane)
            let uvW = CVPixelBufferGetWidthOfPlane(pixelBuffer, uvPlane)
            let uvH = CVPixelBufferGetHeightOfPlane(pixelBuffer, uvPlane)

            // Map ROI to UV plane.
            let x0u = max(0, min(uvW - 2, x0 / 2))
            let y0u = max(0, min(uvH - 2, y0 / 2))
            let wu = max(4, min(uvW - x0u, w / 2))
            let hu = max(4, min(uvH - y0u, h / 2))

            if let cand = detectOnChannel(name: "Cb",
                                          points: edgePointsChroma(uvBase: uvBase, x0u: x0u, y0u: y0u, wu: wu, hu: hu, bytesPerRow: uvBytes, componentOffset: 0),
                                          roiCenter: roiCenter,
                                          rMin: rMin, rMax: rMax,
                                          expectedRadiusPx: expectedRadiusPx,
                                          scale: 2) {
                candidates.append(cand)
            }
            if let cand = detectOnChannel(name: "Cr",
                                          points: edgePointsChroma(uvBase: uvBase, x0u: x0u, y0u: y0u, wu: wu, hu: hu, bytesPerRow: uvBytes, componentOffset: 1),
                                          roiCenter: roiCenter,
                                          rMin: rMin, rMax: rMax,
                                          expectedRadiusPx: expectedRadiusPx,
                                          scale: 2) {
                candidates.append(cand)
            }
        }

        guard let best = candidates.max(by: { $0.score < $1.score }) else { return nil }

        // Convert center from ROI pixels to full image normalized TL.
        let cxFull = Float(x0) + best.center.x
        let cyFull = Float(y0) + best.center.y
        let centerNormTL = CGPoint(x: CGFloat(cxFull) / CGFloat(yW),
                                   y: CGFloat(cyFull) / CGFloat(yH))

        let inliersNormTL: [CGPoint] = best.inliers.map { p in
            CGPoint(x: CGFloat(Float(x0) + p.x) / CGFloat(yW),
                    y: CGFloat(Float(y0) + p.y) / CGFloat(yH))
        }

        return CircleRansacResult(centerImageNormTL: centerNormTL,
                                  radiusPixels: CGFloat(best.radius),
                                  inlierPointsImageNormTL: inliersNormTL,
                                  channelUsed: best.name,
                                  confidence: best.confidence)
    }

    private struct ChannelCandidate {
        let name: String
        let center: SIMD2<Float>
        let radius: Float
        let inliers: [SIMD2<Float>]
        let score: Float
        let confidence: Float
    }

    private func detectOnChannel(name: String,
                                 points: [SIMD2<Float>],
                                 roiCenter: SIMD2<Float>,
                                 rMin: Float,
                                 rMax: Float,
                                 expectedRadiusPx: Float?,
                                 scale: Float = 1) -> ChannelCandidate? {
        var pts = points.map { $0 * scale }
        if pts.count < 80 { return nil }
        if pts.count > options.maxEdgePoints {
            pts.shuffle()
            pts = Array(pts.prefix(options.maxEdgePoints))
        }

        guard let fit = ransacCircle(points: pts,
                                     iterations: options.ransacIterations,
                                     rMin: rMin, rMax: rMax,
                                     inlierThreshold: options.inlierThresholdPixels) else {
            return nil
        }

        let inlierFrac = Float(fit.inliers.count) / Float(max(pts.count, 1))
        let centerDist = simd_length(fit.center - roiCenter) / max(1, min(roiCenter.x, roiCenter.y))

        var radiusPenalty: Float = 0
        if let exp = expectedRadiusPx, exp.isFinite, exp > 5 {
            radiusPenalty = min(abs(fit.radius - exp) / exp, 1.0)
        }

        // Score weights tuned empirically: prioritize inlier fraction, then radius prior, then center proximity.
        let score = (2.2 * inlierFrac) - (0.9 * radiusPenalty) - (0.35 * centerDist)
        let confidence = max(0, min(1, score)) // heuristic clamp

        return ChannelCandidate(name: name,
                                center: fit.center,
                                radius: fit.radius,
                                inliers: fit.inliers,
                                score: score,
                                confidence: confidence)
    }

    private func expectedRadiusPixels(frame: ARFrame, zSurface: Float?) -> Float? {
        guard let z = zSurface, z.isFinite, z > 0.1, z < 5 else { return nil }
        let rPhys: Float = 0.05715 / 2.0
        let K = frame.camera.intrinsics
        let f = max((K.columns.0.x + K.columns.1.y) * 0.5, 1e-4)
        let zc = z + rPhys
        return f * rPhys / zc
    }

    private func edgePointsLuma(yBase: UnsafeMutableRawPointer, x0: Int, y0: Int, w: Int, h: Int, bytesPerRow: Int) -> [SIMD2<Float>] {
        let roiPtr = yBase.advanced(by: y0 * bytesPerRow + x0)
        return sobelEdges(roiPtr: roiPtr, roiW: w, roiH: h, rowBytes: bytesPerRow).points
    }

    private func edgePointsChroma(uvBase: UnsafeMutableRawPointer,
                                  x0u: Int, y0u: Int,
                                  wu: Int, hu: Int,
                                  bytesPerRow: Int,
                                  componentOffset: Int) -> [SIMD2<Float>] {
        // uv plane is interleaved (Cb,Cr) bytes. We run sobel on selected component.
        let ptr = uvBase.advanced(by: y0u * bytesPerRow + x0u * 2)
        return sobelEdgesChroma(uvPtr: ptr, roiW: wu, roiH: hu, rowBytes: bytesPerRow, componentOffset: componentOffset)
    }

    private func sobelEdgesChroma(uvPtr: UnsafeMutableRawPointer, roiW: Int, roiH: Int, rowBytes: Int, componentOffset: Int) -> [SIMD2<Float>] {
        let src = uvPtr.assumingMemoryBound(to: UInt8.self)
        // Compute magnitudes and threshold similarly to luma.
        var mags = Array(repeating: UInt16(0), count: roiW * roiH)
        mags.withUnsafeMutableBufferPointer { magsBuf in
            let magPtr = magsBuf.baseAddress!
            for y in 1..<(roiH - 1) {
                let rowM = y * roiW
                let row0 = (y - 1) * rowBytes
                let row1 = y * rowBytes
                let row2 = (y + 1) * rowBytes
                for x in 1..<(roiW - 1) {
                    let idx = (x * 2) + componentOffset
                    let p00 = Int(src[row0 + idx - 2]), p01 = Int(src[row0 + idx]), p02 = Int(src[row0 + idx + 2])
                    let p10 = Int(src[row1 + idx - 2]), /*p11*/ _ = Int(src[row1 + idx]), p12 = Int(src[row1 + idx + 2])
                    let p20 = Int(src[row2 + idx - 2]), p21 = Int(src[row2 + idx]), p22 = Int(src[row2 + idx + 2])

                    let gx = (p02 + 2 * p12 + p22) - (p00 + 2 * p10 + p20)
                    let gy = (p20 + 2 * p21 + p22) - (p00 + 2 * p01 + p02)
                    let m = min(65535, abs(gx) + abs(gy))
                    magPtr[rowM + x] = UInt16(m)
                }
            }
        }
        let sorted = mags.sorted()
        let idx = Int(Double(sorted.count) * 0.93)
        let thr = sorted[min(max(idx, 0), sorted.count - 1)]

        var pts: [SIMD2<Float>] = []
        pts.reserveCapacity(1800)
        for y in 1..<(roiH - 1) {
            let row = y * roiW
            for x in 1..<(roiW - 1) {
                if mags[row + x] >= thr {
                    pts.append(SIMD2<Float>(Float(x), Float(y)))
                }
            }
        }
        return pts
    }

    private func estimateRadiusRangePixels(frame: ARFrame, zSurface: Float?, roiW: Int, roiH: Int) -> (minR: Float, maxR: Float) {
        let K = frame.camera.intrinsics
        let fx = K.columns.0.x
        let fy = K.columns.1.y
        let f = max((fx + fy) * 0.5, 1e-4)
        let rPhys: Float = 0.05715 / 2.0

        if let z = zSurface, z.isFinite, z > 0.1, z < 5 {
            // z_center ≈ z_surface + r
            let zc = z + rPhys
            let rPx = f * rPhys / zc
            let minR = max(10, rPx * 0.55)
            let maxR = min(Float(min(roiW, roiH)) * 0.48, rPx * 1.55)
            return (minR, maxR)
        } else {
            let maxPossible = Float(min(roiW, roiH)) * 0.48
            return (max(10, maxPossible * 0.15), maxPossible)
        }
    }

    private func sobelEdges(roiPtr: UnsafeMutableRawPointer, roiW: Int, roiH: Int, rowBytes: Int) -> (points: [SIMD2<Float>], magnitudes: [UInt16]) {
        // We'll compute magnitude on interior pixels only for simplicity.
        var mags = Array(repeating: UInt16(0), count: roiW * roiH)
        mags.withUnsafeMutableBufferPointer { magsBuf in
            let magPtr = magsBuf.baseAddress!
            // Luma bytes
            let src = roiPtr.assumingMemoryBound(to: UInt8.self)

            for y in 1..<(roiH - 1) {
                let rowM = y * roiW
                let row0 = (y - 1) * rowBytes
                let row1 = y * rowBytes
                let row2 = (y + 1) * rowBytes
                for x in 1..<(roiW - 1) {
                    // Sobel X/Y
                    let p00 = Int(src[row0 + x - 1]), p01 = Int(src[row0 + x]), p02 = Int(src[row0 + x + 1])
                    let p10 = Int(src[row1 + x - 1]), /*p11*/ _ = Int(src[row1 + x]), p12 = Int(src[row1 + x + 1])
                    let p20 = Int(src[row2 + x - 1]), p21 = Int(src[row2 + x]), p22 = Int(src[row2 + x + 1])

                    let gx = (p02 + 2 * p12 + p22) - (p00 + 2 * p10 + p20)
                    let gy = (p20 + 2 * p21 + p22) - (p00 + 2 * p01 + p02)
                    let m = min(65535, abs(gx) + abs(gy))
                    magPtr[rowM + x] = UInt16(m)
                }
            }
        }

        // Threshold by percentile (keep top edges).
        let sorted = mags.sorted()
        let idx = Int(Double(sorted.count) * 0.93)
        let thr = sorted[min(max(idx, 0), sorted.count - 1)]

        var pts: [SIMD2<Float>] = []
        pts.reserveCapacity(2500)
        for y in 1..<(roiH - 1) {
            let row = y * roiW
            for x in 1..<(roiW - 1) {
                if mags[row + x] >= thr {
                    pts.append(SIMD2<Float>(Float(x), Float(y)))
                }
            }
        }
        return (pts, mags)
    }

    private func ransacCircle(points: [SIMD2<Float>],
                              iterations: Int,
                              rMin: Float,
                              rMax: Float,
                              inlierThreshold: Float) -> (center: SIMD2<Float>, radius: Float, inliers: [SIMD2<Float>])? {
        guard points.count >= 20 else { return nil }
        var rng = SystemRandomNumberGenerator()

        var bestInliers: [SIMD2<Float>] = []
        var bestCenter = SIMD2<Float>(0, 0)
        var bestR: Float = 0

        let n = points.count
        for _ in 0..<iterations {
            let i1 = Int.random(in: 0..<n, using: &rng)
            var i2 = Int.random(in: 0..<n, using: &rng)
            var i3 = Int.random(in: 0..<n, using: &rng)
            if i2 == i1 { i2 = (i2 + 7) % n }
            if i3 == i1 || i3 == i2 { i3 = (i3 + 13) % n }

            let p1 = points[i1], p2 = points[i2], p3 = points[i3]
            guard let circle = circumcircle(p1, p2, p3) else { continue }
            if circle.r < rMin || circle.r > rMax { continue }

            var inliers: [SIMD2<Float>] = []
            inliers.reserveCapacity(bestInliers.count + 64)
            let r = circle.r
            let c = circle.c
            for p in points {
                let d = simd_length(p - c)
                if abs(d - r) <= inlierThreshold {
                    inliers.append(p)
                }
            }
            if inliers.count > bestInliers.count {
                bestInliers = inliers
                bestCenter = c
                bestR = r
            }
        }

        guard bestInliers.count >= 80 else { return nil }

        // Refine using Kåsa fit on inliers in pixel space.
        if let refined = fitCircleKasaPixels(points: bestInliers) {
            let r = refined.r
            if r >= rMin && r <= rMax {
                return (refined.c, r, bestInliers)
            }
        }

        return (bestCenter, bestR, bestInliers)
    }

    private func circumcircle(_ p1: SIMD2<Float>, _ p2: SIMD2<Float>, _ p3: SIMD2<Float>) -> (c: SIMD2<Float>, r: Float)? {
        let ax = p1.x, ay = p1.y
        let bx = p2.x, by = p2.y
        let cx = p3.x, cy = p3.y

        let d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
        if abs(d) < 1e-3 { return nil }

        let ax2ay2 = ax * ax + ay * ay
        let bx2by2 = bx * bx + by * by
        let cx2cy2 = cx * cx + cy * cy

        let ux = (ax2ay2 * (by - cy) + bx2by2 * (cy - ay) + cx2cy2 * (ay - by)) / d
        let uy = (ax2ay2 * (cx - bx) + bx2by2 * (ax - cx) + cx2cy2 * (bx - ax)) / d

        let c = SIMD2<Float>(ux, uy)
        let r = simd_length(p1 - c)
        if !r.isFinite { return nil }
        return (c, r)
    }

    private func fitCircleKasaPixels(points: [SIMD2<Float>]) -> (c: SIMD2<Float>, r: Float)? {
        guard points.count >= 20 else { return nil }

        // Solve x^2 + y^2 + a x + b y + c = 0
        var sumX: Double = 0, sumY: Double = 0, sumX2: Double = 0, sumY2: Double = 0, sumXY: Double = 0
        var sumZ: Double = 0, sumZX: Double = 0, sumZY: Double = 0
        let n = Double(points.count)

        for p in points {
            let x = Double(p.x)
            let y = Double(p.y)
            let z = x * x + y * y
            sumX += x; sumY += y
            sumX2 += x * x; sumY2 += y * y
            sumXY += x * y
            sumZ += z
            sumZX += z * x
            sumZY += z * y
        }

        let m00 = sumX2, m01 = sumXY, m02 = sumX
        let m10 = sumXY, m11 = sumY2, m12 = sumY
        let m20 = sumX,  m21 = sumY,  m22 = n
        let b0 = -sumZX, b1 = -sumZY, b2 = -sumZ

        guard let sol = solve3x3D(m00, m01, m02,
                                  m10, m11, m12,
                                  m20, m21, m22,
                                  b0, b1, b2) else { return nil }

        let a = sol.0, b = sol.1, c = sol.2
        let center = SIMD2<Float>(Float(-a / 2.0), Float(-b / 2.0))
        let r2 = max(0.0, (a * a + b * b) / 4.0 - c)
        let r = Float(sqrt(r2))
        return (center, r)
    }

    private func solve3x3D(_ a00: Double, _ a01: Double, _ a02: Double,
                           _ a10: Double, _ a11: Double, _ a12: Double,
                           _ a20: Double, _ a21: Double, _ a22: Double,
                           _ b0: Double, _ b1: Double, _ b2: Double) -> (Double, Double, Double)? {
        let det =
        a00 * (a11 * a22 - a12 * a21) -
        a01 * (a10 * a22 - a12 * a20) +
        a02 * (a10 * a21 - a11 * a20)
        if abs(det) < 1e-12 { return nil }
        let invDet = 1.0 / det

        let d0 =
        b0 * (a11 * a22 - a12 * a21) -
        a01 * (b1 * a22 - a12 * b2) +
        a02 * (b1 * a21 - a11 * b2)

        let d1 =
        a00 * (b1 * a22 - a12 * b2) -
        b0 * (a10 * a22 - a12 * a20) +
        a02 * (a10 * b2 - b1 * a20)

        let d2 =
        a00 * (a11 * b2 - b1 * a21) -
        a01 * (a10 * b2 - b1 * a20) +
        b0 * (a10 * a21 - a11 * a20)

        return (d0 * invDet, d1 * invDet, d2 * invDet)
    }

    // MARK: - Depth sampling

    private func sampleSceneDepth(frame: ARFrame, imagePointNormTL: CGPoint) -> (Float, CGSize)? {
        guard let sceneDepth = frame.sceneDepth else { return nil }
        let depthMap = sceneDepth.depthMap
        let w = CVPixelBufferGetWidth(depthMap)
        let h = CVPixelBufferGetHeight(depthMap)

        // imagePointNormTL is in camera image normalized space. Map to depth map pixels.
        // This is an approximation; ARKit aligns depth to the camera image.
        let px = clamp(Int(round(imagePointNormTL.x * CGFloat(w))), low: 0, high: w - 1)
        let py = clamp(Int(round(imagePointNormTL.y * CGFloat(h))), low: 0, high: h - 1)

        CVPixelBufferLockBaseAddress(depthMap, .readOnly)
        defer { CVPixelBufferUnlockBaseAddress(depthMap, .readOnly) }

        guard let base = CVPixelBufferGetBaseAddress(depthMap) else { return nil }
        let bytesPerRow = CVPixelBufferGetBytesPerRow(depthMap)

        // Sample a small neighborhood and take the median to reduce speckle/noise.
        var samples: [Float] = []
        samples.reserveCapacity(25)

        for dy in -2...2 {
            let yIdx = clamp(py + dy, low: 0, high: h - 1)
            let row = base.advanced(by: yIdx * bytesPerRow)
            let rowF = row.assumingMemoryBound(to: Float32.self)
            for dx in -2...2 {
                let xIdx = clamp(px + dx, low: 0, high: w - 1)
                let v = Float(rowF[xIdx])
                if v.isFinite, v > 0.05, v < 10 {
                    samples.append(v)
                }
            }
        }

        guard !samples.isEmpty else { return nil }
        samples.sort()
        let median = samples[samples.count / 2]
        return (median, CGSize(width: w, height: h))
    }

    private func clamp(_ v: Int, low: Int, high: Int) -> Int { min(max(v, low), high) }

    // MARK: - ROI + orientation helpers

    private func makeImageTLROIFromViewTap(tapPointInView: CGPoint,
                                           viewportSize: CGSize,
                                           invDisplayTransform: CGAffineTransform,
                                           roiSizeViewPoints: CGFloat) -> CGRect {
        let half = roiSizeViewPoints / 2.0
        let tlView = CGPoint(x: tapPointInView.x - half, y: tapPointInView.y - half)
        let brView = CGPoint(x: tapPointInView.x + half, y: tapPointInView.y + half)

        func viewToImageTL(_ pView: CGPoint) -> CGPoint {
            let vNorm = CGPoint(x: pView.x / viewportSize.width, y: pView.y / viewportSize.height)
            return vNorm.applying(invDisplayTransform) // image normalized origin top-left
        }

        let a = viewToImageTL(tlView)
        let b = viewToImageTL(brView)

        let minX = min(a.x, b.x)
        let maxX = max(a.x, b.x)
        let minY = min(a.y, b.y)
        let maxY = max(a.y, b.y)

        var roi = CGRect(x: minX, y: minY, width: maxX - minX, height: maxY - minY)

        // Clamp to [0,1] bounds.
        roi.origin.x = max(0, min(roi.origin.x, 1))
        roi.origin.y = max(0, min(roi.origin.y, 1))
        roi.size.width = max(0.001, min(roi.size.width, 1 - roi.origin.x))
        roi.size.height = max(0.001, min(roi.size.height, 1 - roi.origin.y))
        return roi
    }

    private func cgImageOrientation(from interfaceOrientation: UIInterfaceOrientation) -> CGImagePropertyOrientation {
        // ARFrame displayTransform uses UIInterfaceOrientation; Vision needs CGImagePropertyOrientation.
        // These mappings are typical for camera buffers.
        switch interfaceOrientation {
        case .portrait:
            return .right
        case .portraitUpsideDown:
            return .left
        case .landscapeLeft:
            return .up
        case .landscapeRight:
            return .down
        default:
            return .right
        }
    }
}

