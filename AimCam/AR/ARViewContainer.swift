import SwiftUI
import ARKit
import RealityKit

struct ARViewContainer: UIViewRepresentable {
    @ObservedObject var sessionManager: ARSessionManager

    func makeUIView(context: Context) -> UIView {
        let container = UIView(frame: .zero)
        container.backgroundColor = .black

        let arView = AimCamARView(frame: .zero)
        arView.translatesAutoresizingMaskIntoConstraints = false

        container.addSubview(arView)
        NSLayoutConstraint.activate([
            arView.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            arView.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            arView.topAnchor.constraint(equalTo: container.topAnchor),
            arView.bottomAnchor.constraint(equalTo: container.bottomAnchor),
        ])

        // Keep coaching overlay unzoomed for UX.
        let coaching = ARCoachingOverlayView()
        coaching.session = arView.session
        coaching.goal = .horizontalPlane
        coaching.activatesAutomatically = true
        coaching.translatesAutoresizingMaskIntoConstraints = false
        coaching.isUserInteractionEnabled = false

        container.addSubview(coaching)
        NSLayoutConstraint.activate([
            coaching.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            coaching.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            coaching.topAnchor.constraint(equalTo: container.topAnchor),
            coaching.bottomAnchor.constraint(equalTo: container.bottomAnchor),
        ])

        arView.onTap = { [weak sessionManager] point, zoom in
            guard let sessionManager else { return }
            sessionManager.handleTap(point: point, zoomScale: zoom, in: arView)
        }
        arView.onZoomChanged = { [weak sessionManager] zoom in
            sessionManager?.zoomScale = zoom
        }

        sessionManager.attach(arView: arView)
        sessionManager.startSessionIfNeeded()
        sessionManager.applyDebugOptions()

        return container
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        // No-op. Session/state is driven by ARSessionManager.
    }
}

