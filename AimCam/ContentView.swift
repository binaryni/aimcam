import SwiftUI

struct ContentView: View {
    @StateObject private var sessionManager = ARSessionManager()
    @State private var showingPrivacyPolicy = false
    @Environment(\.scenePhase) private var scenePhase

    var body: some View {
        ZStack {
            ARViewContainer(sessionManager: sessionManager)
                .ignoresSafeArea()

            VStack {
                // Top bar: ball / pocket / reset (left-aligned)
                topBarContent
                    .padding(.horizontal, 10)
                    .padding(.top, 4)

                Spacer()

                // Instructions at bottom center
                bottomInstructions
                    .padding(.bottom, 60)

                // Utility buttons (debug + pause) at bottom right
                HStack {
                    Spacer()
                    utilityButtons
                        .padding(.trailing, 14)
                        .padding(.bottom, 16)
                }
            }
        }
        .sheet(isPresented: $showingPrivacyPolicy) {
            PrivacyPolicyView()
        }
        .onChange(of: scenePhase) { _, newPhase in
            // Defer to avoid publishing during view updates.
            DispatchQueue.main.async {
                sessionManager.handleScenePhaseChange(newPhase)
            }
        }
    }

    // MARK: - Top bar (ball, pocket, reset)
    private var topBarContent: some View {
        HStack(spacing: 12) {
            iconButton(systemName: "arrow.triangle.2.circlepath", accessibilityLabel: "Update ball") {
                sessionManager.autoUpdateBall()
            }

            iconButton(systemName: "scope", accessibilityLabel: "Reselect pocket") {
                sessionManager.reselectPocket()
            }
            .disabled(sessionManager.selectionState == .pickBall)
            .opacity(sessionManager.selectionState == .pickBall ? 0.35 : 1.0)

            iconButton(systemName: "arrow.counterclockwise", accessibilityLabel: "Reset") {
                sessionManager.reset()
            }

            Spacer(minLength: 0)

            iconButton(systemName: "doc.text", accessibilityLabel: "Privacy policy") {
                showingPrivacyPolicy = true
            }
        }
        .frame(height: 50)
        .background(Color.black.opacity(0.45))
        .clipShape(RoundedRectangle(cornerRadius: 14))
    }

    // MARK: - Bottom-right utility buttons (debug + pause)
    private var utilityButtons: some View {
        VStack(spacing: 10) {
            iconButton(
                systemName: sessionManager.sessionRunning ? "pause.fill" : "play.fill",
                accessibilityLabel: sessionManager.sessionRunning ? "Pause" : "Resume",
                isActive: !sessionManager.sessionRunning
            ) {
                sessionManager.toggleSession()
            }

            iconButton(
                systemName: "ladybug",
                accessibilityLabel: sessionManager.debugEnabled ? "Debug on" : "Debug off",
                isActive: sessionManager.debugEnabled
            ) {
                sessionManager.setDebugEnabled(!sessionManager.debugEnabled)
            }
        }
    }

    // MARK: - Instructions
    private var bottomInstructions: some View {
        let text = sessionManager.statusText ?? sessionManager.instructionsText
        return Text(text)
            .font(.subheadline.weight(.semibold))
            .foregroundStyle(.white)
            .padding(.horizontal, 14)
            .padding(.vertical, 10)
            .background(.black.opacity(0.55))
            .clipShape(RoundedRectangle(cornerRadius: 14))
            .padding(.horizontal, 14)
            .frame(maxWidth: .infinity, alignment: .center)
    }

    // MARK: - Icon button (20% bigger than before)
    private func iconButton(systemName: String,
                            accessibilityLabel: String,
                            isActive: Bool = false,
                            action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: systemName)
                .font(.system(size: 19, weight: .semibold))
                .foregroundStyle(isActive ? Color.yellow : Color.white)
                .frame(width: 40, height: 40)
                .background(Color.white.opacity(0.12))
                .clipShape(RoundedRectangle(cornerRadius: 12))
        }
        .buttonStyle(.plain)
        .accessibilityLabel(accessibilityLabel)
    }
}

#Preview {
    ContentView()
}
