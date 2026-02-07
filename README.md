# AimCam

An iPhone AR app prototype to help pool players visualize the "ghost ball" position for potting a target ball into a selected pocket.

## What’s implemented (v0)

- Live camera via ARKit/RealityKit (`ARView`)
- Pinch-to-zoom (digital zoom via view scaling)
- Tap a **target ball** (first tap) → detect ball boundary (Sobel + RANSAC circle fit) in a small ROI, use LiDAR depth when available, then raycast from the detected center
- Tap a **pocket** (second tap) → intersect tap ray with table plane; pocket center Y is forced to the ball center Y (level table assumption)
- Render a world-locked **ghost ball** at \(2r\) behind the target ball along the ball→pocket line
- Automatic “Update Ball” action: updates the ball by re-detecting at the projected ball center (works from the button or by tapping the overlay)
- If the ghost ball is already placed, pocket X/Z is preserved; pocket Y is updated to the new ball height and ghost ball is re-placed
- Debug overlays (ROI, edge dots, fitted circle) toggled by a debug button
- Flashing confirmation of detected boundary circle after successful ball detection
- Start/stop button to pause/resume AR session for power saving
- Auto-reset if tracking is lost or the camera is blocked for several seconds after ghost placement
- Compact icon-based UI with top bar controls and bottom instructions

## Requirements

- Xcode 15+ (should work with the installed Xcode)
- iOS 17+ (tested target); physical device recommended
- LiDAR iPhones improve ball sizing and stability, but plane-based fallback works without LiDAR
- Camera permission (prompted on first run)

## Run

1. Open `AimCam.xcodeproj` in Xcode
2. Select a physical iPhone device (recommended)
3. Build & Run

## Notes / next steps

- Refine ball detection stability under low light and glare
- Add optional aim line from target ball → pocket

