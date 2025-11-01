# NixOS Configuration Fixes

## Issues Addressed
- SSH/Tailscale integration (remove hardcoded ListenAddress)
- GPU acceleration for Hyprland (add hardware.opengl.enable)
- Nix garbage collection configuration (7-day retention)

## Implementation Steps
- [ ] Fix SSH configuration for Tailscale compatibility
- [ ] Add GPU acceleration for Hyprland
- [ ] Configure Nix garbage collection (7-day retention)
- [ ] Test configuration with dry-build
- [ ] Apply changes with nixos-rebuild
