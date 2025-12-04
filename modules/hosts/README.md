# Host Configurations

This directory contains the machine-specific configurations for different NixOS hosts managed by this flake.

## Overview

Each file corresponds to a `nixosConfiguration` entry in `flake.nix`. These modules define host-specific settings such as hostname, hardware quirks, power management, and monitor profiles.

## Hosts

### `laptop-nix.nix`
Configuration for the laptop environment.

- **Hostname:** `laptop-nix`
- **Safety Check:** Includes an assertion to prevent accidental deployment to the wrong machine.
- **Power Management:**
  - Enables `thermald` and `tlp` for battery optimization.
  - Custom TLP settings to handle USB autosuspend (preventing keyboard standby on AC).
- **Display Management (Kanshi):**
  - Configures `kanshi` for dynamic display profile switching (undocked, docked, double docked).
  - Automatically reloads Hyprland when monitor configuration changes.

### `desktop-nix.nix`
Configuration for the desktop environment.

- **Hostname:** `desktop-nix`
- **Safety Check:** Includes an assertion to ensure configuration matches the hostname.

## Usage

To apply a specific host configuration:

```bash
# For laptop
sudo nixos-rebuild switch --flake .#laptop-nix

# For desktop
sudo nixos-rebuild switch --flake .#desktop-nix
```

**Note:** The root `flake.nix` maps these modules to the respective system configurations.
