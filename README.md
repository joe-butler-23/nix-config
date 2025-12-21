# NixOS Configuration

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **Pure NixOS**: All packages and services managed at the system level (no Home Manager)
- **Chezmoi**: Dotfile management for configs (Hyprland, Zsh, Kitty, etc.) via symlinks
- **SOPS**: Encrypted secrets management at system level
- **Custom AI Stack**: Integrated MCP wrappers and AI utilities from local flakes
- **Concise Aliases**: Quick commands for configuration management (ns, nsdry)

## Repository Structure

```
nix-config/
├── README.md                    # This file
├── nixos-install-guide.md       # Complete installation guide
├── flake.nix                    # Flake configuration and inputs
├── modules/                     # Modular configuration
│   ├── core/                    # Core system (boot, hardware, env, user-dirs, sops)
│   ├── desktop/                 # Desktop environment (Hyprland, Web Apps)
│   ├── overlays/                # Custom package overlays (OpenCode, Claude, etc.)
│   ├── services/                # System-level user services (Espanso, Syncthing)
│   ├── scripts/                 # Custom maintenance and utility scripts
│   ├── editor/                  # Editor configurations (Emacs)
│   └── hosts/                   # Host-specific configurations
│       ├── desktop-nix.nix      # Desktop-specific settings
│       └── laptop-nix.nix       # Laptop-specific (Kanshi, TLP, etc.)
├── parts/                       # Flake parts (formatting, systems)
├── secrets/                     # SOPS encrypted secrets
└── assets/                      # Static assets (wallpaper)
```

## Installation

### Prerequisites
- NixOS 25.05 or later
- Basic familiarity with command line
- Target system with UEFI boot support

### Quick Start
1. **Clone repository:**
   ```bash
   git clone https://github.com/joe-butler-23/nix-config.git
   cd nix-config
   ```

2. **Apply system configuration:**
   ```bash
   # Replace 'laptop-nix' with your host name
   sudo nixos-rebuild switch --flake .#laptop-nix
   ```

### Code Formatting
```bash
# Format all Nix files
nix fmt
```

## Configuration Management

This configuration is entirely system-level. Binary packages and system services are managed declaratively through NixOS modules, while dotfiles are managed by Chezmoi using symlinks for instant updates.

### Configuration Layers
- **System Packages**: `modules/core/sys-apps.nix` - All system-wide packages
- **User Services**: `modules/services/` - Systemd user services (Espanso, Kanshi)
- **Dotfiles**: Managed by Chezmoi (`~/.local/share/chezmoi/`) - symlinked to `~/.config/`
- **Secrets**: SOPS-encrypted in `secrets/secrets.yaml`, deployed to `/run/secrets/`
- **AI Tools**: Custom MCP wrappers and utilities imported from `sys-arc/ai` flake

### Aliases

| Alias | Command | Purpose |
|-------|---------|---------|
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Apply system configuration |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | Test changes without applying |

### Practical Usage

#### Installing a New Package
1. Add to `modules/core/sys-apps.nix`
2. Run `ns` to apply

#### Adding a New Service
1. Create config in `modules/services/<service>.nix`
2. Define as `systemd.user.services.<service>` (or `services.<service>` for system services)
3. Import in `modules/services/default.nix`
4. Run `ns` to apply

#### Managing Dotfiles
1. Dotfiles are managed by Chezmoi. Since they are symlinked, edits in `~/.config/` take effect immediately.
2. To add a new config to management: `chezmoi add ~/.config/<app>/config`

#### Managing Secrets
1. Edit encrypted secrets: `sops secrets/secrets.yaml`
2. Reference secret in `modules/core/sops.nix` or your service module.
3. Run `ns` to deploy to `/run/secrets/`

## Local Packages with Flakes

To install a local project (e.g., `ai-utilities`) as a Nix package:

1.  **Define Flake**: Ensure your local project has a `flake.nix` exporting a `nixosModule` or `overlay`.
2.  **Add Input**: Add the local path to `flake.nix` inputs:
    ```nix
    inputs.ai-utilities.url = "git+file:///home/user/projects/ai";
    ```
3.  **Import & Use**: Import the module in `parts/systems.nix` or apply the overlay in `overlays/default.nix`.
