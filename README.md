# NixOS Configuration

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **System-Level Configuration**: Packages and services managed at system level
- **Minimal Home Manager**: Integrated for user-level applications only
- **Chezmoi**: Dotfile management for configs (Hyprland, Zsh, Kitty, etc.)
- **SOPS**: Encrypted secrets management at system level
- **Stylix**: Unified system-wide theming and colour schemes
- **Concise Aliases**: Quick commands for configuration management (ns, nsdry)

## Repository Structure

```
nix-config/
├── README.md                    # This file
├── nixos-install-guide.md       # Complete installation guide
├── flake.nix                    # Flake configuration and inputs
├── modules/                     # Modular configuration
│   ├── core/                    # Core system (boot, hardware, system packages)
│   ├── desktop/                 # Desktop environment (Hyprland, Stylix)
│   ├── apps/                    # User applications (GUI apps, dev tools)
│   ├── services/                # System-level user services (Espanso, Syncthing)
│   ├── shell/                   # Shell config (environment vars, SOPS, XDG)
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

2. **Update flake inputs:**
   ```bash
   nix flake update
   ```

3. **Apply system configuration:**
   ```bash
   # Replace 'laptop-nix' with your host name
   sudo nixos-rebuild switch --flake .#laptop-nix
   ```

### Installation
Step-by-step installation guide, including disk encryption, Btrfs setup, and initial system configuration, at [nixos-install-guide.md](./docs/nixos-install-guide.md).

### Code Formatting
```bash
# Format all Nix files
nix fmt

# Check formatting without applying
nix check
```

## Configuration Management

This configuration is primarily system-level with minimal Home Manager integration for user applications. Most configurations are managed declaratively through Nix, with dotfiles handled by Chezmoi.

### Configuration Layers
- **System Packages**: `modules/core/sys-apps.nix` - All system-wide packages
- **User Services**: `modules/services/` - Systemd user services (Espanso, Syncthing)
- **Dotfiles**: Managed by Chezmoi (`~/.local/share/chezmoi/`) for Hyprland, Zsh, Kitty, etc.
- **Secrets**: SOPS-encrypted in `secrets/secrets.yaml`, deployed to `/run/secrets/`
- **User Apps**: Home Manager handles minimal user-level packages in `modules/apps/`

### Aliases

| Alias | Command | Purpose |
|-------|---------|---------|
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Apply system configuration |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | Test changes without applying |

*Note: This setup uses integrated Home Manager as a NixOS module, managed entirely via `nixos-rebuild`.*

### Practical Usage

#### Installing a New Package
1. Add to `modules/core/sys-apps.nix` (all system packages are here)
2. Run `nsdry` to test
3. Run `ns` to apply

#### Adding a New Service
1. Create config in `modules/services/<service>.nix`
2. Define as `systemd.user.services.<service>`
3. Import in `modules/services/default.nix`
4. Run `ns` to apply

#### Managing Dotfiles
1. Dotfiles are managed by Chezmoi in `~/.local/share/chezmoi/`
2. Edit: `chezmoi edit ~/.config/<app>/config`
3. Apply: `chezmoi apply`

#### Managing Secrets
1. Edit encrypted secrets: `sops secrets/secrets.yaml`
2. Add secret to `modules/shell/sops.nix` or `modules/services/<service>.nix`
3. Run `ns` to deploy to `/run/secrets/`

#### Changing System Settings
1. Edit `modules/core/system.nix` or `modules/hosts/<host>.nix`
2. Run `ns` to apply

## Local Packages with Flakes

To install a local project (e.g., `wlr-which-key`) as a Nix package:

1.  **Define Flake**: Ensure your local project has a `flake.nix` exporting a package.
2.  **Add Input**: Add the local path to `flake.nix` inputs:
    ```nix
    inputs.whichkey.url = "git+file:///home/user/dev/whichkey";
    ```
3.  **Import & Use**: Pass the input to specialArgs and add to `home.packages` in a module.
