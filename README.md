# NixOS Configuration

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **Hybrid Setup**: Both system-level NixOS and integrated Home Manager configurations
- **Concise Aliases**: Quick commands for configuration management (ns, nsdry)
- **Stylix** for unified system-wide theming and colour schemes

## Repository Structure

```
nix-config/
├── README.md                    # This file
├── nixos-install-guide.md       # Complete installation guide
├── flake.nix                    # Flake configuration and inputs
├── modules/                     # Modular configuration
│   ├── core/                    # Core system modules (boot, hardware, system pkgs)
│   ├── desktop/                 # Desktop environment (Hyprland, Waybar, etc.)
│   ├── apps/                    # User applications and tools
│   ├── shell/                   # Shell configuration (Zsh, terminals)
│   ├── scripts/                 # Custom maintenance and utility scripts
│   └── hosts/                   # Host-specific configurations
│       ├── desktop-nix.nix
│       └── laptop-nix.nix
├── parts/                       # Flake parts (formatting, systems)
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

This configuration uses NixOS with Home Manager integrated as a module. This means you typically manage the entire system (including user config) with `nixos-rebuild`.

### Aliases

| Alias | Command | Purpose |
|-------|---------|---------|
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Update system & user config |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | Test changes safely |

*Note: The standalone `home-manager` command (`hs`) is less commonly used in this integrated setup, as `ns` handles everything.*

### Practical Usage

#### Installing a New Tool
1.  Add the package to `modules/apps/default.nix` (or a specific category file).
2.  Run `nsdry` to test.
3.  Run `ns` to apply.

#### Changing System Settings
1.  Edit `modules/core/system.nix` or `modules/hosts/<host>.nix`.
2.  Run `ns` to apply.

## Local Packages with Flakes

To install a local project (e.g., `wlr-which-key`) as a Nix package:

1.  **Define Flake**: Ensure your local project has a `flake.nix` exporting a package.
2.  **Add Input**: Add the local path to `flake.nix` inputs:
    ```nix
    inputs.whichkey.url = "git+file:///home/user/dev/whichkey";
    ```
3.  **Import & Use**: Pass the input to specialArgs and add to `home.packages` in a module.

