# NixOS Configuration

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **Home Manager** integration for user-level configuration management
- **Stylix** for unified system-wide theming and colour schemes

## Repository Structure

```
nix-config/
├── README.md                    # This file
├── nixos-install-guide.md       # Complete installation guide
├── flake.nix                    # Flake configuration and inputs
├── configuration.nix            # Main system configuration
├── home.nix                     # Home Manager configuration
├── hardware-configuration.nix   # Hardware-specific settings
├── treefmt.nix                  # Code formatting configuration
├── wallpaper.jpeg               # Wallpaper for theming
├── modules/                     # Modular configuration
│   ├── sys/                     # System-level modules
│   │   ├── packages.nix         # System packages
│   │   ├── services.nix         # System services
│   │   └── stylix.nix           # Stylix configuration
│   └── home/                    # Home Manager modules
│       ├── packages.nix         # User packages
│       ├── services.nix         # User services
│       └── dotfiles/            # Application configurations
│           ├── default.nix      # Dotfiles entry point
│           ├── foot.nix         # Foot terminal config
│           ├── hyprland.nix     # Hyprland compositor config
│           ├── hyprland-extras.nix # Additional Hypr settings
│           ├── mako.nix         # Notification daemon config
│           ├── rofi.nix         # Application launcher config
│           ├── starship.nix     # Shell prompt config
│           ├── waybar.nix       # Status bar config
│           ├── wlogout/         # Logout menu config
│           ├── yazi.nix         # File manager config
│           ├── zathura.nix      # PDF viewer config
│           └── zsh.nix          # Shell configuration
└── Makefile                     # Helper commands
```

## Installation

### Prerequisites
- NixOS 25.05 or later
- Basic familiarity with the command line
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
   sudo nixos-rebuild switch --flake .
   ```

4. **Apply home configuration:**
   ```bash
   home-manager switch --flake .#joebutler
   ```

### Installation
Step-by-step installation guide, including disk encryption, Btrfs setup, and initial system configuration, at [nixos-install-guide.md](./nixos-install-guide.md).

### Code Formatting
```bash
# Format all Nix files
nix fmt

# Check formatting without applying
nix check
```
## Resources

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)
- [Stylix Documentation](https://danth.github.io/stylix/)
- [Hyprland Wiki](https://wiki.hyprland.org/)