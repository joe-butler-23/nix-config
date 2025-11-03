# NixOS Configuration

A modern, declarative NixOS configuration with Home Manager integration, featuring a complete Wayland desktop environment with Hyprland, comprehensive theming with Stylix, and modular configuration structure.

## 🚀 Features

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **Home Manager** integration for user-level configuration management
- **Stylix** for unified system-wide theming and colour schemes
- **Modular structure** with separate system and home configurations
- **Automatic garbage collection** with weekly cleanup

### Desktop Environment
- **Hyprland** - Dynamic tiling Wayland compositor
- **Waybar** - Customisable status bar
- **Rofi** - Application launcher and menu system
- **Foot** - Fast, lightweight terminal emulator
- **Mako** - Notification daemon
- **Wlogout** - Logout/power menu
- **Yazi** - Terminal file manager

### Development Tools
- **Git** with version control integration
- **Starship** - Customisable shell prompt
- **Zsh** with enhanced configuration
- **Development utilities** and tools

### Theming & Customisation
- **Stylix** integration for consistent colour schemes
- **Papirus-Dark** icon theme
- **Custom Hyprland** configuration with animations and effects
- **Unified colour scheme** across all applications
- **Impermanence** support with Btrfs subvolumes

## 📁 Repository Structure

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
│           ├── hyprland-extras.nix # Additional Hyprland settings
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

## 🛠️ Installation

### Prerequisites
- NixOS 25.05 or later
- Basic familiarity with the command line
- Target system with UEFI boot support

### Quick Start
1. **Clone this repository:**
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

### Detailed Installation
For a complete step-by-step installation guide, including disk encryption, Btrfs setup, and initial system configuration, see [nixos-install-guide.md](./nixos-install-guide.md).

## ⚙️ Configuration Management

### System Updates
```bash
# Update system configuration
sudo nixos-rebuild switch --flake .

# Update home configuration only
home-manager switch --flake .#joebutler

# Update both system and home
make update
```

### Testing Changes
```bash
# Test system configuration without applying
sudo nixos-rebuild test --flake .

# Build configuration to check for errors
sudo nixos-rebuild build --flake .

# Dry run to see what would be built
sudo nixos-rebuild dry-build --flake .
```

### Code Formatting
```bash
# Format all Nix files
make fmt

# Check formatting without applying
make check
```

## 🎨 Customisation

### Theming
The configuration uses **Stylix** for unified theming. To customise colours:

1. Update the wallpaper in `modules/sys/stylix.nix`
2. Stylix will automatically generate a colour palette
3. All supported applications will use the new theme

### Adding Packages
- **System packages:** Edit `modules/sys/packages.nix`
- **User packages:** Edit `modules/home/packages.nix`

### Application Configuration
Individual application configurations are located in `modules/home/dotfiles/`. Each file contains the specific configuration for that application.

## 🔧 Development

### Flake Inputs
- **nixpkgs:** NixOS 25.05 release channel
- **nixpkgs-unstable:** Unstable packages for specific applications
- **home-manager:** User environment management
- **stylix:** System-wide theming
- **treefmt-nix:** Code formatting

### Build Targets
```bash
# Build system configuration
nix build .#nixosConfigurations.nixos.config.system.build.toplevel

# Build home configuration
nix build .#homeConfigurations.joebutler.activationPackage

# Format code
nix fmt
```

## 📚 Documentation

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)
- [Stylix Documentation](https://danth.github.io/stylix/)
- [Hyprland Wiki](https://wiki.hyprland.org/)

## 🤝 Contributing

1. Fork this repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make your changes
4. Test thoroughly: `make check && make test`
5. Commit your changes: `git commit -m "Add feature"`
6. Push to the branch: `git push origin feature-name`
7. Open a pull request

## 📄 License

This configuration is provided as-is for educational and personal use. Feel free to adapt and modify it for your own needs.

## 🔗 Related Projects

- [NixOS](https://nixos.org/) - Declarative Linux distribution
- [Home Manager](https://github.com/nix-community/home-manager) - User environment management
- [Stylix](https://github.com/danth/stylix) - System-wide theming
- [Hyprland](https://github.com/hyprwm/Hyprland) - Dynamic tiling Wayland compositor

---

**Note:** This configuration is tailored for a specific user (`joebutler`) and system architecture (`x86_64-linux`). Adjust usernames and paths as needed for your own setup.
