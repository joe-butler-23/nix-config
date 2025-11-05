# NixOS Configuration

### System Configuration
- **NixOS 25.05** with experimental features enabled (flakes, nix-command)
- **Hybrid Setup**: Both system-level NixOS and standalone Home Manager configurations
- **Concise Aliases**: Quick commands for configuration management (hs, ns, hsdry, nsdry, hn, hstatus)
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
│           ├── whichkey.nix     # wlr-which-key configuration
│           ├── yazi.nix         # File manager config
│           ├── zathura.nix      # PDF viewer config
│           └── zsh.nix          # Shell configuration
└── Makefile                     # Helper commands
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

## Hybrid Setup & Concise Aliases

This configuration uses a hybrid approach that provides both system-level NixOS management and standalone Home Manager functionality, with convenient aliases for quick access.

### Available Aliases

| Alias | Command | Purpose | When to Use |
|-------|---------|---------|-------------|
| `hs` | `home-manager switch --flake "$HOME/nix-config#joebutler"` | Update user configuration | Daily user changes, package updates, dotfile tweaks |
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Update system configuration | System services, kernel changes, system packages |
| `hsdry` | `home-manager build --flake "$HOME/nix-config#joebutler"` | Test user changes | Safe testing before applying user config |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | Test system changes | Safe testing before applying system config |
| `hn` | `home-manager news` | Check for updates | Review important Home Manager news |
| `hstatus` | `home-manager generations` | View history | Check configuration generations and rollback options |

### Practical Usage Scenarios

#### Scenario 1: Installing a New User Tool
```bash
# Add package to modules/home/packages.nix
# Then apply user-level changes (fast, no sudo required)
hs
```

#### Scenario 2: Adding a System Service
```bash
# Add service to modules/sys/services.nix
# Then apply system-level changes (requires sudo)
ns
```

#### Scenario 3: Testing Configuration Changes
```bash
# Make changes to dotfiles or packages
# Test first without applying
hsdry

# If successful, apply the changes
hs
```

#### Scenario 4: Troubleshooting After Update
```bash
# Check what generations are available
hstatus

# Review recent Home Manager news
hn

# If needed, rollback to previous generation
home-manager switch --generation <generation-number>
```

#### Scenario 5: System Maintenance
```bash
# Check for system updates first
nsdry

# If no issues, apply system updates
ns

# Then update user packages
hs
```

### Benefits of Hybrid Approach

1. **Speed**: User updates (`hs`) are much faster than full system rebuilds
2. **Safety**: Dry run commands (`hsdry`, `nsdry`) prevent breaking changes
3. **Flexibility**: Choose between user-only vs full system updates
4. **Convenience**: Short aliases work from any directory
5. **Isolation**: User changes don't affect system stability
6. **Portability**: Home Manager config can be used on other NixOS systems

### Workflow Recommendations

- **Daily Development**: Use `hs` for quick user package/config changes
- **System Changes**: Use `ns` for system services, kernel, or system packages
- **Testing**: Always use `hsdry`/`nsdry` before applying major changes
- **Maintenance**: Use `hn` weekly to check for important updates
- **Troubleshooting**: Use `hstatus` to review and rollback if needed

## Guide: Installing Local Packages with Flakes

This guide demonstrates how to install a local Rust project as a Nix package using flakes, based on wlr-which-key implementation.

### 1. Create Project Flake

In your project directory (e.g., `/home/joebutler/development/whichkey`):

```nix
# flake.nix
{
  description = "wlr-which-key - A Wayland which-key menu";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, rust-overlay, crane, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import rust-overlay) ];
      };

      craneLib = crane.lib.${system};
      rustToolchain = pkgs.rust-bin.stable.latest.default;

      src = craneLib.cleanCargoSource ./.;

      commonArgs = {
        inherit src;
        strictDeps = true;
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        nativeBuildInputs = with pkgs; [ pkg-config ];
        buildInputs = with pkgs; [
          pango
          cairo
          gdk-pixbuf
          gtk3
        ];
      };

      package = craneLib.buildPackage (commonArgs // {
        pname = "wlr-which-key";
        version = "0.1.0";
      });
    in {
      packages.${system}.default = package;
      packages.${system}.wlr-which-key = package;
    };
}
```

### 2. Add to System Configuration

Add local flake as an input to your main nix-config:

```nix
# flake.nix (in your nix-config)
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = { /* ... */ };
    
    # Local development flake
    whichkey.url = "git+file:///home/joebutler/development/whichkey";
  };

  outputs = { nixpkgs, home-manager, whichkey, ... }: {
    nixosConfigurations = {
      laptop-nix = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit whichkey; };
        modules = [
          # ... other modules
          home-manager.nixosModules.home-manager
          {
            home-manager.users.joebutler = import ./home.nix;
            home-manager.extraSpecialArgs = { inherit whichkey; };
          }
        ];
      };
    };
  };
}
```

### 3. Create Home Manager Module

Create a dedicated module for the package:

```nix
# modules/home/dotfiles/whichkey.nix
{ pkgs, whichkey, ... }: {
  home.packages = [
    whichkey.packages.x86_64-linux.wlr-which-key
  ];

  # Optional: Manage configuration files
  xdg.configFile."wlr-which-key/config.yaml".text = ''
    # Your configuration here
    font: JetBrainsMono Nerd Font 11
    background: "#282828d0"
    color: "#ffffff"
    border: "#ffffff"
    # ... rest of config
  '';
}
```

### 4. Import Module

Add module to your dotfiles imports:

```nix
# modules/home/dotfiles/default.nix
{
  imports = [
    ./espanso.nix
    ./foot.nix
    ./hyprland.nix
    ./whichkey.nix  # Add this line
    # ... other modules
  ];
}
```

### 5. Rebuild and Test

```bash
# Rebuild system
sudo nixos-rebuild switch

# Test package
which wlr-which-key
# Should show: /home/joebutler/.nix-profile/bin/wlr-which-key
```

### Key Benefits

- **Reproducible**: Same package across all machines
- **Declarative**: Configuration managed in Nix
- **Isolated**: No conflicts with system packages
- **Updatable**: Changes automatically propagate on rebuild

### Common Issues

- **"access to absolute path forbidden"**: Use `xdg.configFile.text` instead of absolute paths
- **"does not provide attribute"**: Ensure flake outputs correct package name
- **Missing dependencies**: Add required libraries to `buildInputs`
- **"Git tree is dirty"**: Normal for development - doesn't affect functionality
- **"Will not write lock file"**: Normal for local inputs - doesn't affect functionality

## Resources

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)
- [Stylix Documentation](https://danth.github.io/stylix/)
- [Hyprland Wiki](https://wiki.hyprland.org/)
- [Crane Build System](https://github.com/ipetkov/crane)
- [Rust Overlay](https://github.com/oxalica/rust-overlay)
