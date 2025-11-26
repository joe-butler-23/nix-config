# Gemini Agent Guide for This NixOS Configuration

This document provides specific instructions for you, a Gemini agent, to manage this NixOS configuration for the user `joebutler`. Your primary directive is to make all changes declaratively by editing the Nix files in this repository, adhering to its established structure and conventions.

For additional agent-specific guidelines and technical details (e.g., pre-commit hooks), please refer to `AGENTS.md`.

## 1. Core Principles & Safety

### 1.1. Declarative & Modular
All changes must be made within the `.nix` files. The configuration is split into logical modules:
- `modules/core`: System-level configs (boot, hardware, system packages).
- `modules/desktop`: Desktop Environment configs (Hyprland, Waybar, theming).
- `modules/apps`: User applications and development tools.
- `modules/shell`: Shell and terminal configurations.
- `modules/hosts`: Host-specific entry points.

### 1.2. Critical Safety Protocols

When a user asks you for edits or changes, you must present them with a clear an unambiguous set of proposed changes/edits. You should not proceed until you have received explicit confirmation from the user to continue. When you do continue, bear in mind that NixOS system changes can render the system unbootable if improperly implemented. You **MUST** adhere to the following:
- **Verify Before Applying**: **Always** run a dry-build (`nsdry`) to check for errors before applying the configuration with `ns`.
- **Incremental Changes**: Implement changes incrementally where possible.
- **No Destructive Operations**: Never execute destructive operations without explicit user confirmation.
- **Backup Awareness**: The user is responsible for backups. Your role is to avoid breaking the system.
- **Official documentation**: Do not base important decisions on guesses or conjecture. Ensure that you are accessing up to date and official documentation where possible to inform your decisions.

### 1.3. Git & Change Management Workflow
You must follow this ordered process for making and applying configuration changes to ensure safety and maintain a clean history.

1.  **Edit Code**: Make the desired changes to your `.nix` files.
2.  **Format Code**: Run `nix fmt` to ensure consistent style and formatting.
3.  **Commit Changes**: Commit your changes to Git with a clear, descriptive message. This links your upcoming system generation to a specific code state.
    *Example: `git commit -m "feat(services): enable docker"`*
4.  **Verify Build**: **Always** test the build before applying it. Use `nsdry` for system changes.
5.  **Apply Configuration**: If verification succeeds, apply the configuration using `ns`.
6.  **Push to Remote**: After a successful application, push your commit to the remote repository. This backs up your working configuration and makes it available to other systems.
    *Example: `git push`*

## 2. Integrated Setup & Concise Aliases

This configuration uses an integrated approach where Home Manager is a module within NixOS. This means a single command updates both system and user configurations.

| Alias | Command | Purpose | When to Use |
|---|---|---|---|
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Update **system & user** configuration | All changes (system services, packages, dotfiles). |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | **Test** changes | Safe testing before applying config. **ALWAYS run this first.** |
| `hstatus`| `home-manager generations` | View user history | Check configuration generations and get rollback numbers. |
| `hn` | `home-manager news` | Check for updates | Review important Home Manager news. |

## 3. Agent Playbooks

### Playbook 1: Install a Package
**Example**: Add `btop`, a command-line resource monitor.
1.  **Scope**: Determine if it's a system tool (core) or user app (apps).
2.  **Locate File**: Open `modules/apps/utils.nix` (for user utils) or `modules/core/sys-apps.nix` (for system utils).
3.  **Add Package**: Add `btop` to the `home.packages` (user) or `environment.systemPackages` (system) list.
4.  **Verify**: Run `nsdry`.
5.  **Apply**: If verification succeeds, run `ns`.

### Playbook 2: Enable a Service
**Objective**: Enable a background service or daemon.
1.  **Determine Scope**: Is this a **system-wide service** (e.g., Docker) or a **user-specific service** (e.g., Syncthing)?
2.  **Locate File**:
    *   System services: `modules/core/system.nix` (or a specific service file).
    *   User services: `modules/apps/syncthing.nix` (or `services.nix` if added).
3.  **Make Change**:
    *   System: `virtualisation.docker.enable = true;`
    *   User: `services.syncthing.enable = true;`
4.  **Verify and Apply**: Run `nsdry`, then `ns`.

### Playbook 3: Change an Application's Configuration
**Example**: Change the font size in the `kitty` terminal.
1.  **Identify Module**: The configuration for `kitty` is in `modules/shell/kitty.nix`.
2.  **Edit Configuration**: In that file, locate `programs.kitty.settings` and modify the `font_size` value.
3.  **Verify and Apply**: Run `nsdry`, then `ns`.

### Playbook 4: Update the System Theme
**Example**: Change the system-wide wallpaper and color scheme.
1.  **Identify Module**: Theming is controlled by `stylix` in `modules/desktop/stylix.nix`.
2.  **Edit Configuration**:
    *   Change wallpaper: Update the `stylix.image` path.
    *   Change color scheme: Update `stylix.base16Scheme`.
3.  **Verify and Apply**: Run `nsdry`, then `ns`.

### Playbook 5: Troubleshooting
**Example**: Recover from a failed update.
1.  **Analyze Error**: Read the output from the failed `ns` command.
2.  **Investigate**: Use `git diff` to see what changes were made.
3.  **Iterate**: Correct the error in the relevant `.nix` file. Use `nsdry` repeatedly until it reports success.
4.  **Apply Fix**: Once `nsdry` passes, run `ns` to apply the corrected configuration.
5.  **Rollback (if needed)**: If the system is broken, reboot and select the previous generation from the boot loader menu.

## 4. Repository Structure & File Reference

```
nix-config/
├── flake.nix                    # Flake configuration and inputs
├── modules/                     # Modular configuration
│   ├── core/                    # System-level modules
│   │   ├── default.nix          # Imports all core modules
│   │   ├── hardware.nix         # Hardware config
│   │   ├── sys-apps.nix         # System packages
│   │   └── system.nix           # System services & boot
│   ├── desktop/                 # Desktop Environment
│   │   ├── default.nix          # Imports desktop modules
│   │   ├── hypr/                # Hyprland configuration
│   │   ├── waybar.nix           # Status bar
│   │   └── stylix.nix           # Theming
│   ├── apps/                    # User Applications
│   │   ├── default.nix          # Imports app modules
│   │   ├── dev.nix              # Development tools
│   │   └── web-apps/            # Web applications
│   ├── shell/                   # Shell & Terminal
│   │   ├── default.nix          # Imports shell modules
│   │   ├── zsh.nix              # Zsh config
│   │   └── kitty.nix            # Terminal config
│   └── hosts/                   # Host-specific configurations
│       ├── desktop-nix.nix
│       └── laptop-nix.nix
└── assets/                      # Wallpaper and other assets
    └── wallpaper.jpeg
```

## 5. Documentation & Resources
When in doubt, refer to official documentation:
- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)
- [Stylix Documentation](https://danth.github.io/stylix/)
- [Hyprland Wiki](https://wiki.hyprland.org/)
