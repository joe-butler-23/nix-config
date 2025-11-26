# Gemini Agent Guide for This NixOS Configuration

This document provides specific instructions for you, a Gemini agent, to manage this NixOS configuration for the user `joebutler`. Your primary directive is to make all changes declaratively by editing the Nix files in this repository, adhering to its established structure and conventions.

## 1. Core Principles & Safety

### 1.1. Declarative & Modular
All changes must be made within the `.nix` files. The configuration is split between system-level (`modules/sys`) and user-level (`modules/home`) modules. Dotfiles for applications are managed individually in `modules/home/dotfiles/`.

### 1.2. Critical Safety Protocols

When a user asks you for edits or changes, you must present them with a clear an unambiguous set of proposed changes/edits. You should not proceed until you have received explicit confirmation from the user to continue. When you do continue, bear in mind that NixOS system changes can render the system unbootable if improperly implemented. You **MUST** adhere to the following:
- **Verify Before Applying**: **Always** run a dry-build (`hsdry` or `nsdry`) to check for errors before applying the configuration with `hs` or `ns`.
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
4.  **Verify Build**: **Always** test the build before applying it. Use `hsdry` for user changes or `nsdry` for system changes.
5.  **Apply Configuration**: If verification succeeds, apply the configuration using `hs` or `ns`.
6.  **Push to Remote**: After a successful application, push your commit to the remote repository. This backs up your working configuration and makes it available to other systems.
    *Example: `git push`*

## 2. Hybrid Setup & Concise Aliases

This configuration uses a hybrid approach with both system-level NixOS and user-level Home Manager management. You **MUST** use the following aliases for all configuration tasks.

| Alias | Command | Purpose | When to Use |
|---|---|---|---|
| `hs` | `home-manager switch --flake "$HOME/nix-config#joebutler"` | Update **user** configuration | Daily user changes, package updates, dotfile tweaks. |
| `ns` | `sudo nixos-rebuild switch --flake "$HOME/nix-config"` | Update **system** configuration | System services, kernel changes, system-wide packages. |
| `hsdry` | `home-manager build --flake "$HOME/nix-config#joebutler"` | **Test** user changes | Safe testing before applying user config. **ALWAYS run this first.** |
| `nsdry` | `sudo nixos-rebuild dry-build --flake "$HOME/nix-config"` | **Test** system changes | Safe testing before applying system config. **ALWAYS run this first.** |
| `hstatus`| `home-manager generations` | View user history | Check configuration generations and get rollback numbers. |
| `hn` | `home-manager news` | Check for updates | Review important Home Manager news. |

## 3. Agent Playbooks

### Playbook 1: Install a `nixpkgs` Package
**Example**: Add `btop`, a command-line resource monitor, for the user `joebutler`.
1.  **Scope**: This is a user-specific tool.
2.  **Locate File**: Open `modules/home/packages.nix`.
3.  **Add Package**: Add `btop` to the `home.packages` list.
4.  **Verify**: Run `hsdry`.
5.  **Apply**: If verification succeeds, run `hs`.

### Playbook 2: Enable a Service (System or User)
**Objective**: Enable a background service or daemon.
1.  **Determine Scope**: Is this a **system-wide service** (e.g., git or Docker) or a **user-specific service** (e.g., a clipboard manager)?
2.  **Locate File**:
    *   System services: `modules/sys/services.nix`.
    *   User services: `modules/home/services.nix`.
3.  **Make Change**:
    *   System: `virtualisation.docker.enable = true;`
    *   User: `services.espanso.enable = true;`
4.  **Verify and Apply**:
    *   System service: Run `nsdry`, then `ns`.
    *   User service: Run `hsdry`, then `hs`.

### Playbook 3: Change an Application's Configuration
**Example**: Change the font size in the `foot` terminal.
1.  **Identify Module**: The configuration for `foot` is in `modules/home/dotfiles/foot.nix`.
2.  **Edit Configuration**: In that file, locate `programs.foot.settings` and modify the `font` value.
3.  **Verify and Apply**: This is a user-level change. Run `hsdry`, then `hs`.

### Playbook 4: Update the System Theme
**Example**: Change the system-wide wallpaper and color scheme.
1.  **Identify Module**: Theming is controlled by `stylix` in `modules/sys/stylix.nix`.
2.  **Edit Configuration**:
    *   Change wallpaper: Update the `stylix.image` path.
    *   Change color scheme: Update `stylix.base16Scheme`.
3.  **Verify and Apply**: This is a system-level change. Run `nsdry`, then `ns`.

### Playbook 5: Install a Local Package (The `whichkey` Example)
**Example**: Replicate the process used to install the local `wlr-which-key` flake.
1.  **Add Flake Input**: In the root `flake.nix`, add `whichkey` as an input.
    ```nix
    # In flake.nix inputs
    whichkey.url = "git+file:///home/joebutler/development/whichkey";
    ```
2.  **Expose to Modules**: In `flake.nix`, pass `whichkey` down to `specialArgs` (for NixOS) and `extraSpecialArgs` (for Home Manager).
3.  **Create Module**: Create the file `modules/home/dotfiles/whichkey.nix`.
4.  **Define Package**: Add the package to `home.packages` and configure it in the new file.
    ```nix
    # In modules/home/dotfiles/whichkey.nix
    { pkgs, whichkey, ... }: {
      home.packages = [ whichkey.packages.x86_64-linux.wlr-which-key ];
      xdg.configFile."wlr-which-key/config.yaml".text = ''
        font: JetBrainsMono Nerd Font 11
        # ... other config
      '';
    }
    ```
5.  **Import Module**: In `modules/home/dotfiles/default.nix`, add `./whichkey.nix` to the `imports` list.
6.  **Verify and Apply**: Since `flake.nix` was modified, this requires a system rebuild. Run `nsdry`, then `ns`.

### Playbook 6: Troubleshooting
**Example**: Recover from a failed `hs` update that broke an application.
1.  **Analyze Error**: Read the output from the failed `hs` command.
2.  **Rollback Immediately**: Use `hstatus` to list recent user generations. Find the number of the generation before the failed one. Roll back to it using `home-manager switch --generation <number>`. This restores a working state.
3.  **Investigate**: Use `git diff` to see what changes were made.
4.  **Iterate**: Correct the error in the relevant `.nix` file. Use `hsdry` repeatedly until it reports success.
5.  **Apply Fix**: Once `hsdry` passes, run `hs` to apply the corrected configuration.

## 4. Repository Structure & File Reference

```
nix-config/
├── flake.nix                    # Flake configuration and inputs
├── configuration.nix            # Main system configuration
├── home.nix                     # Home Manager configuration
├── modules/                     # Modular configuration
│   ├── sys/                     # System-level modules
│   │   ├── packages.nix         # System packages
│   │   ├── services.nix         # System services
│   │   └── stylix.nix           # Stylix theming
│   ├── home/                    # Home Manager modules
│   │   ├── packages.nix         # User packages
│   │   ├── services.nix         # User services
│   │   └── dotfiles/            # Application configurations
│   │       ├── default.nix      # Dotfiles entry point
│   │       ├── foot.nix         # Foot terminal config
│   │       ├── hyprland.nix     # Hyprland compositor config
│   │       └── zsh.nix          # Shell configuration & ALIASES
│   └── hosts/                   # Host-specific configurations
│       ├── desktop-nix.nix
│       └── laptop-nix.nix
└── wallpaper.jpeg               # Wallpaper for theming
```

## 5. Documentation & Resources
When in doubt, refer to official documentation:
- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Home Manager Manual](https://nix-community.github.io/home-manager/)
- [Stylix Documentation](https://danth.github.io/stylix/)
- [Hyprland Wiki](https://wiki.hyprland.org/)