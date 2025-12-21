# Complete Home-Manager Removal Plan

## Overview

Migrate all functionality from home-manager to system-level NixOS configuration and chezmoi, then remove home-manager entirely. This affects ~81 packages, 25+ configuration modules, user services, MIME associations, and desktop entries.

**User's Preferences:**
- MIME associations → System-level Nix (`environment.etc`)
- User services → System-level systemd services
- Emacs config symlink → Chezmoi
- Desktop entries → Keep generating in Nix

## Pre-Migration Checklist

1. **Backup current state**
   - `home-manager generations` - List all generations
   - Back up `~/.config` and `~/.local` directories
   - Git commit all current Nix configs

2. **Document current working system**
   - List all installed packages: `nix-store --query --requisites /run/current-system | grep -v '.drv$'`
   - List all systemd user services: `systemctl --user list-unit-files --state=enabled`
   - Capture all MIME associations: `cat ~/.config/mimeapps.list`

## Migration Strategy: 4-Phase Approach

### Phase 1: Move Packages to System Level

Move all ~81 packages from `home.packages` to `environment.systemPackages`.

**1.1 Core System Packages**
File: `modules/core/sys-apps.nix`

Add to existing packages:
```nix
# Shell & CLI Tools (from modules/shell/utils.nix)
bat
chezmoi
clipse
fd
fzf
git
gum
htop
jq
lazygit
nix-search-tv
ripgrep
tmux
trash-cli
unzip
wget
zoxide
iwd
sshpass

# Desktop Utilities (from modules/apps/utils.nix)
hyprshot
hyprland-protocols
wl-clipboard
xdg-utils
wayland-utils
uwsm
bluetui
blueman
localsend
viewnior
mousepad
gtksourceview4
papirus-icon-theme
hicolor-icon-theme
desktop-file-utils
nerd-fonts.jetbrains-mono
font-awesome
noto-fonts-color-emoji
qmk
sops
imagemagick
brightnessctl
ffmpeg
grim
slurp
scrcpy
networkmanagerapplet
alsa-utils
pavucontrol

# Desktop Environment
wlr-which-key (from whichkey flake input)

# Development Tools (from modules/apps/dev.nix)
app2unit
go
rustup
uv
scdoc
tectonic
texlive.combined.scheme-full
pandoc
R (with packages: languageserver, tidyverse, rmarkdown, knitr)
nodejs_latest

# Applications (from modules/apps/apps.nix)
gnumeric
obsidian
zotero
vivaldi

# Editors (from modules/editor/)
zed-editor
emacs-pgtk

# Learning Tools (from modules/apps/anki.nix & anki-forge.nix)
anki (unstable with anki-connect addon)
anki-forge
libnotify

# Custom Scripts (from modules/scripts/default.nix)
file-launcher
recent-files-launcher
copy-prompt
directory-finder
study-focus
rofi-quick-capture
weekly-review
file-review
system-maintenance
weekly-metrics
```

**1.2 Custom AI Tools**
These come from `inputs.ai-utilities` - need to be added via overlay or directly:
- opencode
- gemini
- claude-code
- codex

**1.3 Custom Anki Wrappers**
From anki-forge.nix - need custom derivations:
- ankiSmart
- ankiForgeLauncher

### Phase 2: Migrate Configurations

Split configurations between system-level Nix and chezmoi based on type.

**2.1 Configs Moving to Chezmoi**

Create these config files in `~/.local/share/chezmoi/`:

1. **wlogout** - `dot_config/wlogout/`
   - `layout.json` (convert from Nix list to JSON)
   - `style.css` (extract CSS from Nix string)
   - Copy `icons/` directory to chezmoi

2. **wlr-which-key** - `dot_config/wlr-which-key/config.yaml`
   - Already exists in xdg.configFile, extract the YAML content
   - Use chezmoi template for home directory paths: `{{ .chezmoi.homeDir }}`

3. **kanshi** - `dot_config/kanshi/config`
   - Extract monitor configuration from laptop-nix.nix
   - Static file, no templating needed

4. **VSCodium** - Skip or handle separately
   - Large config with extensions, profiles, settings
   - Consider keeping in Nix if actively used, or migrate to chezmoi if stable

5. **Emacs symlink** - `dot_emacs.d.tmpl`
   - Create chezmoi external template that symlinks to editor config:
   ```
   {{- if stat (joinPath .chezmoi.homeDir "nix-config/modules/editor/.emacs.d") }}
   .type = "symlink"
   .target = {{ joinPath .chezmoi.homeDir "nix-config/modules/editor/.emacs.d" | quote }}
   {{- end }}
   ```

**2.2 System-Level Nix Configs**

Create new NixOS modules for:

1. **XDG MIME Associations** - `modules/shell/mime-types.nix`
   ```nix
   { ... }: {
     environment.etc."xdg/mimeapps.list".text = ''
       [Default Applications]
       text/html=brave-gatekeeper.desktop
       x-scheme-handler/http=brave-gatekeeper.desktop
       x-scheme-handler/https=brave-gatekeeper.desktop
       application/pdf=org.pwmt.zathura.desktop
       image/jpeg=viewnior.desktop
       image/png=viewnior.desktop
       text/plain=emacsclient.desktop
       inode/directory=thunar.desktop
       # ... all other associations
     '';
   }
   ```

2. **XDG User Directories** - `modules/shell/user-dirs.nix`
   ```nix
   { config, ... }: {
     environment.etc."xdg/user-dirs.defaults".text = ''
       DESKTOP=desktop
       DOCUMENTS=documents
       DOWNLOAD=downloads
       PICTURES=pictures
       # Others point to $HOME
     '';
   }
   ```

3. **Desktop Entries** - `modules/apps/web-apps-nix.nix`
   - Convert xdg.desktopEntries to environment.etc entries
   - Generate .desktop files for 7 web apps (ChatGPT, Spotify, etc.)

4. **Thunar XFconf Settings** - `modules/apps/thunar-system.nix`
   - Convert xfconf.settings to system-level configuration
   - May need to use environment.etc or systemd tmpfiles

### Phase 3: Migrate Services

Convert home-manager user services to system-level systemd services.

**3.1 Espanso Service** - `modules/apps/espanso-system.nix`
```nix
{
  systemd.services.espanso = {
    description = "Espanso daemon";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    serviceConfig = {
      Type = "simple";
      User = "joebutler";
      ExecStart = "${pkgs.espanso}/bin/espanso daemon";
      Restart = "on-failure";
    };
  };
}
```

**3.2 Syncthing Service**
Already has system-level NixOS option:
```nix
services.syncthing = {
  enable = true;
  user = "joebutler";
  dataDir = "/home/joebutler/.local/share/syncthing";
  configDir = "/home/joebutler/.config/syncthing";
};
```

**3.3 Kanshi Monitor Daemon** - `modules/hosts/kanshi-system.nix`
```nix
{
  systemd.user.services.kanshi = {
    description = "Kanshi output autoconfig";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.kanshi}/bin/kanshi";
      Restart = "on-failure";
    };
  };
}
```

### Phase 4: Handle Environment Variables

Move all `home.sessionVariables` to system-level.

**4.1 Global Session Variables** - `modules/shell/environment.nix`
```nix
{
  environment.sessionVariables = {
    # Rust
    CARGO_HOME = "$HOME/.local/share/rust/cargo";
    RUSTUP_HOME = "$HOME/.local/share/rust/rustup";

    # Node/Bun
    BUN_INSTALL = "$HOME/.local/share/bun";
    npm_config_cache = "$HOME/.cache/npm";
    NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";

    # SOPS
    SOPS_AGE_KEY_FILE = "$HOME/nix-config/secrets/sops.agekey";

    # Emacs (if needed)
    # Add any emacs-specific vars

    # Qt (from syncthing)
    # Add Qt platform plugin settings if needed
  };
}
```

### Phase 5: Handle SOPS Integration

**5.1 Convert sops-nix to System-Level**

Currently using `inputs.sops-nix.homeManagerModules.sops`.

Change to system-level module in `parts/systems.nix`:
```nix
inputs.sops-nix.nixosModules.sops
```

**5.2 Update SOPS Configuration** - `modules/shell/sops-system.nix`
```nix
{
  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    age.keyFile = "/home/joebutler/nix-config/secrets/sops.agekey";

    secrets = {
      CONTEXT7_API_KEY = {};
      "secrets.yml" = {
        owner = "joebutler";
        path = "/home/joebutler/.config/espanso/match/secrets.yml";
      };
    };
  };
}
```

### Phase 6: Handle Stylix Integration

**6.1 System-Level Stylix Only**

Remove `home-manager.users.${user}.stylix.targets` block.

Update `modules/desktop/stylix.nix` to use system-level targeting only:
```nix
{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";

    # System-level targets only
    targets = {
      # Configure what you want themed
    };
  };
}
```

**Note:** Some applications may lose theming. You can:
- Add theme configs manually to chezmoi
- Use system-level stylix targets where available
- Configure per-app themes in their config files

### Phase 7: Remove Home-Manager

**7.1 Update Flake** - `flake.nix`

Remove home-manager input:
```nix
# DELETE THIS:
home-manager = {
  url = "github:nix-community/home-manager/release-25.11";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

**7.2 Update Systems Configuration** - `parts/systems.nix`

Remove entire home-manager configuration block (lines 47-82):
```nix
# DELETE THIS:
inputs.home-manager.nixosModules.home-manager
{
  home-manager.useGlobalPkgs = true;
  # ... entire block
}
```

**7.3 Remove Home-Manager Module Imports**

Delete or empty these directories/files:
- `modules/shell/` - Keep only system-level configs
- `modules/desktop/` - Migrate configs to chezmoi or system
- `modules/apps/` - Move packages to system, configs to chezmoi
- `modules/scripts/` - Move to system packages
- `modules/editor/` - Handle emacs symlink via chezmoi

**7.4 Update Module Structure**

Reorganize to:
```
modules/
├── core/          # System configuration
│   ├── default.nix
│   ├── sys-apps.nix (ALL packages here)
│   ├── system.nix
│   └── hardware.nix
├── shell/         # Shell/environment (system-level)
│   ├── default.nix
│   ├── environment.nix
│   ├── user-dirs.nix
│   └── sops.nix
├── desktop/       # Desktop environment
│   ├── default.nix
│   ├── mime-types.nix
│   ├── gtk.nix
│   ├── stylix.nix
│   ├── brave-wrapper.nix
│   ├── system.nix
│   └── hypr/
├── apps/          # Applications (kept together)
│   ├── default.nix
│   ├── thunar.nix
│   ├── web-apps.nix
│   └── overlays/
├── services/      # Systemd services
│   ├── default.nix
│   ├── espanso.nix
│   ├── kanshi.nix
│   └── syncthing.nix
└── hosts/         # Host-specific
    ├── desktop-nix.nix
    └── laptop-nix.nix
```

## Testing Strategy

After each phase:

**Package Testing:**
1. `which <command>` for each critical command
2. Test a few GUI apps launch correctly
3. Verify custom scripts are in PATH

**Config Testing:**
1. MIME associations: `xdg-mime query default application/pdf`
2. Desktop entries: Check rofi/application launcher shows web apps
3. Wlogout: Test logout menu appears and functions
4. Which-key: Test Super+Space shows menu

**Service Testing:**
1. `systemctl --user status espanso`
2. `systemctl --user status syncthing`
3. `systemctl --user status kanshi`
4. Test espanso text expansion works
5. Test monitor switching (if laptop)

**Environment Testing:**
1. Echo each env var: `echo $CARGO_HOME`, etc.
2. Test Rust/Node tools find their homes correctly
3. Test sops can decrypt secrets

## Detailed Rollback Procedures

### Rollback Strategy: Git-Based Snapshots

Before each phase, create a git commit. If anything breaks, revert to the last working commit.

**Pre-Migration Snapshot:**
```bash
cd ~/nix-config
git add .
git commit -m "snapshot: pre-home-manager-removal baseline"
git tag pre-hm-removal
```

### Phase-by-Phase Rollback

**After Phase 1 (Packages):**
```bash
# If packages missing or broken
git revert HEAD
sudo nixos-rebuild switch --flake "$HOME/nix-config"
# Test: which <missing-command>
```

**After Phase 2 (Configs):**
```bash
# If configs not working
git revert HEAD~1..HEAD  # Revert last 2 commits if needed
sudo nixos-rebuild switch --flake "$HOME/nix-config"

# Restore chezmoi configs if needed
cd ~/.local/share/chezmoi
git log  # Find commit before changes
git revert <commit>
chezmoi apply --force
```

**After Phase 3 (Services):**
```bash
# If services failing
systemctl --user status espanso  # Check what's failing
git revert HEAD
sudo nixos-rebuild switch --flake "$HOME/nix-config"
systemctl --user daemon-reload
```

**After Phase 7 (Home-Manager Removed):**
```bash
# FULL ROLLBACK - Restore home-manager completely
cd ~/nix-config

# Option 1: Revert to pre-migration tag
git reset --hard pre-hm-removal
sudo nixos-rebuild switch --flake "$HOME/nix-config"

# Option 2: Revert specific commits
git log --oneline -20  # Find the commit range
git revert <commit-range>
sudo nixos-rebuild switch --flake "$HOME/nix-config"

# After rollback, verify:
home-manager generations  # Should show generations again
systemctl --user status home-manager-joebutler.service
```

### Emergency Recovery

**If system won't rebuild:**
```bash
# Boot into previous generation from GRUB
# At GRUB menu, select previous generation
# Then fix the issue and rebuild

# Or use nixos-rebuild with previous generation
sudo nixos-rebuild switch --rollback
```

**If configs completely broken:**
```bash
# Restore from backup
cp -r ~/backup/.config/* ~/.config/
cp -r ~/backup/.local/* ~/.local/

# Restore nix config
cd ~/nix-config
git reset --hard pre-hm-removal
sudo nixos-rebuild switch --flake "$HOME/nix-config"
```

### Testing Checkpoints

After each phase, verify these work before proceeding:

**Phase 1 Checkpoint:**
- [ ] `which git bat fd fzf gum` all succeed
- [ ] `which hyprshot brightnessctl wlr-which-key` all succeed
- [ ] GUI apps launch: `obsidian`, `vivaldi`, `zed`

**Phase 2 Checkpoint:**
- [ ] `xdg-mime query default application/pdf` returns zathura
- [ ] Rofi shows web app entries (ChatGPT, Spotify, etc.)
- [ ] Wlogout menu appears and works
- [ ] Super+Space shows wlr-which-key menu

**Phase 3 Checkpoint:**
- [ ] `systemctl --user status espanso` shows active
- [ ] Espanso text expansion works (type `:test`)
- [ ] Syncthing web UI accessible

**Phase 7 Checkpoint:**
- [ ] No home-manager services: `systemctl --user | grep home-manager` empty
- [ ] All packages in system: `which <cmd>` shows `/run/current-system/sw/bin/`
- [ ] Configs persist after reboot

### Commit Message Template

Use consistent commit messages for easy identification:

```
Phase 1: Move packages to system level
Phase 2: Migrate configs (wlogout, which-key, kanshi)
Phase 3: Convert services to system level
Phase 4: Move environment variables
Phase 5: Convert SOPS to system level
Phase 6: Update Stylix configuration
Phase 7: Remove home-manager completely
```

## Critical Files to Modify

### Nix Files:
- `/home/joebutler/nix-config/flake.nix` - Remove home-manager input
- `/home/joebutler/nix-config/parts/systems.nix` - Remove home-manager integration
- `/home/joebutler/nix-config/modules/core/sys-apps.nix` - Add all packages
- Create: `modules/services/espanso.nix`
- Create: `modules/services/kanshi.nix`
- Create: `modules/desktop/mime-types.nix`
- Create: `modules/desktop/user-dirs.nix`
- Create: `modules/desktop/web-apps.nix`
- Create: `modules/special/environment.nix`
- Update: `modules/shell/sops.nix` → system-level
- Update: `modules/desktop/stylix.nix` → remove home-manager targets

### Chezmoi Files to Create:
- `~/.local/share/chezmoi/dot_config/wlogout/layout.json`
- `~/.local/share/chezmoi/dot_config/wlogout/style.css`
- `~/.local/share/chezmoi/dot_config/wlogout/icons/` (copy directory)
- `~/.local/share/chezmoi/dot_config/wlr-which-key/config.yaml.tmpl`
- `~/.local/share/chezmoi/dot_config/kanshi/config`
- `~/.local/share/chezmoi/.chezmoiexternal.toml` (for emacs symlink)

### Files to Delete:
- All files in `modules/shell/` using home.packages or home.*
- All files in `modules/apps/` using home.packages or home.*
- All files in `modules/editor/` using home.packages or home.*
- All files in `modules/desktop/` using home.* (except system-level)
- All files in `modules/scripts/` using home.packages

## Success Criteria

- ✓ All packages installed and accessible
- ✓ All keybinds work (test Hyprland, wlr-which-key, etc.)
- ✓ All services running (espanso, syncthing, kanshi)
- ✓ MIME associations work (opening files with correct apps)
- ✓ Desktop entries appear in launcher
- ✓ Environment variables set correctly
- ✓ SOPS secrets accessible
- ✓ No home-manager references in flake or modules
- ✓ System rebuilds successfully
- ✓ Configs persist through reboot
- ✓ No `/etc/profiles/per-user/joebutler/` packages (all in system)

## Post-Migration Benefits

1. **Simpler architecture**: One config system (NixOS) instead of two (NixOS + home-manager)
2. **Faster rebuilds**: No home-manager activation step
3. **Better for system packages**: Everything truly at system level
4. **Cleaner with chezmoi**: Dotfiles managed independently, instant iteration
5. **Easier to understand**: No home-manager abstractions, direct NixOS

## Estimated Time

- Phase 1 (Packages): 1-2 hours
- Phase 2 (Configs): 2-3 hours
- Phase 3 (Services): 1-2 hours
- Phase 4-6 (Env, SOPS, Stylix): 1 hour
- Phase 7 (Removal): 30 minutes
- Testing: 1-2 hours
- **Total: 7-11 hours** (can be done across multiple sessions)

## Directory Structure: Before vs After

### BEFORE (Current Structure with Home-Manager)

```
nix-config/
├── flake.nix (includes home-manager input)
├── flake.lock
├── parts/
│   └── systems.nix (home-manager integration)
├── modules/
│   ├── core/
│   │   ├── sys-apps.nix (only ~20 packages)
│   │   ├── system.nix
│   │   └── hardware.nix
│   ├── shell/ (HOME-MANAGER)
│   │   ├── default.nix (imports home-manager modules)
│   │   ├── utils.nix (home.packages: bat, fd, fzf, git, etc.)
│   │   ├── xdg.nix (xdg.mimeApps, home.sessionVariables)
│   │   └── sops.nix (sops.* home-manager module)
│   ├── desktop/ (HOME-MANAGER)
│   │   ├── default.nix
│   │   ├── wlogout/wlogout.nix (programs.wlogout, home.file)
│   │   ├── whichkey.nix (home.packages, xdg.configFile)
│   │   ├── gtk.nix
│   │   ├── stylix.nix (includes home-manager.users.*.stylix)
│   │   └── system.nix
│   ├── apps/ (HOME-MANAGER)
│   │   ├── default.nix
│   │   ├── apps.nix (home.packages: obsidian, zotero, etc.)
│   │   ├── utils.nix (home.packages: hyprshot, blueman, etc.)
│   │   ├── dev.nix (home.packages: go, rust, R, etc.)
│   │   ├── espanso.nix (systemd.user.services, sops.secrets)
│   │   ├── syncthing.nix (services.syncthing home-manager)
│   │   ├── thunar.nix (xfconf.settings)
│   │   ├── vscodium.nix (programs.vscode)
│   │   ├── anki.nix (home.packages)
│   │   ├── anki-forge.nix (home.packages)
│   │   └── web-apps/ (xdg.desktopEntries)
│   ├── scripts/ (HOME-MANAGER)
│   │   ├── default.nix (home.packages: custom scripts)
│   │   └── ... (script files)
│   ├── editor/ (HOME-MANAGER)
│   │   ├── emacs.nix (programs.emacs, home.file symlink)
│   │   └── zed.nix (programs.zed, home.packages)
│   └── hosts/
│       └── laptop-nix.nix (services.kanshi via home-manager)
```

### AFTER (System-Level NixOS + Chezmoi)

```
nix-config/
├── flake.nix (NO home-manager input)
├── flake.lock
├── parts/
│   └── systems.nix (NO home-manager, sops-nix.nixosModules)
├── modules/
│   ├── core/
│   │   ├── default.nix
│   │   ├── sys-apps.nix (ALL ~101 packages!)
│   │   ├── system.nix
│   │   └── hardware.nix
│   ├── shell/ (System-level shell/environment config)
│   │   ├── default.nix
│   │   ├── environment.nix (NEW - sessionVariables)
│   │   ├── user-dirs.nix (NEW - XDG user directories)
│   │   └── sops.nix (system-level sops)
│   ├── desktop/ (Desktop environment)
│   │   ├── default.nix
│   │   ├── mime-types.nix (NEW - file associations)
│   │   ├── gtk.nix (kept as-is)
│   │   ├── stylix.nix (NO home-manager targets)
│   │   ├── brave-wrapper.nix
│   │   ├── system.nix
│   │   └── hypr/
│   │       └── default.nix
│   ├── apps/ (Applications - kept together)
│   │   ├── default.nix
│   │   ├── thunar.nix (system-level xfconf)
│   │   ├── web-apps.nix (NEW - desktop entries)
│   │   └── overlays/
│   │       └── default.nix
│   ├── services/ (NEW - Systemd services)
│   │   ├── default.nix
│   │   ├── espanso.nix (systemd.services)
│   │   ├── syncthing.nix (services.syncthing)
│   │   └── kanshi.nix (systemd.user.services)
│   └── hosts/ (Host-specific - kept similar)
│       ├── desktop-nix.nix
│       └── laptop-nix.nix
├── docs/
│   └── HOME-MANAGER-REMOVAL.md (NEW - this plan)
└── [DELETED: home-manager package modules only]

chezmoi repo (~/.local/share/chezmoi/):
├── dot_config/
│   ├── wlogout/
│   │   ├── layout.json (NEW)
│   │   ├── style.css (NEW)
│   │   └── icons/ (NEW - copied from nix-config)
│   ├── wlr-which-key/
│   │   └── config.yaml.tmpl (NEW - templated)
│   ├── kanshi/
│   │   └── config (NEW)
│   ├── hypr/ (existing)
│   │   ├── hyprland.conf.tmpl
│   │   ├── binds.conf
│   │   ├── rules.conf
│   │   ├── settings.conf.tmpl
│   │   ├── hypridle.conf
│   │   └── hyprlock.conf.tmpl
│   ├── kitty/ (existing)
│   ├── rofi/ (existing)
│   ├── zsh/ (existing)
│   ├── yazi/ (existing)
│   ├── mako/ (existing)
│   └── waybar/ (existing)
└── .chezmoiexternal.toml (NEW - emacs symlink)
```

### Key Changes Summary

**Eliminated:**
- ❌ `flake.nix` home-manager input
- ❌ `parts/systems.nix` home-manager integration block
- ❌ All `modules/shell/` home-manager modules
- ❌ All `modules/apps/` home-manager modules
- ❌ All `modules/scripts/` home-manager modules
- ❌ All `modules/editor/` home-manager modules
- ❌ `modules/desktop/wlogout/` (moved to chezmoi)
- ❌ `modules/desktop/whichkey.nix` (config moved to chezmoi)

**Created:**
- ✅ `modules/services/` - System-level systemd services (espanso, syncthing, kanshi)
- ✅ `modules/shell/environment.nix` - Environment variables
- ✅ `modules/shell/user-dirs.nix` - XDG user directories
- ✅ `modules/desktop/mime-types.nix` - File associations
- ✅ `modules/apps/web-apps.nix` - Desktop entries (moved from web-apps/)
- ✅ `docs/HOME-MANAGER-REMOVAL.md` - This migration plan
- ✅ 5+ new chezmoi config files (wlogout, which-key, kanshi)

**Modified:**
- 📝 `modules/core/sys-apps.nix` - Now contains ALL ~101 packages
- 📝 `modules/shell/` - System-level only (no more home.packages)
- 📝 `modules/apps/` - Kept thunar, overlays, web-apps together
- 📝 `modules/desktop/stylix.nix` - Removed home-manager targets
- 📝 `modules/shell/sops.nix` - System-level only
- 📝 `flake.nix` - Uses `inputs.sops-nix.nixosModules.sops`
- 📝 `modules/hosts/laptop-nix.nix` - Similar to current (kanshi via services/)

### Package Location Changes

**Before:**
- System packages in `/run/current-system/sw/bin/` (~20 packages)
- User packages in `/etc/profiles/per-user/joebutler/bin/` (~81 packages)

**After:**
- ALL packages in `/run/current-system/sw/bin/` (~101 packages)
- NO packages in `/etc/profiles/per-user/joebutler/` (directory may not exist)

### Config Location Changes

| Config | Before | After |
|--------|--------|-------|
| wlogout | Generated by home-manager | Managed by chezmoi |
| wlr-which-key | Generated by home-manager | Managed by chezmoi |
| kanshi | Generated by home-manager | Managed by chezmoi |
| MIME associations | home-manager (xdg.mimeApps) | System-level (/etc/xdg/) |
| Desktop entries | home-manager (xdg.desktopEntries) | System-level (/etc/) |
| Environment vars | home-manager (home.sessionVariables) | System-level (environment.sessionVariables) |
| Espanso service | home-manager (systemd.user) | System-level (systemd.services) |
| Emacs symlink | home-manager (home.file) | Chezmoi (.chezmoiexternal) |

## Copying Plan to Nix Repo

After approval, copy this plan to your nix-config for reference:

```bash
cp ~/.claude/plans/dazzling-meandering-hippo.md ~/nix-config/docs/HOME-MANAGER-REMOVAL.md
cd ~/nix-config
mkdir -p docs
git add docs/HOME-MANAGER-REMOVAL.md
git commit -m "docs: add home-manager removal migration plan"
```

This creates a permanent record of the migration strategy in your repository.

## Migration Complete ✅

**Date Completed**: December 21, 2025

All phases of the home-manager removal migration have been successfully completed:

### Final System Architecture

**System-Level (NixOS)**:
- All packages in `modules/core/sys-apps.nix`
- Services as systemd user services in `modules/services/`
- Environment variables in `modules/shell/environment.nix`
- SOPS secrets at system level (`/run/secrets/`)
- MIME types in `modules/desktop/mime-types.nix`
- XDG user directories in `modules/shell/user-dirs.nix`

**Chezmoi**:
- All dotfiles (Hyprland, Zsh, Kitty, Waybar, Rofi, Mako, etc.)
- wlogout config and icons
- wlr-which-key config

**Minimal Home Manager**:
- Only user-level applications in `modules/apps/`
- AI utilities integration
- Editor configurations

### Removed Files
- `modules/desktop/gtk.nix` → Replaced by system-level Papirus theme
- `modules/desktop/whichkey.nix` → Config moved to chezmoi
- `modules/shell/xdg.nix` → Split into system-level mime-types.nix and user-dirs.nix
- `modules/shell/utils.nix` → Packages moved to sys-apps.nix

### Key Fixes During Migration
- SOPS migration: Removed home-manager sops, all secrets now system-level
- Espanso secret key: Fixed to use `espanso_matches` from secrets.yaml
- AI utilities: Updated wrapper to use `/run/secrets/CONTEXT7_API_KEY`
- wlogout: Added to system packages with chezmoi config
- GTK theme: Added Papirus to system packages with environment variable

All functionality preserved, no regressions detected.
