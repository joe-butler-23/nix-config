---
name: local-development
description: This details the user's local development environment on NixOS with ZSH/P10K shell environment, running Hyprland window manager, and Kitty terminal. Helps with troubleshooting of command not found errors, script failures, path issues, environment setup, shell configuration, and desktop workflow problems. Uses progressive disclosure for detailed troubleshooting.
---

# Local Development Environment

Quick fixes for common development issues on your NixOS + ZSH + Hyprland + Kitty setup.

## Instant Fixes

### Command Not Found
```bash
# Check what's available
which python3          # Should show /nix/store/... path
echo $PATH             # Verify binary in PATH
nix search nixpkgs python3  # Find package name

# Quick package additions
nix shell nixpkgs#curl     # Temporary install
nix shell nixpkgs#jq       # JSON processing tool
nix shell nixpkgs#git      # Version control
```

### Script Fails
```bash
#!/usr/bin/env bash        # ✅ Always use env on NixOS
#!/bin/bash                # ❌ Fails on NixOS

# For Python scripts
#!/usr/bin/env python3     # ✅ Works everywhere
```

### Path Issues
```bash
# Use relative paths, not FHS assumptions
./tools/script.sh          # ✅ Relative
$HOME/.local/bin/tool      # ✅ Environment variable
$(dirname "$0")/helper.sh  # ✅ Script-relative
```

### Environment Variables
```bash
# In flake.nix devShell:
shellHook = ''
  export API_KEY="dev-key"
  if [ -f .env ]; then source .env; fi
'';
```

### Terminal Issues
```bash
# Kitty-specific fixes
kitty +kitten ssh host     # Better SSH experience
kitty +kitten themes       # Change themes quickly

# ZSH + P10K issues
p10k configure             # Reconfigure prompt
exec zsh                   # Reload shell
```

## When You Need More Help

**NixOS Specific Issues:**
- Package equivalents and flake patterns → [references/nix-gotchas.md](references/nix-gotchas.md)

**Shell & Terminal Problems:**
- ZSH configuration, P10K tweaks, Kitty setup → [references/shell-environment.md](references/shell-environment.md)

**Hyprland & Desktop Issues:**
- Window management, keybindings, display setup → [references/desktop-environment.md](references/desktop-environment.md)

**Development Toolchain:**
- Editor setup, Git config, language tools → [references/development-tools.md](references/development-tools.md)

## Common Trigger Patterns

If you encounter:
- "command not found" → Check nix-gotchas.md
- Shell behavior issues → Check shell-environment.md
- Window/display problems → Check desktop-environment.md
- Tool configuration → Check development-tools.md
