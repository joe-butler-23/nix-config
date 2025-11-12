# Scripts Development Guide

This document outlines the standard process for creating new Nix-based scripts in this configuration.

## Overview

All scripts in this folder follow a consistent pattern using Nix's `writeShellScriptBin` to create self-contained executables with managed dependencies.

## Script Template

```nix
{ pkgs ? import <nixpkgs> {} }:

(pkgs.writeShellScriptBin "script-name" ''
  set -euo pipefail

  # Import dependencies from Nix
  # Example: ${pkgs.fzf}/bin/fzf, ${pkgs.bat}/bin/bat, etc.

  # Script logic here
  main() {
    # Main functionality
  }

  # Run main function
  main
'')
```

## Required Components

### 1. Nix Wrapper
- Use `pkgs.writeShellScriptBin` to create the executable
- Pass `pkgs` parameter for dependency management
- Use single quotes for the script content

### 2. Shell Best Practices
- Always start with `set -euo pipefail`
- Use `main()` function for organization
- Exit with appropriate codes (0 for success, 1 for errors)

### 3. Dependency Management
- Reference all external tools via Nix paths: `${pkgs.toolname}/bin/toolname`
- Common dependencies include:
  - `pkgs.fzf` - fuzzy finder
  - `pkgs.bat` - cat alternative with syntax highlighting
  - `pkgs.wl-clipboard` - Wayland clipboard tools
  - `pkgs.xclip` - X11 clipboard fallback
  - `pkgs.foot` - terminal emulator (if needed)

### 4. Terminal Integration
- For scripts launched from which-key, use Foot terminal:
  ```yaml
  cmd: foot -a appname -e script-name
  ```
- Use descriptive app names for Foot class identification

## Integration Steps

### 1. Create the Script File
- Name it `script-name.nix` (kebab-case)
- Follow the template above
- Test locally with `nix-build` if needed

### 2. Update scripts/default.nix
Add your script to the packages list:
```nix
{ pkgs, ... }: {
  home.packages = [
    (import ./file-launcher.nix { inherit pkgs; })
    (import ./recent-files-launcher.nix { inherit pkgs; })
    (import ./script-name.nix { inherit pkgs; })  # Add this line
  ];
}
```

### 3. Update which-key Configuration
Add to `/modules/home/dotfiles/whichkey.nix`:
```yaml
# category
- key: "x"
  desc: category
  submenu:
    - key: "s"
      desc: script description
      cmd: foot -a appname -e script-name
```

### 4. Apply Changes
```bash
cd /home/joebutler/nix-config
home-manager switch --flake .
```

### 5. Test and Commit
- Test the script works: `which script-name`
- Test which-key integration
- Commit changes with descriptive message

## Common Patterns

### FZF Integration
```bash
# Colors for consistent theming
COLORS=(
  --color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796
  --color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6
  --color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796
)

# FZF command with standard options
selected=$(${pkgs.fzf}/bin/fzf \
  --layout=reverse \
  --height="80%" \
  --border=rounded \
  --pointer="▌" \
  --marker="%" \
  "${COLORS[@]}") || exit 0
```

### Clipboard Operations
```bash
# Wayland-first, X11 fallback
CLIP_CMD=""
if command -v ${pkgs.wl-clipboard}/bin/wl-copy >/dev/null 2>&1; then
  CLIP_CMD="${pkgs.wl-clipboard}/bin/wl-copy"
elif command -v ${pkgs.xclip}/bin/xclip >/dev/null 2>&1; then
  CLIP_CMD="${pkgs.xclip}/bin/xclip -selection clipboard"
else
  echo "Error: No clipboard tool available." >&2
  exit 1
fi

# Usage
echo "content" | $CLIP_CMD
```

### File Operations
```bash
# Check file existence and readability
if [[ -f "$filepath" && -r "$filepath" ]]; then
  # Process file
fi

# MIME type detection
mime="$(${pkgs.file}/bin/file --mime-type -Lb "$filepath" 2>/dev/null || echo "")"
```

## Naming Conventions

- **Script files**: `kebab-case.nix`
- **Executable names**: `kebab-case`
- **Foot app names**: `descriptive-name` (lowercase, no spaces)
- **which-key descriptions**: `lowercase with spaces`

## Testing Checklist

- [ ] Script builds without errors
- [ ] Dependencies are correctly referenced
- [ ] Script executes when called directly
- [ ] which-key integration works
- [ ] Foot terminal launches correctly
- [ ] Error handling works (Ctrl+C, invalid input)
- [ ] No hardcoded paths outside home directory

## Examples

See existing scripts:
- `file-launcher.nix` - File selection and opening
- `recent-files-launcher.nix` - Recent files from GTK database
- `copy-prompt.nix` - Prompt library management with clipboard

## Troubleshooting

### Common Issues
1. **"command not found"** - Check Nix dependency paths
2. **"permission denied"** - Ensure script is executable via Nix
3. **"does not exist"** - Check file is added to scripts/default.nix
4. **Blank windows** - Remove Kitty-specific code, use Foot

### Debug Commands
```bash
# Check if script is available
which script-name

# Test script directly
script-name

# Check which-key config
cat ~/.config/wlr-which-key/config.yaml

# Rebuild if needed
cd ~/nix-config && home-manager switch --flake .
