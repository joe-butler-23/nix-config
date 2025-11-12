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

### 3. Apply Changes
```bash
cd /home/joebutler/nix-config
home-manager switch --flake .
```

### 4. Test and Commit
- Test the script works: `which script-name`
- Test which-key integration
- Commit changes to git with descriptive message

## Naming Conventions

- **Script files**: `kebab-case.nix`
- **Executable names**: `kebab-case`
- **Foot app names**: `descriptive-name` (lowercase, no spaces)

## Testing Checklist

- [ ] Script builds without errors
- [ ] Dependencies are correctly referenced
- [ ] Script executes when called directly
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
3. **"does not exist"** - Check file is added to scripts/default.nix and added to git tree
4. **Blank windows** - Remove Kitty-specific code, use Foot

### Debug Commands
```bash
# Check if script is available
which script-name

# Test script directly
script-name

# Rebuild if needed
cd ~/nix-config && home-manager switch --flake .
