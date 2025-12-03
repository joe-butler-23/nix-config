# NixOS Troubleshooting Patterns

## Common Configuration Issues

### Syntax Highlighting Not Working

**Symptoms:**
- Text editor not highlighting language syntax despite packages being installed
- Language definitions present but not being recognized

**Debugging Steps:**
1. Check if application already links to required libraries: `nix-store --query --requisites $(which app) | grep library`
2. Verify user config isn't overriding system defaults: `ls -la ~/.local/share/*/language-specs/`
3. Compare user vs system language definitions for conflicts
4. Remove user overrides to test system defaults first

**Common Cause:** User Home Manager configs overriding working system defaults with incompatible versions.

### Package Integration Issues

**Symptoms:**
- Package installed but features not working
- Missing dependencies despite package being present

**Debugging Steps:**
1. Check package dependencies: `nix-store --query --requisites $(which package)`
2. Verify system-wide vs user-specific package conflicts
3. Check if package needs system-level integration vs just installation

**Common Cause:** Assuming packages need explicit addition when system already provides integration.

### Type Errors in Configuration

**Symptoms:**
- Build failures with type mismatch errors
- Options expecting different data types than provided

**Prevention:**
1. Check option type before implementation: `nixos-option path.to.option`
2. Reference NixOS manual for option specifications
3. Test incremental changes rather than multiple modifications at once

**Pattern:** Always verify option types in documentation before adding new configurations.

## Effective Debugging Sequence

1. **Verify Current State**: What's actually installed and working?
2. **Check Dependencies**: Are required libraries properly linked?
3. **Compare Configurations**: User vs system, expected vs actual
4. **Isolate Variables**: Test one change at a time
5. **Use System Defaults**: Try removing customizations first
6. **Incremental Addition**: Add complexity only after basics work

This sequence prevents assumption-based debugging and reduces iteration time.

## Verification Patterns

### Option Verification Strategy

**Problem:** Applying configuration snippets from web searches that contain hallucinated or deprecated NixOS options (e.g., `services.hardware.lm-sensors.enable`).

**The Trap:**
- "It looks correct" is not a valid validation.
- LLMs and older forum posts often invent options that *sound* plausible but don't exist.

**The Fix:**
1.  **Authoritative Search**: Use the [NixOS Options Search](https://search.nixos.org/options) or `man configuration.nix` locally.
2.  **Package vs Module**: If an option doesn't exist, the tool often just needs to be added to `environment.systemPackages` (e.g., `lm_sensors`, `htop`).
3.  **Schema Check**: If unsure, check the defining module in `nixpkgs` source to see available options.

**Rule:** never blindly apply a `services.*` or `programs.*` configuration without confirming the option key exists.
