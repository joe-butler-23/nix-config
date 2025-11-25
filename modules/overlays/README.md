# Custom Nixpkgs Overlays

This directory contains custom Nixpkgs overlays used in this configuration. Overlays allow for modifying existing packages, adding new packages, or overriding package versions from `nixpkgs` or `nixpkgs-unstable`.

## OpenCode Overlay (`opencode`)

### Problem Statement

The `opencode` package available in standard `nixpkgs` was significantly outdated, and direct integration of the upstream `opencode` flake (`github:sst/opencode`) resulted in build failures related to `bun` dependency resolution (`catalog:` protocol errors). The goal was to provide the latest functional version of `opencode`.

### Approach and Solution

1.  **Initial Attempt (Upstream Flake)**:
    *   The first attempt was to integrate `github:sst/opencode` as a flake input and expose it via an overlay.
    *   **Result**: This failed during `nixos-rebuild dry-build` due to `bun`'s dependency resolution mechanisms conflicting with the Nix build sandbox, specifically `error: @cloudflare/workers-types@catalog: failed to resolve`.

2.  **Second Attempt (Binary Release)**:
    *   Given the upstream flake's build issues, the next approach was to use the pre-built binary releases provided by the `sst/opencode` GitHub repository.
    *   The `opencode-linux-x64.tar.gz` artifact was fetched using `fetchurl`.
    *   **Initial Result**: When run, the installed `opencode` binary behaved like a generic `bun` runtime, displaying `bun`'s help message instead of the `opencode` CLI. This was because the Nix build process automatically stripped the binary, removing the embedded JavaScript code required for its functionality.
    *   **Solution**: The derivation was modified to include `dontStrip = true;`. This prevents Nix from stripping the binary, preserving the embedded JavaScript and allowing the application to execute correctly. `autoPatchelfHook` was used to ensure dynamic library paths are correctly resolved on NixOS.

### Current Implementation

The `opencode` overlay (`modules/overlays/default.nix`) now defines a custom derivation that:
*   Fetches the latest `opencode-linux-x64.tar.gz` binary release from the official GitHub repository.
*   Disables binary stripping (`dontStrip = true;`) to preserve the embedded JavaScript application code.
*   Utilizes `autoPatchelfHook` to ensure proper dynamic linking on NixOS.
*   Includes an `updateScript` in its `passthru` attribute for easy automation of version updates.

### How to Use

The `opencode` package is now available through `pkgsUnstable` (as defined in `flake.nix` and `modules/home/packages.nix`). Simply reference `pkgsUnstable.opencode` where needed in your Home Manager or NixOS configurations.

### How to Update `opencode`

To update `opencode` to its latest binary release, you can run the following command, which utilizes the `updateScript` provided in the overlay:

```bash
# This command needs `nix-update` or similar shell environment setup
nix-shell -p nix-update --run "nix eval --raw '.#overlays.opencode.passthru.updateScript' | bash"
# Alternatively, if nix-update is not set up, run the script directly:
nix eval --raw '(import ./. { }).opencode.passthru.updateScript' | bash
```

This script will automatically fetch the latest release version and its corresponding `sha256` hash, then update `modules/overlays/default.nix` accordingly. You will then need to commit the changes and apply the new configuration (`sudo nixos-rebuild switch --flake .`).

## Gemini CLI Overlay (`gemini-cli`)

### Problem Statement

The `gemini-cli` package in `nixpkgs` is often behind the rapid release cycle of the official GitHub repository. The goal was to provide the latest version directly from the official binary/script releases.

### Approach and Solution

*   **Source**: Fetches the pre-bundled `gemini.js` executable from the official `google-gemini/gemini-cli` GitHub Releases.
*   **Wrapper**: Wraps the JavaScript file with the system's `nodejs` runtime using `makeWrapper` to create an executable `gemini` command.
*   **Automation**: Includes an `updateScript` to automate version bumping.

### How to Use

The `gemini-cli` package is available through `pkgsUnstable`.

### How to Update `gemini-cli`

To update `gemini-cli` to the latest release:

```bash
nix-shell -p nix-update --run "nix eval --raw '.#overlays.gemini-cli.passthru.updateScript' | bash"
```
