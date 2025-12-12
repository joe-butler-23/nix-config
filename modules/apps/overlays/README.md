# Custom Nixpkgs Overlays

This directory contains custom Nixpkgs overlays used in this configuration. Overlays allow for modifying existing packages, adding new packages, or overriding package versions from `nixpkgs` or `nixpkgs-unstable`.

## OpenCode Overlay (`opencode`)

### Problem Statement

The `opencode` package available in standard `nixpkgs` is significantly outdated, and direct integration of the upstream `opencode` flake (`github:sst/opencode`) resulted in build failures related to `bun` dependency resolution (`catalog:` protocol errors). The goal was to provide the latest functional version of `opencode`.

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

### How to Update `opencode` (Agent Instructions)

To update `opencode`, you must follow this procedure:

1.  **Get Latest Version**:
    ```bash
    curl -s https://api.github.com/repos/sst/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//'
    ```
2.  **Get New Hash**:
    Run `nix-prefetch-url` with the new version number found above:
    ```bash
    nix-prefetch-url "https://github.com/sst/opencode/releases/download/v<VERSION>/opencode-linux-x64.tar.gz"
    ```
3.  **Update File**:
    Edit `modules/apps/overlays/default.nix`:
    *   Update `version = "..."` with the new version.
    *   Update `sha256 = "..."` in the `opencode` section with the new hash.

## Gemini CLI Overlay (`gemini-cli`)

### Problem Statement

The `gemini-cli` package in `nixpkgs` is often behind the rapid release cycle of the official GitHub repository. The goal was to provide the latest version directly from the official binary/script releases.

### Approach and Solution

*   **Source**: Fetches the pre-bundled `gemini.js` executable from the official `google-gemini/gemini-cli` GitHub Releases.
*   **Wrapper**: Wraps the JavaScript file with the system's `nodejs` runtime using `makeWrapper` to create an executable `gemini` command.
*   **Automation**: Includes an `updateScript` to automate version bumping.

### How to Use

The `gemini-cli` package is now available through `pkgsUnstable`.

## Codex Overlay (`codex`)

### Description

Provides the `codex` CLI tool from the `openai/codex` repository. It fetches the pre-built Linux binary from GitHub Releases.

### Implementation

*   Fetches `codex-x86_64-unknown-linux-gnu.tar.gz` from GitHub Releases.
*   Installs the binary to `$out/bin/codex`.
*   Uses `autoPatchelfHook` for dynamic linking on NixOS.

### How to Use

The `codex` package is now available through `pkgs`.

### How to Update Overlays

To update these packages, follow this procedure:

1.  **Get Latest Versions**:
    ```bash
    curl -s https://api.github.com/repos/google-gemini/gemini-cli/releases/latest | jq -r '.tag_name' | sed 's/^v//'
    curl -s https://api.github.com/repos/sst/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//'
    curl -s https://api.github.com/repos/openai/codex/releases/latest | jq -r '.tag_name' | sed 's/^rust-v//'
    ```
2.  **Get New Hash**:
    Run `nix-prefetch-url` with the new version number found above:
    ```bash
    nix-prefetch-url "https://github.com/google-gemini/gemini-cli/releases/download/v<VERSION>/gemini.js"
    nix-prefetch-url "https://github.com/sst/opencode/releases/download/v<VERSION>/opencode-linux-x64.tar.gz"
    nix-prefetch-url "https://github.com/openai/codex/releases/download/rust-v<VERSION>/codex-x86_64-unknown-linux-gnu.tar.gz"
    ```
3.  **Update Files**:
    Edit `modules/apps/overlays/default.nix`:
    *   Update `version = "..."` with the new version.
    *   Update `sha256 = "..."` in the respective section with the new hash.
