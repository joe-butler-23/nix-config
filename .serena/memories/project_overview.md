# Project Overview

## Purpose
This repository contains the **NixOS configuration** for user `joebutler`. It manages both system-level (NixOS) and user-level (Home Manager) configurations in a unified, declarative manner using **Nix Flakes**.

## Tech Stack
*   **Core**: NixOS 25.05 (unstable/experimental features enabled)
*   **Framework**: `flake-parts` for modular flake structure.
*   **Styling**: `stylix` for system-wide theming.
*   **Secrets**: `sops-nix` for encrypted secret management.
*   **AI**: `beads` (task tracking), `openskills` (procedures), and `opencode` (agent config).

## Architecture
The repository follows a modular structure:
*   `flake.nix`: The entry point, defining inputs and outputs using `flake-parts`.
*   `modules/`: Reusable Nix modules.
    *   `core/`: System-level (boot, hardware, basic pkgs).
    *   `desktop/`: UI configuration (Hyprland, Waybar, etc.).
    *   `apps/`: User applications.
    *   `hosts/`: Machine-specific entry points (e.g., `laptop-nix`, `desktop-nix`).
*   `parts/`: Flake-parts logic (systems, formatting, devshells).
*   `secrets/`: SOPS encrypted secrets.
