# Code Style & Conventions

## General
*   **Language**: Nix
*   **Format**: Code must be formatted using the standard formatter defined in the flake, triggered via `nix fmt`.
*   **Modularity**: Prefer small, focused modules imported via `default.nix` or explicit imports lists.

## Flake Structure
*   Use `flake-parts` for the `flake.nix` structure.
*   Define systems in `parts/systems.nix`.
*   Use `specialArgs` to pass flake inputs to modules.

## NixOS & Home Manager
*   **Integration**: Home Manager is integrated as a NixOS module. **Do not** use standalone Home Manager commands (`home-manager switch`).
*   **Management**: Manage the entire system state via `nixos-rebuild`.

## AI Agents
*   **Context**: Always check `AGENTS.md` before major operations.
*   **Task Tracking**: Use `beads` (`bd`) for all task state.
*   **Tools**: Prefer `serena` tools for file operations to ensure context awareness.
