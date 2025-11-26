# Agent-Specific Documentation

This document outlines configurations and conventions relevant to automated agents interacting with this repository.

## Pre-Commit Hooks

This repository utilizes `pre-commit` hooks to enforce code quality and formatting standards automatically. These hooks run before each commit, ensuring that your code adheres to project guidelines.

### Enabled Hooks:

*   **`nixpkgs-fmt`**: Automatically formats all `.nix` files according to `nixpkgs` style guidelines. If formatting changes are made by this hook, the commit will be aborted, requiring you to `git add` the formatted changes before retrying the commit.
*   **`check-yaml`**: Checks YAML files for syntax errors.
*   **`end-of-file-fixer`**: Ensures that files end with a newline and are not unnecessarily long.
*   **`trailing-whitespace`**: Trims trailing whitespace from lines.

### Usage:

When attempting to commit changes, `pre-commit` will automatically execute these checks. If any hook modifies a file or reports an error, the commit will be blocked. You will need to stage any changes made by the hooks (e.g., after `nixpkgs-fmt` reformats a file) and then retry your commit.

### Installation and Management:

The `pre-commit` framework is installed system-wide via NixOS. The hooks are set up by running `pre-commit install` in the repository root. This command creates hooks in your `.git/hooks` directory.

## Git & Flakes Workflow

**Crucial Rule:** When using Nix Flakes, the `nixos-rebuild` command **only sees files that are tracked by Git**.

1.  **New Files:** If you create a new file (e.g., `modules/new-service.nix`), you **MUST** stage it (`git add`) before running `nixos-rebuild` (aliases `ns`, `nsdry`). If you do not, Nix will report that the file does not exist.
2.  **Best Practice:** Always commit your changes (or at least stage them) before applying the configuration. This ensures the system state matches the version control history.
