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
