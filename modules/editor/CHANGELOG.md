# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Magit (Git integration)
- Undo System (`undo-fu` for linear undo/redo, `undo-fu-session` for persistence)
- Doom Modeline (`doom-modeline`, `nerd-icons` for attractive modeline)
- GCMH (Garbage Collector Magic Hack for runtime performance)
- Leader Key System (`general.el` for `SPC` leader)
- Completion Framework (`vertico` stack: `vertico`, `orderless`, `consult`, `marginalia`, `embark`, `embark-consult`)
- Code Intelligence (LSP via `eglot`, Tree-sitter via `treesit-auto`)
- Terminal Emulation (`vterm`)

### Changed
- `README.md`: Updated to accurately reflect current features, file structure, and package management approach.
- `emacs.nix`: Removed and re-added `gcmh` in the process; added new packages.

### Removed
- `TODO.md`: Replaced by Org-mode for task tracking.
