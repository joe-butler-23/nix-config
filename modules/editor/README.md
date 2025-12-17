# Editor Module - Emacs Configuration

Minimal, performance-optimized Emacs configuration based on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) principles.

## Philosophy

- **Start Minimal**
- **Build Incrementally**: Add features as needed, not speculatively.
- **Performance**: Sub-second startup required
- **Fast Iteration**: Package management via straight.el for instant updates without system rebuilds.

## Current Features

### Performance
- **Expected startup**: < 1 second cold start.

### Evil Mode (Vim Emulation)
- Complete Vim modal editing (normal, insert, visual, operator-pending modes).
- **Evil Collection**: Vim bindings for 50+ built-in modes.
- **Evil Surround**: Surround text objects (cs, ds, ys commands).
- Visual line navigation (j/k move by screen line).
- **Undo System**: Linear undo/redo with `undo-fu` and persistent history.

### Keybindings & Navigation
- **Leader Key**: Spacemacs/Doom-style `SPC` leader system via `general.el`.
- **Discovery**: `which-key` shows available key bindings after partial key sequence.
- **Completion**: Modern "Vertico Stack" for fast, fuzzy vertical completion:
    - `vertico` (UI), `orderless` (Fuzzy matching), `consult` (Enhanced commands), `marginalia` (Annotations), `embark` (Actions).

### Code Intelligence
- **LSP**: Built-in `eglot` for "Go to definition", renaming, and error highlighting.
- **Tree-sitter**: Fast, accurate syntax highlighting via `treesit-auto`.
- **Version Control**: `magit` for Git integration.

### Theme & UI
- **doom-nord**: Nord color scheme matching system theme.
- **Doom Modeline**: Minimal, informative modeline with icons (`nerd-icons`).
- **Minimal chrome**: No toolbars, menubars, or scrollbars.
- **Terminal**: Integrated `vterm` for full terminal emulation.
- **Editor**: Relative line numbers, current line highlighting.

## Package Management

Packages are managed by [straight.el](https://github.com/radian-software/straight.el), which provides:
- **Reproducible builds**: Packages pinned to specific commits
- **Git-based**: Clones packages directly from source repositories
- **Fast iteration**: Add/update packages without system rebuilds
- **Lockfile**: `~/.emacs.d/straight/versions/` tracks exact versions

### How to Add a New Package

1.  **Add to `post-init.el`**:
    Simply add a `use-package` declaration to `modules/editor/emacs.d/post-init.el`:
    ```elisp
    (use-package new-package
      :defer t                    ; Lazy load
      :hook (mode-name . setup)   ; Load on mode activation
      :config
      ;; Configuration here
      )
    ```

2.  **Restart Emacs**:
    straight.el will automatically clone and build the package on next launch.

3.  **For GitHub packages**:
    Specify the repository explicitly:
    ```elisp
    (use-package new-package
      :straight (:host github :repo "user/repo")
      :config
      ;; Configuration here
      )
    ```

### Package Updates

```elisp
;; Update a specific package
M-x straight-pull-package RET package-name RET

;; Update all packages
M-x straight-pull-all

;; Rebuild a package after updates
M-x straight-rebuild-package RET package-name RET
```

## File Structure

```
/modules/editor/
├── default.nix              # Module aggregator
├── emacs.nix                # Nix config (installs Emacs binary only)
├── README.md                # This file
└── emacs.d/                 # Emacs Lisp configuration
    ├── early-init.el        # Core performance foundation (DO NOT EDIT)
    ├── init.el              # Entry point & defaults (DO NOT EDIT)
    ├── post-init.el         # User config (straight.el bootstrap, packages, theme)
    ├── bindings.el          # Keybindings (SPC leader + general.el)
    └── pre-early-init.el    # Early customizations (e.g. paths)
```

> **Note:** `init.el` and `early-init.el` are managed core files. All user customizations should go into `post-init.el` and `bindings.el`.

## Troubleshooting

### Slow Startup
```bash
# Profile startup
emacs --debug-init

# Check init time within Emacs
M-x emacs-init-time
```

### Package Issues
```elisp
;; Check if straight.el loaded a package
M-x straight-get-recipe RET package-name RET

;; Force rebuild a package
M-x straight-rebuild-package RET package-name RET

;; Check straight.el build directory
M-x straight-check-all

;; Reset straight.el completely (nuclear option)
;; Delete ~/.emacs.d/straight/ and restart Emacs
```

### Native Compilation
```bash
# Check compilation status within Emacs
M-x native-compile-async-query

# Force recompilation (if needed)
rm -rf ~/.emacs.d/eln-cache
```

### First Launch
On first launch, straight.el will clone and build all packages (~1-2 minutes). Subsequent launches are instant. You'll see messages like:
```
Bootstrapping straight.el...
Building evil...
Building magit...
```
This is normal and only happens once.

## References

- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) - The upstream configuration base.
- [straight.el](https://github.com/radian-software/straight.el) - Package manager.
- [Evil Documentation](https://evil.readthedocs.io/) - Vim emulation.
- [use-package](https://github.com/jwiegley/use-package) - Package configuration macro.
- [Doom Emacs Modules](https://github.com/doomemacs/doomemacs/tree/master/modules) - Reference implementation.
- [General.el](https://github.com/noctuid/general.el) - Keybinding framework.
- [Vertico](https://github.com/minad/vertico) - Vertical completion.
- [Magit](https://magit.vc/) - Git porcelain.
- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) - Incremental parsing.
