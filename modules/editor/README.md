# Editor Module - Emacs Configuration

Minimal, performance-optimized Emacs configuration based on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) principles, integrated with Nix.

## Philosophy

- **Start Minimal**
- **Build Incrementally**: Add features as needed, not speculatively.
- **Performance**: Sub-second startup required
- **Nix-First**: Declarative package management via Home Manager + emacs-overlay.

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

**Hybrid Approach**:
1.  **Nix (`emacs.nix`)**: Installs packages (declarative, reproducible).
2.  **Elisp (`emacs.d/post-init.el`)**: Configures packages (lazy loading, key bindings).

### How to Add a New Package

1.  **Install via Nix**:
    Add the package to `extraPackages` in `modules/editor/emacs.nix`:
    ```nix
    extraPackages = epkgs: with epkgs; [
      # ... existing packages
      new-package
    ];
    ```

2.  **Configure via Elisp**:
    Add the configuration to `modules/editor/emacs.d/post-init.el`:
    ```elisp
    (use-package new-package
      :defer t                    ; Lazy load
      :hook (mode-name . setup)   ; Load on mode activation
      :config
      ;; Configuration here
      )
    ```

3.  **Apply Changes**:
    Rebuild your NixOS/Home Manager configuration:
    ```bash
    home-manager switch --flake ~/nix-config
    ```

## File Structure

```
/modules/editor/
├── default.nix              # Module aggregator
├── emacs.nix                # Nix config (programs.emacs + packages)
├── README.md                # This file
└── emacs.d/                 # Emacs Lisp configuration
    ├── early-init.el        # Core performance foundation (DO NOT EDIT)
    ├── init.el              # Entry point & defaults (DO NOT EDIT)
    ├── post-init.el         # User configuration (Evil, Theme, Packages)
    └── pre-early-init.el    # Early customizations (e.g. paths)
```

> **Note:** `init.el` and `early-init.el` are managed core files. All user customizations should go into `post-init.el`.

## Troubleshooting

### Slow Startup
```bash
# Profile startup
emacs --debug-init

# Check init time within Emacs
M-x emacs-init-time
```

### Package Issues
```bash
# Check package availability in Nix
nix search nixpkgs#emacsPackages.<package-name>
```

### Native Compilation
```bash
# Check compilation status within Emacs
M-x native-compile-async-query

# Force recompilation (if needed)
rm -rf ~/.emacs.d/eln-cache
```

## References

- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) - The upstream configuration base.
- [emacs-overlay](https://github.com/nix-community/emacs-overlay) - Nix package source.
- [Evil Documentation](https://evil.readthedocs.io/) - Vim emulation.
- [use-package](https://github.com/jwiegley/use-package) - Package configuration macro.
- [Doom Emacs Modules](https://github.com/doomemacs/doomemacs/tree/master/modules) - Reference implementation.
- [General.el](https://github.com/noctuid/general.el) - Keybinding framework.
- [Vertico](https://github.com/minad/vertico) - Vertical completion.
- [gptel](https://github.com/karthink/gptel) - LLM integration.
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/) - Language Server Protocol.
- [Magit](https://magit.vc/) - Git porcelain.
- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) - Incremental parsing.
