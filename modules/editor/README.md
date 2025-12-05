# Editor Module - Emacs Configuration

Minimal, performance-optimized Emacs configuration incorporating Doom Emacs performance principles with a Nix approach as far as possible.

## Philosophy

- **Start Minimal**: Core performance + Evil mode only
- **Build Incrementally**: Add features as needed, not speculatively
- **Doom Performance**: Sub-second startup via lazy loading and GC optimization
- **Nix-First**: Declarative package management via Home Manager + emacs-overlay

## Current Features

### Performance
- **Early-init optimizations**: GC suppression, file handler optimization
- **GCMH**: Intelligent garbage collection (GC during idle, not typing)
- **Lazy loading**: All packages deferred by default via use-package
- **Native compilation**: Automatic via emacs-overlay
- **Expected startup**: < 1 second cold start

### Evil Mode (Vim Emulation)
- Complete Vim modal editing (normal, insert, visual, operator-pending modes)
- Evil Collection: Vim bindings for 50+ built-in modes
- Evil Surround: Surround text objects (cs, ds, ys commands)
- Visual line navigation (j/k move by screen line)

### Discovery
- **which-key**: Shows available key bindings after partial key sequence

### Theme
- **doom-nord**: Nord color scheme matching Kitty terminal
- Doom Emacs theme package with visual bell and org-mode support
- Dark background (#2e3440) with Nord palette
- Optimized for programming with syntax highlighting

### UI
- Minimal chrome (no toolbars, menubars, scrollbars)
- Relative line numbers in programming/text modes
- Current line highlighting
- Smooth pixel-precision scrolling (Wayland-native)

## Package Management

**Hybrid Approach**:
- **Nix**: Installs packages via emacs-overlay (declarative, reproducible)
- **use-package**: Configures packages (lazy loading, key bindings)

To add a new package:

1. Add to `emacs.nix` in `extraPackages`:
   ```nix
   extraPackages = epkgs: with epkgs; [
     existing-package
     new-package  # Add here
   ];
   ```

2. Configure in appropriate module file:
   ```elisp
   (use-package new-package
     :defer t                    ; Lazy load
     :hook (mode-name . setup)   ; Load on mode activation
     :config
     ;; Configuration here
     )
   ```

3. Rebuild: `home-manager switch --flake ~/nix-config`

## File Structure

```
/modules/editor/
├── default.nix              # Module aggregator
├── emacs.nix                # Nix config (programs.emacs + packages)
├── README.md                # This file
└── emacs.d/                 # Emacs Lisp configuration
    ├── early-init.el        # Performance foundation (runs before GUI)
    ├── init.el              # Entry point + use-package setup
    └── modules/
        ├── core-perf.el     # Runtime performance (GCMH, scrolling)
        ├── core-theme.el    # Nord theme (doom-nord)
        ├── core-ui.el       # Minimal UI + which-key
        └── core-evil.el     # Complete Evil mode setup
```

## Usage

### Building
```bash
# Home Manager rebuild
home-manager switch --flake ~/nix-config

# First launch (native compilation runs in background)
emacs
```
## Extending the Configuration

### Adding Language Support

Example: Adding Python support with LSP

1. **Add packages** (`emacs.nix`):
   ```nix
   extraPackages = epkgs: with epkgs; [
     # ... existing packages
     python-mode
     lsp-mode
     lsp-ui
   ];
   ```

2. **Create module** (`emacs.d/modules/lang-python.el`):
   ```elisp
   (use-package python-mode
     :defer t
     :mode "\\.py\\'"
     :hook (python-mode . lsp-deferred))

   (use-package lsp-mode
     :defer t
     :commands (lsp lsp-deferred)
     :config
     (setq lsp-idle-delay 0.5))
   ```

3. **Load in init.el**:
   ```elisp
   (load (concat user-emacs-directory "modules/lang-python.el"))
   ```

### Adding Completion Framework

Example: Adding Vertico + Orderless + Consult

1. Add packages to `emacs.nix`
2. Create `modules/completion-vertico.el`
3. Load in `init.el`

See Doom Emacs modules for reference patterns.

## Troubleshooting

### Slow Startup
```bash
# Profile startup
emacs --debug-init

# Check init time
M-x emacs-init-time
```

### Package Issues
```bash
# Rebuild after package changes
home-manager switch --flake ~/nix-config

# Check package availability
nix search nixpkgs#emacsPackages.<package-name>
```

### Native Compilation
```bash
# Check compilation status
M-x native-compile-async-query

# Force recompilation
rm -rf ~/.emacs.d/eln-cache
```

## Performance Benchmarks

Target metrics:
- **Cold startup**: < 1 second
- **Warm startup**: < 0.5 seconds
- **GC pauses**: < 50ms during editing
- **LSP latency**: < 100ms (with read-process-output-max optimization)

Measure with: `M-x emacs-init-time`

## Agent Instructions

When modifying this configuration:

1. **Add features incrementally** - Don't bulk-add packages speculatively
2. **Use lazy loading** - Always defer via `:defer`, `:hook`, `:commands`, `:mode`
3. **Document additions** - Update this README with new features
4. **Test startup time** - Verify `emacs-init-time` stays < 1s
5. **Follow patterns** - Match existing module structure
6. **Nix-first** - Add packages to `emacs.nix` first, then configure in elisp

## References

- [Doom Emacs](https://github.com/doomemacs/doomemacs) - Performance inspiration
- [emacs-overlay](https://github.com/nix-community/emacs-overlay) - Package source
- [Evil Documentation](https://evil.readthedocs.io/) - Vim emulation
- [use-package](https://github.com/jwiegley/use-package) - Package configuration
