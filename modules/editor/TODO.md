# Emacs Configuration TODO

This document tracks planned future additions to the Emacs configuration. Items are organized by priority/dependency order.

## Current Status
- ✅ Core performance optimizations (early-init, GCMH)
- ✅ Evil mode with evil-collection and evil-surround
- ✅ which-key (already included in core)
- ✅ Basic UI configuration

## Planned Additions

### Phase 1: Essential Development Tools
These are the most critical additions for daily development work.

- [ ] **Magit** - Git porcelain for Emacs
  - Best git interface available, essential for version control
  - Package: `magit`

- [ ] **Undo System** - Better undo/redo
  - Options: `undo-fu` (Doom's choice, simpler) or `undo-tree` (visual tree)
  - Doom uses `undo-fu` with `undo-fu-session` for persistence
  - Recommended: `undo-fu` for simplicity and Evil integration

- [ ] **doom-modeline** - Attractive, informative modeline
  - Fast, minimal, shows git status, LSP status, etc.
  - Requires: `all-the-icons` fonts (can be installed via Nix)
  - Package: `doom-modeline`

### Phase 2: Keybindings & Leader Key System
Ergonomic, mnemonic keybindings for efficient workflow.

- [ ] **General.el** - Spacemacs-style leader key system
  - Package: `general.el` (Doom's keybinding system)
  - SPC as leader key in normal mode (Spacemacs/Doom convention)
  - , as local leader for mode-specific bindings
  - Mnemonic key structure:
    - `SPC f` - File operations (find, save, recent)
    - `SPC b` - Buffer operations (switch, kill, list)
    - `SPC p` - Project operations (find file, search, switch)
    - `SPC g` - Git operations (status, commit, push)
    - `SPC w` - Window operations (split, delete, navigate)
    - `SPC h` - Help operations (describe, info, apropos)
    - `SPC t` - Toggle operations (line numbers, theme, etc.)
    - `SPC o` - Open operations (terminal, dired, etc.)
    - `SPC i` - Insert/AI operations (snippet, AI assist)
    - `SPC a` - AI operations (gptel, agent shell)
  - Integrates with which-key for discoverability
  - Alternative: `evil-leader` (simpler, less flexible)

### Phase 3: Completion Framework
Critical for efficient navigation and command execution.

- [ ] **Completion System** - Minibuffer completion
  - **Doom's choice**: Vertico + Orderless + Consult + Marginalia
  - **Alternative**: Helm or Ivy (heavier, slower)
  - **Recommended**: Vertico stack (fastest, lowest latency)
    - `vertico` - Vertical completion UI
    - `orderless` - Flexible completion style (space-separated patterns)
    - `consult` - Enhanced commands (buffer switching, grep, etc.)
    - `marginalia` - Completion annotations
    - `embark` - Contextual actions on completions
  - Vertico is significantly faster than Helm/Ivy for large candidate lists

### Phase 4: Color Themes
Visual customization and consistency.

- [ ] **Theme Framework**
  - Doom has `doom-themes` package (20+ themes)
  - Alternative: `modus-themes` (built-in Emacs 28+, excellent accessibility)
  - Consider: Integration with existing Stylix theming
  - Recommended: Start with `modus-themes` (already available), add `doom-themes` if desired

### Phase 5: AI Integration
LLM-powered assistance and coding tools.

- [ ] **gptel** - ChatGPT/LLM integration
  - Package: `gptel`
  - Multi-provider support: OpenAI, Anthropic (Claude), local models, OpenAI-compatible APIs
  - Simple interface: Select region, send to LLM, receive response
  - Streaming responses
  - Can be bound to leader key: `SPC a g` for gptel
  - Custom prompts/templates for code review, refactoring, documentation
  - More lightweight than alternatives (ellama, copilot.el)

- [ ] **Agent Shell Integration** - Agentic AI in Emacs
  - Options:
    - `opencode.el` - OpenCode integration (if available as Emacs package)
    - `agent-shell` or custom shell integration
    - Direct subprocess/comint-mode integration with opencode/claude-code CLI
  - Potential bindings:
    - `SPC a o` - Open OpenCode session
    - `SPC a c` - Open Claude Code session
    - `SPC a s` - Send region to agent shell
  - Note: May need custom Elisp for CLI integration if no packages exist
  - Alternative: Use vterm with agent shells directly

### Phase 6: Code Intelligence (LSP & Tree-sitter)
Language-aware editing and navigation.

- [ ] **LSP Mode** - Language Server Protocol
  - Package: `lsp-mode` (Doom's choice)
  - Alternative: `eglot` (built-in Emacs 29+, simpler but less features)
  - Recommended: `lsp-mode` for feature completeness
  - Companion: `lsp-ui` for UI improvements (sideline, doc popup)

- [ ] **Tree-sitter** - Fast, incremental parsing
  - Built-in Emacs 29+ with native support
  - Need: `tree-sitter` grammars for each language
  - Emacs overlay provides `treesit-grammars` package
  - Major modes: Use `-ts-mode` variants (e.g., `python-ts-mode`)

- [ ] **Syntax Checking** - On-the-fly error detection
  - Doom uses: `flycheck` (mature, many checkers)
  - Built-in alternative: `flymake` (simpler, fewer checkers)
  - Recommended: `flycheck` for comprehensive coverage
  - Integrates with LSP mode automatically

### Phase 7: Project Management & Navigation
Fast file and project navigation.

- [ ] **Project Management**
  - Built-in: `project.el` (Emacs 28+, already available)
  - Alternative: `projectile` (Doom's choice, more features)
  - Recommended: Start with `project.el`, add `projectile` if needed

- [ ] **Search Tools** - Fast project-wide search
  - `ripgrep` integration via `rg.el` or `consult-ripgrep`
  - `fd` integration for fast file finding
  - Note: Both `ripgrep` and `fd` available via Nix, just need Elisp integration

- [ ] **File Navigation**
  - `dired` (built-in, already available)
  - Optional: `dirvish` for modern dired UI
  - Optional: `dired-sidebar` for IDE-like sidebar

### Phase 8: Terminal Integration
In-editor terminal access.

- [ ] **Terminal Emulator**
  - Options:
    - `vterm` (Doom's choice, fastest, requires native module)
    - `eat` (pure Elisp, good performance)
    - `ansi-term` (built-in, basic)
  - Recommended: `vterm` for performance
  - Requires: `libvterm` system package (available via Nix)
  - Evil integration: `evil-collection-vterm`

### Phase 9: Language-Specific Modules
Add support for specific programming languages.

- [ ] **Nix** - Essential for NixOS configuration
  - Mode: `nix-mode` or `nix-ts-mode` (tree-sitter)
  - LSP: `nixd` or `nil` (via `lsp-mode`)
  - Formatting: `nixpkgs-fmt` or `alejandra`

- [ ] **Python**
  - Mode: `python-mode` or `python-ts-mode`
  - LSP: `pyright`, `pylsp`, or `ruff-lsp`
  - Formatting: `black` or `ruff`

### Phase 10: Org Mode
Note-taking, task management, literate programming.

- [ ] **Org Mode Enhancement**
  - Built-in `org-mode` already available (Emacs 29+)
  - Configure: Agenda, capture templates, export
  - Optional: `org-modern` for better visuals
  - Optional: `org-roam` for Zettelkasten note-taking
  - Optional: `org-babel` for literate programming

### Phase 11: Smart Editing
Advanced text manipulation.

- [ ] **Smartparens** - Intelligent parenthesis handling
  - Package: `smartparens`
  - Doom uses this for all structural editing
  - Alternative: `electric-pair-mode` (built-in, simpler)
  - Note: Evil-surround already provides some functionality
  - Recommended: Start with built-in `electric-pair-mode`, add `smartparens` if needed

### Phase 12: Additional Enhancements (Lower Priority)

- [ ] **Company Mode** - Text completion frontend
  - Alternative to built-in `completion-at-point`
  - Integrates with LSP for code completion
  - Doom's choice for completion UI

- [ ] **Snippets** - Code templates
  - `yasnippet` - Snippet engine
  - `yasnippet-snippets` - Common snippet collection

- [ ] **Which-key Enhancements**
  - Already included, but could add custom prefix descriptions
  - Configure leader key system (SPC-based like Doom)

## Implementation Guidelines

For each addition:

1. **Research**: Understand existing approaches, e.g. Doom Emacs configuration for reference patterns
2. **Minimal First**: Add package with minimal config, test startup time
3. **Lazy Load**: Use `:defer`, `:hook`, `:commands`, `:mode` aggressively
4. **Document**: Update README.md with new features
5. **Test**: Verify `emacs-init-time` stays < 1.5 seconds
6. **Commit**: Atomic commits per feature/module
7. **Incremental**: Pursue one thing at a time, confirm it works before moving on

## Performance Targets

As packages are added:
- **Startup time**: < 1.5 seconds with all Phase 1-5 additions
- **Startup time**: < 2.0 seconds with all Phase 1-10 additions
- **LSP latency**: < 100ms for code actions
- **File opening**: < 50ms for typical files
- **gptel response**: < 3s initial response (depends on API)

## References

- [Doom Emacs Modules](https://github.com/doomemacs/doomemacs/tree/master/modules) - Reference implementation
- [General.el](https://github.com/noctuid/general.el) - Keybinding framework
- [Vertico](https://github.com/minad/vertico) - Vertical completion
- [gptel](https://github.com/karthink/gptel) - LLM integration
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/) - Language Server Protocol
- [Magit](https://magit.vc/) - Git porcelain
- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) - Incremental parsing
