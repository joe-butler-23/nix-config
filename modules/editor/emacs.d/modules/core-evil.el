;;; core-evil.el --- Vim emulation with Evil mode -*- lexical-binding: t -*-

;;; Evil - Extensible VI Layer
;; Complete Vim emulation for Emacs
(use-package evil
  :demand t  ; Load immediately for consistent keybindings
  :init
  ;; Evil configuration (must be set before loading)
  (setq evil-want-integration t         ; Enable evil-collection integration
        evil-want-keybinding nil        ; Let evil-collection handle bindings
        evil-want-C-u-scroll t          ; C-u scrolls up (Vim behavior)
        evil-want-C-d-scroll t          ; C-d scrolls down
        evil-want-Y-yank-to-eol t       ; Y yanks to end of line (Vim default)
        evil-split-window-below t       ; Split below (consistent with tmux)
        evil-vsplit-window-right t      ; Split right
        evil-undo-system 'undo-redo)    ; Use Emacs 28+ undo-redo system

  :config
  (evil-mode 1)

  ;; Use visual line motions (j/k move by visual line, not logical)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set initial states for specific modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;;; Evil Collection
;; Evil bindings for built-in Emacs modes
(use-package evil-collection
  :after evil
  :demand t
  :config
  ;; Don't activate for these modes (we'll configure them separately)
  (setq evil-collection-mode-list
        (delq 'lisp-mode evil-collection-mode-list))
  (evil-collection-init))

;;; Evil Surround
;; Surround text objects (cs, ds, ys commands)
;; Examples:
;;   cs"'   - Change surrounding " to '
;;   ds"    - Delete surrounding "
;;   ysiw"  - Surround inner word with "
(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

;;; core-evil.el ends here
