;;; post-init.el --- Post-init configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Post-init configuration for packages and UX.
;;; Code:

;; If you are using undo-fu for Evil undo integration.
;; Note: undo-fu should be installed/available for this to work as intended.
(setq evil-undo-system 'undo-fu)

;; ============================================================
;; Evil (Vim emulation)
;; ============================================================
(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  ;; Enable Evil after Emacs finishes initialising.
  :hook (after-init . evil-mode)

  :init
  ;; These variables must be set before Evil loads.
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :custom
  ;; Make :s in visual mode operate only on the actual visual selection.
  (evil-ex-visual-char-range t)
  ;; Use Vim-style regex in search/substitute commands.
  (evil-ex-search-vim-style-regexp t)
  ;; Prefer splits below/right like Vim.
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  ;; Do not echo Evil state in the echo area.
  (evil-echo-state nil)
  ;; Do not move cursor back when exiting insert state.
  (evil-move-cursor-back nil)
  ;; Make `v$` exclude the final newline.
  (evil-v$-excludes-newline t)
  ;; Allow C-h / C-u delete behaviours in insert state.
  (evil-want-C-h-delete t)
  (evil-want-C-u-delete t)
  ;; Fine-grained undo behaviour.
  (evil-want-fine-undo t)
  ;; Allow moving beyond end-of-line in visual block mode.
  (evil-move-beyond-eol t)
  ;; Do not wrap search around the buffer.
  (evil-search-wrap nil)
  ;; Whether Y yanks to end of line.
  (evil-want-Y-yank-to-eol t))

;; After Evil loads, set initial states and define mode-specific keys.
(with-eval-after-load 'evil
  ;; Force certain modes to start in Evil normal state.
  (dolist (mode '(debugger-mode
                  backtrace-mode
                  help-mode
                  org-agenda-mode
                  messages-buffer-mode
                  info-mode
                  man-mode
                  calendar-mode
                  compilation-mode
                  occur-mode))
    (evil-set-initial-state mode 'normal))

  ;; Define debugger bindings once Emacs debugger keymap exists.
  (with-eval-after-load 'debug
    (evil-define-key 'normal debugger-mode-map
      "j" #'debugger-step-through
      "k" #'previous-line
      "q" #'top-level
      "y" #'evil-yank
      "C-a" #'mark-whole-buffer
      "RET" #'debugger-step-through))

  ;; Define Org Agenda bindings once org-agenda keymap exists.
  (with-eval-after-load 'org-agenda
    (evil-define-key 'normal org-agenda-mode-map
      "j" #'org-agenda-next-line
      "k" #'org-agenda-previous-line
      "t" #'org-agenda-todo
      "q" #'org-agenda-quit
      "r" #'org-agenda-redo
      "g" #'org-agenda-redo
      "s" #'org-agenda-schedule
      "e" #'org-agenda-entry-text-mode
      "f" #'org-agenda-follow-mode
      "RET" #'org-agenda-goto
      "TAB" #'org-agenda-goto)

    ;; Auto-refresh agenda when a task state changes:
    ;; - Save buffer when org triggers run
    ;; - Redo agenda after toggling TODO state from agenda
    (add-hook 'org-trigger-hook #'save-buffer)
    (advice-add 'org-agenda-todo :after #'org-agenda-redo)))

;; Evil Collection (Evil bindings across many modes)
(use-package evil-collection
  :after evil
  :ensure t
  :init
  ;; Must be set before evil-collection initialises.
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; ============================================================
;; Which-key
;; ============================================================
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0))

;; ============================================================
;; Themes
;; ============================================================
(use-package doom-themes
  :ensure t
  :config
  ;; Load theme immediately after package loads.
  (load-theme 'doom-nord t)
  ;; Optional theme styling toggles.
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; ============================================================
;; Magit
;; ============================================================
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch))

;; ============================================================
;; Undo-fu (+ session persistence)
;; ============================================================
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :config
  ;; Rebind Ctrl+Z / Ctrl+Shift+Z to undo-fu.
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") #'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") #'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;; ============================================================
;; Doom modeline
;; ============================================================
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; ============================================================
;; GCMH (garbage collection magic hack)
;; ============================================================
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

;; ============================================================
;; General.el (keybinding framework)
;; ============================================================
(use-package general
  :ensure t
  :after evil
  :config
  ;; Load keybindings from bindings.el (requires minimal-emacs-user-directory to be set elsewhere).
  (load (expand-file-name "bindings.el" minimal-emacs-user-directory)))

;; ============================================================
;; Completion: Vertico / Orderless / Marginalia / Embark / Consult
;; ============================================================
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :init
  ;; Use Embark’s prefix help.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode-line in Embark collect buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Make xref use consult UI.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; ============================================================
;; Code intelligence: Eglot / Treesit-auto
;; ============================================================
(use-package eglot
  ;; Eglot is built-in in modern Emacs, so do not install from ELPA.
  :ensure nil
  :commands (eglot-ensure eglot-rename eglot-format-buffer))

(use-package treesit-auto
  :ensure t
  :custom
  ;; Prompt before installing grammars.
  (treesit-auto-install 'prompt)
  :config
  ;; Add tree-sitter modes to auto-mode-alist and enable globally.
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ============================================================
;; Terminal: vterm
;; ============================================================
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; ============================================================
;; UI customisations
;; ============================================================
;; Remove the initial scratch message.
(setq initial-scratch-message nil)

;; Default font.
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 100)

;; Top padding via header line.
(setq-default header-line-format " ")

;; Ensure the header line blends in with the background and adds vertical padding.
(add-hook 'window-setup-hook
          (lambda ()
            (set-face-attribute 'header-line nil
                                :height 1.5
                                :background (face-attribute 'default :background)
                                :box nil
                                :inherit 'default)))

;; ============================================================
;; Claude Code Integration
;; ============================================================
;; Eat (Emulate A Terminal) - backend dependency for claude-code (if you use it that way).
(use-package eat
  :ensure t)

;; Claude Code package (installed via Nix, so :ensure nil).
(use-package claude-code
  :ensure nil
  :config
  (claude-code-mode))

;; Automatically revert buffers when files change on disk.
(global-auto-revert-mode 1)

;; ============================================================
;; Org-roam
;; ============================================================
(use-package org-roam
  :ensure t
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-capture
             org-roam-dailies-capture-today)
  :init
  (setq org-roam-directory (expand-file-name "~/projects/org-roam")
        org-roam-dailies-directory "daily/")
  :config
  (when (file-directory-p org-roam-directory)
    (org-roam-db-autosync-mode)))

;; ============================================================
;; Org-modern
;; ============================================================
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-todo-faces
   '(("TODO"  :background "#bf616a" :foreground "white" :weight bold)
     ("EVENT" :background "#b48ead" :foreground "white" :weight bold)
     ("DONE"  :background "#a3be8c" :foreground "white" :weight bold))))

;; ============================================================
;; Org-super-agenda
;; ============================================================
(use-package org-super-agenda
  :ensure t
  ;; Ensure Org Agenda is available before initialising integration.
  :after org-agenda
  :config
  (org-super-agenda-mode))

;; ============================================================
;; Org (agenda, todo keywords, helper functions, custom views)
;; ============================================================
(use-package org
  :ensure nil
  :config
  ;; Add tasks.org to agenda files.
  (add-to-list 'org-agenda-files "/home/joebutler/projects/tasks.org")

  ;; Enable automatic ID generation and tracking.
  (require 'org-id)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-id-track-globally t)

  ;; Define TODO keywords including EVENT.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "EVENT(e)" "|" "DONE(d!)")))

  ;; Remove current-time grid line display (the “now” marker).
  (setq org-agenda-current-time-string "")
  (setq org-agenda-time-grid '((daily) () "" ""))
  (setq org-agenda-block-separator ?─)

  ;; Define a spacer with reduced height (half-width effect).
  (defconst my/org-agenda-spacer (propertize "\n" 'face '(:height 0.4)))

  (defun my/org-agenda-move-time-to-end (item)
    "Move time (e.g. 16:00 or 16:00-17:00) from start to end of ITEM."
    (if (string-match
         "^\\s-*\\([0-9]+:[0-9]+\\(?:-[0-9]+:[0-9]+\\)?\\)\\s-+\\(.*\\)"
         item)
        (let ((time (match-string 1 item))
              (rest (match-string 2 item)))
          (concat rest " " (propertize time 'face '(:weight bold :slant italic))))
      item))

  ;; Clean agenda prefix formatting (controls what appears before each entry).
  (setq org-agenda-prefix-format
        '((agenda . "  %t ")
          (todo   . " %i ")
          (tags   . " %i ")
          (search . " %i ")))

  ;; Custom Agenda Views
  (setq org-agenda-custom-commands
        '(("o" "organised agenda"
           (
            ;; ------------------------------
            ;; BLOCK 1: Overdue
            ;; ------------------------------
            (tags "SCHEDULED<\"<today>\"|DEADLINE<\"<today>\""
                  ((org-agenda-overriding-header "⚠️ Overdue")
                   (org-super-agenda-groups
                    '((:name none :todo "TODO")))))

            ;; ------------------------------
            ;; BLOCK 2: Today (1 day)
            ;; ------------------------------
            (agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-overriding-header (concat my/org-agenda-spacer "📅 Today"))
                     (org-agenda-format-date "")
                     (org-super-agenda-groups
                      '((:name none :todo "EVENT")
                        (:name none :priority "A")
                        (:name none :todo "TODO")
                        (:discard (:anything t))))))

            ;; ------------------------------
            ;; BLOCK 3: Upcoming (90 days from tomorrow)
            ;; ------------------------------
            (agenda ""
                    ((org-agenda-span 90)
                     (org-agenda-start-day "+1d")
                     (org-agenda-show-all-dates nil)
                     (org-agenda-overriding-header (concat my/org-agenda-spacer "🔮 Upcoming"))
                     (org-super-agenda-groups
                      '((:name none :todo "EVENT"
                               :transformer my/org-agenda-move-time-to-end)
                        (:name none :todo "TODO")
                        (:discard (:anything t))))))

            ;; ------------------------------
            ;; BLOCK 4: Unscheduled TODOs
            ;; ------------------------------
            (alltodo ""
                     ((org-agenda-overriding-header (concat my/org-agenda-spacer "📥 Unscheduled"))
                      (org-super-agenda-groups
                       '((:name "Research" :tag "research")
                         (:name none :anything t)))
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))))
            )))))

;;; post-init.el ends here
