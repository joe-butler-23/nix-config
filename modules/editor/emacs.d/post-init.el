;;; post-init.el --- Post-init configuration -*- lexical-binding: t; -*-

;; Uncomment the following if you are using undo-fu
(setq evil-undo-system 'undo-fu)

;; Vim emulation
(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :hook (after-init . evil-mode)

  :init
  ;; It has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :custom
  ;; Make :s in visual mode operate only on the actual visual selection
  ;; (character or block), instead of the full lines covered by the selection
  (evil-ex-visual-char-range t)
  ;; Use Vim-style regular expressions in search and substitute commands,
  ;; allowing features like \v (very magic), \zs, and \ze for precise matches
  (evil-ex-search-vim-style-regexp t)
  ;; Enable automatic horizontal split below
  (evil-split-window-below t)
  ;; Enable automatic vertical split to the right
  (evil-vsplit-window-right t)
  ;; Disable echoing Evil state to avoid replacing eldoc
  (evil-echo-state nil)
  ;; Do not move cursor back when exiting insert state
  (evil-move-cursor-back nil)
  ;; Make `v$` exclude the final newline
  (evil-v$-excludes-newline t)
  ;; Allow C-h to delete in insert state
  (evil-want-C-h-delete t)
  ;; Enable C-u to delete back to indentation in insert state
  (evil-want-C-u-delete t)
  ;; Enable fine-grained undo behavior
  (evil-want-fine-undo t)
  ;; Allow moving cursor beyond end-of-line in visual block mode
  (evil-move-beyond-eol t)
  ;; Disable wrapping of search around buffer
  (evil-search-wrap nil)
  ;; Whether Y yanks to the end of the line
  (evil-want-Y-yank-to-eol t))

;; Enable evil mode in certain buffers
(with-eval-after-load 'evil
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

  ;; Explicitly bind keys for Org Agenda in Normal state
  (evil-define-key 'normal org-agenda-mode-map
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "t" 'org-agenda-todo
    "q" 'org-agenda-quit
    "r" 'org-agenda-redo
    "g" 'org-agenda-redo
    "s" 'org-agenda-schedule
    "e" 'org-agenda-entry-text-mode
    "f" 'org-agenda-follow-mode
    "RET" 'org-agenda-goto
    "TAB" 'org-agenda-goto)

  ;; Explicitly bind keys for Debugger Mode in Normal state
  (evil-define-key 'normal debugger-mode-map
    "j" 'debugger-step-through
    "k" 'previous-line
    "q" 'top-level
    "y" 'evil-yank
    "C-a" 'mark-whole-buffer
    "RET" 'debugger-step-through)

  ;; Auto-refresh agenda when a task state changes
  (add-hook 'org-trigger-hook 'save-buffer)
  (advice-add 'org-agenda-todo :after 'org-agenda-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  ;; It has to be defined before evil-colllection
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Which-key configuration
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0))

;; Doom Themes configuration
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t)
  ;; Optional: Enable bold and italic for doom-themes (if desired)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Magit configuration
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch))

;; Undo-fu configuration
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;; Doom Modeline configuration
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; GCMH configuration
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

;; General.el configuration
(use-package general
  :ensure t
  :after evil
  :config
  ;; Load keybindings from bindings.el
  (load (expand-file-name "bindings.el" minimal-emacs-user-directory)))

;; Completion configuration
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
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Code Intelligence
(use-package eglot
  :ensure nil
  :commands (eglot-ensure eglot-rename eglot-format-buffer))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Terminal configuration
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; UI Customizations
;; Remove the initial scratch message
(setq initial-scratch-message nil)

;; Font Configuration
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 100)

;;; Top Padding
;; Add visual padding at the top of the buffer using the header line.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Header-Lines.html
(setq-default header-line-format " ")

;; Ensure the header line blends in with the background
(add-hook 'window-setup-hook
          (lambda ()
            (set-face-attribute 'header-line nil
                                :height 1.5           ; 1.5x line height padding
                                :background (face-attribute 'default :background)
                                :box nil
                                :inherit 'default)))

;;; Claude Code Integration

;; Eat (Emulate A Terminal) - Recommended backend for claude-code
(use-package eat
  :ensure t)

;; Claude Code
(use-package claude-code
  :ensure nil ;; Installed via Nix
  :config
  (claude-code-mode))

;; Global Auto Revert
(global-auto-revert-mode 1)

;; Org-roam configuration
(use-package org-roam
  :ensure t
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture org-roam-dailies-capture-today)
  :init
  (setq org-roam-directory (expand-file-name "~/projects/org-roam")
        org-roam-dailies-directory "daily/")
  :config
  (when (file-directory-p org-roam-directory)
    (org-roam-db-autosync-mode)))

;; Org-modern configuration
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-todo-faces
   '(("TODO" :background "#bf616a" :foreground "white" :weight bold)
     ("EVENT" :background "#b48ead" :foreground "white" :weight bold)
     ("DONE" :background "#a3be8c" :foreground "white" :weight bold))))

;; Org-super-agenda configuration
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

;; Org Mode Configuration
(use-package org
  :ensure nil
  :config
  ;; Add tasks.org to agenda files
  (add-to-list 'org-agenda-files "/home/joebutler/projects/tasks.org")

  ;; Enable automatic ID generation and tracking
  (require 'org-id)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-id-track-globally t)

  ;; Define TODO keywords including EVENT
  (setq org-todo-keywords
        '((sequence "TODO(t)" "EVENT(e)" "|" "DONE(d!)")))

   ;; Remove time grid lines
   (setq org-agenda-current-time-string "")
   (setq org-agenda-time-grid '((daily) () "" ""))
   (setq org-agenda-block-separator "")

   (defun my/org-agenda-move-time-to-end (item)
     "Move time (e.g. 16:00 or 16:00-17:00) from start to end of ITEM."
     (if (string-match "^\\s-*\\([0-9]+:[0-9]+\\(?:-[0-9]+:[0-9]+\\)?\\)\\s-+" item)
         (let ((time (match-string 1 item))
               (rest-start (match-end 0)))
           (concat (substring item rest-start)
                   " "
                   (propertize time 'face '(:weight bold :slant italic))))
       item))

   ;; Clean agenda format: remove category prefix, show time at end
   (setq org-agenda-prefix-format
         '((agenda . "  %t ")
           (todo . " %i ")
           (tags . " %i ")
           (search . " %i ")))

   ;; Custom Agenda Views
   (setq org-agenda-custom-commands
         '(("o" "Organised Agenda"
            ((tags "SCHEDULED<\"<today>\"|DEADLINE<\"<today>\""
                   ((org-agenda-overriding-header "⚠️ Overdue")
                    (org-super-agenda-groups
                     '((:name none :todo "TODO")))))
             (agenda ""
                     ((org-agenda-span 1)          ; Focus on TODAY
                      (org-agenda-overriding-header "\n📅 Today")
                      (org-agenda-format-date "")
                      (org-super-agenda-groups
                       '((:name none :todo "EVENT")
                         (:name none :habit t)
                         (:name none :priority "A")
                         (:name none :todo "TODO")
                         (:discard (:anything t))))
                      (org-agenda-skip-function 'my/skip-past-scheduled)))
             (agenda ""
                     ((org-agenda-span 90)         ; Look ahead 3 months
                      (org-agenda-start-day "+1d") ; Start from tomorrow
                      (org-agenda-show-all-dates nil)  ; Only show dates with items
                      (org-agenda-overriding-header "\n🔮 Upcoming")
                      (org-super-agenda-groups
                       '((:name none :todo "EVENT" :transformer my/org-agenda-move-time-to-end)
                         (:name none :todo "TODO")
                         (:discard (:anything t))))))
             (alltodo ""
                      ((org-agenda-overriding-header "\n📥 Unscheduled")
                       (org-super-agenda-groups
                        '((:name "Research" :tag "research")
                          (:name none :anything t)))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp)))))))))
