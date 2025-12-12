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

;; refile.org popup configuration
(add-to-list 'display-buffer-alist
             '("refile\\.org"
               (display-buffer-pop-up-frame)
               (window-parameters (mode-line-format . none))))

(defun my/refile-org-setup ()
  "Setup for refile.org: hide modeline, manage date headers, and add timestamps."
  (when (string-match-p "refile\\.org" (buffer-name))
    ;; Hide modeline
    (setq-local mode-line-format nil)
    ;; Suppress server message by advising the message function temporarily
    (setq-local server-visit-hook nil)
    ;; Clear echo area
    (message "")

    (let* ((date-string (format-time-string "%Y-%m-%d"))
           (day-name (format-time-string "%A"))
           (date-header (format "* %s %s" date-string day-name))
           (time-string (format-time-string "%H:%M:%S"))
           (timestamp-entry (format "%s - " time-string))
           (header-regex (concat "^\\* " (regexp-quote date-string)))
           header-pos)

      ;; Clean up empty timestamp entries first
      (my/refile-org-cleanup-empty-timestamps)

      ;; Search for today's date header
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward header-regex nil t)
          (setq header-pos (line-beginning-position))))

      (if header-pos
          ;; Date header exists - add timestamp at top
          (progn
            (goto-char header-pos)
            (forward-line 1)
            (insert timestamp-entry "\n")
            (forward-line -1)
            (end-of-line))

        ;; Date header doesn't exist - create it
        (progn
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (unless (= (point) (point-min))
            (insert "\n"))
          (insert date-header "\n")
          (insert timestamp-entry)
          (end-of-line)))

      ;; Auto-save after insertion (silently)
      (when (buffer-modified-p)
        (let ((inhibit-message t))
          (save-buffer))
        ;; Clear the echo area again after save
        (message "")))))

(defun my/refile-org-cleanup-empty-timestamps ()
  "Remove timestamp entries that have nothing after the dash."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} - *$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

;; Disable server tutorial message globally
(setq server-client-instructions nil)

;; Hook for emacsclient visits (fires when client opens a file)
(add-hook 'server-visit-hook 'my/refile-org-setup)

;; Also hook into frame creation to handle buffer reuse
(defun my/refile-org-frame-setup (frame)
  "Run refile setup when a new frame is created showing refile.org."
  ;; Small delay to ensure buffer is fully loaded in frame
  (run-with-timer 0.1 nil
                  (lambda ()
                    (with-selected-frame frame
                      (when (and (buffer-file-name)
                                 (string-match-p "refile\\.org" (buffer-file-name)))
                        (my/refile-org-setup))))))

(add-hook 'after-make-frame-functions 'my/refile-org-frame-setup)

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
  :custom
  (org-roam-directory (file-truename "~/documents/projects/org-roam"))
  (org-roam-dailies-directory "daily/")
  :config
  (org-roam-db-autosync-mode)

  ;; Daily templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n* morning\n** priorities\n\n* session log\n\n* habits\n** TODO exercise :habit:\n   SCHEDULED: <%<%Y-%m-%d %a> +1d>\n** TODO review inbox :habit:\n   SCHEDULED: <%<%Y-%m-%d %a> +1d>\n\n* metrics\n:PROPERTIES:\n:STEPS: \n:PAGES: \n:EXERCISE_MIN: \n:END:\n\n* scratch\n\n* shutdown\n- [ ] session end protocol completed\n- [ ] inbox processed\n")))))
