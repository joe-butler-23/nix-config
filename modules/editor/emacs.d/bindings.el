;;; bindings.el --- Keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines the keybindings for the Emacs configuration, inspired by
;; Spacemacs and Doom Emacs. It uses `general.el` to define leader keys.

;;; Code:

;; Define the leader key
(general-create-definer my-leader-def
  :prefix "SPC"
  :states '(normal visual motion)
  :keymaps 'override)

(general-create-definer my-local-leader-def
  :prefix ","
  :states '(normal visual motion)
  :keymaps 'override)

;; Global Keybindings
(my-leader-def
  "SPC" '(execute-extended-command :which-key "M-x")
  ":"   '(execute-extended-command :which-key "M-x")
  "."   '(find-file :which-key "Find file")
  "u"   '(universal-argument :which-key "Universal argument")
  "'"   '(vertico-repeat :which-key "Resume last search")

  ;; Files
  "f"   '(:ignore t :which-key "files")
  "ff"  '(find-file :which-key "Find file")
  "fs"  '(save-buffer :which-key "Save buffer")
  "fr"  '(consult-recent-file :which-key "Recent files")
  "fR"  '(rename-file :which-key "Rename file")
  "fD"  '(delete-file :which-key "Delete file")

  ;; Buffers
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(consult-buffer :which-key "Switch buffer")
  "bk"  '(kill-current-buffer :which-key "Kill buffer")
  "bi"  '(ibuffer :which-key "IBuffer")
  "bn"  '(next-buffer :which-key "Next buffer")
  "bp"  '(previous-buffer :which-key "Previous buffer")
  "b["  '(previous-buffer :which-key "Previous buffer")
  "b]"  '(next-buffer :which-key "Next buffer")

  ;; Windows
  "w"   '(:ignore t :which-key "windows")
  "wh"  '(evil-window-left :which-key "Window left")
  "wj"  '(evil-window-down :which-key "Window down")
  "wk"  '(evil-window-up :which-key "Window up")
  "wl"  '(evil-window-right :which-key "Window right")
  "ws"  '(split-window-below :which-key "Split horizontal")
  "wv"  '(split-window-right :which-key "Split vertical")
  "wd"  '(delete-window :which-key "Delete window")
  "w="  '(balance-windows :which-key "Balance windows")
  "wo"  '(delete-other-windows :which-key "Delete other windows")

  ;; Search
  "s"   '(:ignore t :which-key "search")
  "ss"  '(consult-line :which-key "Search buffer")
  "sr"  '(consult-ripgrep :which-key "Search project (rg)")
  "si"  '(consult-imenu :which-key "Jump to symbol")
  "sk"  '(consult-yank-pop :which-key "Search kill ring")

  ;; Git
  "g"   '(:ignore t :which-key "git")
  "gg"  '(magit-status :which-key "Magit status")
  "gl"  '(magit-log-current :which-key "Magit log")

  ;; Project
  "p"   '(:ignore t :which-key "project")
  "pf"  '(project-find-file :which-key "Find file in project")
  "pp"  '(project-switch-project :which-key "Switch project")
  "pk"  '(project-kill-buffers :which-key "Kill project buffers")

  ;; Toggles
  "t"   '(:ignore t :which-key "toggles")
  "tf"  '(toggle-frame-fullscreen :which-key "Toggle fullscreen")
  "tl"  '(display-line-numbers-mode :which-key "Toggle line numbers")
  "tw"  '(toggle-truncate-lines :which-key "Toggle truncate lines")

  ;; Help
  "h"   '(:ignore t :which-key "help")
  "hf"  '(describe-function :which-key "Describe function")
  "hv"  '(describe-variable :which-key "Describe variable")
  "hk"  '(describe-key :which-key "Describe key")
  "hb"  '(embark-bindings :which-key "Show bindings")
  "hr"  '(lambda () (interactive) (load-file user-init-file) (ignore (message "Reloaded init.el")))

  ;; AI (Custom)
  "a"   '(:ignore t :which-key "ai")
  "ac"  '(claude-code-command-map :which-key "Claude Code"))

(provide 'bindings)
;;; bindings.el ends here
