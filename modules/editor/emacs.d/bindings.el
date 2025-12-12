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

  ;; Files
  "f"   '(:ignore t :which-key "files")
  "ff"  '(find-file :which-key "find file")
  "fs"  '(save-buffer :which-key "save buffer")
  "fr"  '(consult-recent-file :which-key "recent files")
  "fR"  '(rename-file :which-key "rename file")
  "fD"  '(delete-file :which-key "delete file")

  ;; Buffers
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(consult-buffer :which-key "switch buffer")
  "bk"  '(kill-current-buffer :which-key "kill buffer")
  "bi"  '(ibuffer :which-key "ibuffer")
  "bn"  '(next-buffer :which-key "next buffer")
  "bp"  '(previous-buffer :which-key "previous buffer")
  "b["  '(previous-buffer :which-key "previous buffer")
  "b]"  '(next-buffer :which-key "next buffer")

  ;; Windows
  "w"   '(:ignore t :which-key "windows")
  "wh"  '(evil-window-left :which-key "window left")
  "wj"  '(evil-window-down :which-key "window down")
  "wk"  '(evil-window-up :which-key "window up")
  "wl"  '(evil-window-right :which-key "window right")
  "ws"  '(split-window-below :which-key "split horizontal")
  "wv"  '(split-window-right :which-key "split vertical")
  "wd"  '(delete-window :which-key "delete window")
  "w="  '(balance-windows :which-key "balance windows")
  "wo"  '(delete-other-windows :which-key "delete other windows")

  ;; Search
  "s"   '(:ignore t :which-key "search")
  "ss"  '(consult-line :which-key "search buffer")
  "sr"  '(consult-ripgrep :which-key "search project (rg)")
  "si"  '(consult-imenu :which-key "jump to symbol")
  "sk"  '(consult-yank-pop :which-key "search kill ring")

  ;; Git
  "g"   '(:ignore t :which-key "git")
  "gg"  '(magit-status :which-key "magit status")
  "gl"  '(magit-log-current :which-key "magit log")

  ;; Project
  "p"   '(:ignore t :which-key "project")
  "pf"  '(project-find-file :which-key "find file in project")
  "pp"  '(project-switch-project :which-key "switch project")
  "pk"  '(project-kill-buffers :which-key "kill project buffers")

  ;; Org Mode
  "o"   '(:ignore t :which-key "org mode")
  "of"  '(org-roam-node-find :which-key "find node")
  "od"  '(my/open-today-daily-note :which-key "today")
  "oc"  '(org-roam-dailies-capture-today :which-key "capture today")
  "ot"  '(org-todo :which-key "cycle todo")
  "ox"  '(org-toggle-checkbox :which-key "toggle checkbox")
  "oi"  '(org-narrow-to-subtree :which-key "zoom in (narrow)")
  "oo"  '(widen :which-key "zoom out (widen)")

  ;; Toggles
  "t"   '(:ignore t :which-key "toggles")
  "tf"  '(toggle-frame-fullscreen :which-key "toggle fullscreen")
  "tl"  '(display-line-numbers-mode :which-key "toggle line numbers")
  "tw"  '(toggle-truncate-lines :which-key "toggle truncate lines")

  ;; Help
  "h"   '(:ignore t :which-key "help")
  "hf"  '(describe-function :which-key "describe function")
  "hv"  '(describe-variable :which-key "describe variable")
  "hk"  '(describe-key :which-key "describe key")
  "hb"  '(embark-bindings :which-key "show bindings")
  "hr"  '(lambda () (interactive) (load-file user-init-file) (ignore (message "Reloaded init.el")))

  ;; AI (Custom)
  "a"   '(:ignore t :which-key "ai")
  "ac"  '(claude-code-command-map :which-key "claude code"))

(provide 'bindings)
;;; bindings.el ends here
