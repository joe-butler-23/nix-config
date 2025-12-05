;;; core-ui.el --- Minimal UI configuration -*- lexical-binding: t -*-

;;; Basic UI Settings
;; Clean, minimal interface

;; Line numbers (only in programming modes)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Better defaults
(setq-default
 indent-tabs-mode nil           ; Spaces, not tabs
 tab-width 4                    ; 4 spaces per tab
 fill-column 80                 ; 80 character line width
 truncate-lines t               ; Don't wrap long lines
 word-wrap t)                   ; Wrap at word boundaries

;; Visible bell (no beeping)
(setq visible-bell t
      ring-bell-function 'ignore)

;; Smooth scrolling
(pixel-scroll-precision-mode 1)

;; Modern selection behavior
(delete-selection-mode 1)

;;; Which-Key - Discover Key Bindings
;; Shows available key bindings in popup
(use-package which-key
  :demand t
  :config
  (setq which-key-idle-delay 0.5           ; Show after 0.5s
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

;;; core-ui.el ends here
