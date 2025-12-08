;;; post-early-init.el --- Post-early-init configuration -*- lexical-binding: t; -*-

;; Add padding/margins to the Emacs frame
;; internal-border-width adds space between the frame border and the text area (top/bottom/left/right)
(add-to-list 'default-frame-alist '(internal-border-width . 12))

;; Set fringe widths (left/right margins for indicators)
(add-to-list 'default-frame-alist '(left-fringe . 12))
(add-to-list 'default-frame-alist '(right-fringe . 12))
