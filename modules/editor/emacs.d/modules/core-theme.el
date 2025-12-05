;;; core-theme.el --- Nord theme configuration -*- lexical-binding: t -*-

(use-package doom-themes
  :demand t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Load theme immediately
(load-theme 'doom-nord t)

;;; core-theme.el ends here
