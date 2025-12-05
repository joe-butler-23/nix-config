;;; core-theme.el --- Nord theme configuration -*- lexical-binding: t -*-

;;; Doom Nord Theme
;; Matches Kitty terminal Nord theme for consistent aesthetics
(use-package doom-themes
  :demand t  ; Load immediately for consistent appearance
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; Enable bold fonts
        doom-themes-enable-italic t)   ; Enable italic fonts

  ;; Load the Nord theme
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects and improves org-mode's fontification
  (doom-themes-org-config))

;;; core-theme.el ends here
