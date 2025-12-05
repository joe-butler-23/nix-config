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

  ;; Optional: Enable visual bell and org-mode improvements if functions exist
  (when (fboundp 'doom-themes-visual-bell-config)
    (doom-themes-visual-bell-config))

  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config)))

;; Ensure theme loads in emacsclient frames
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'doom-nord t))))
  (load-theme 'doom-nord t))

;;; core-theme.el ends here
