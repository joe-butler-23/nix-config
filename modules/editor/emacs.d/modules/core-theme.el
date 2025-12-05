;;; core-theme.el --- Nord theme configuration -*- lexical-binding: t -*-

(use-package doom-themes
  :demand t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Load theme for new frames (daemon) or immediately (standalone)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (load-theme 'doom-nord t)))
  (load-theme 'doom-nord t))

;;; core-theme.el ends here
