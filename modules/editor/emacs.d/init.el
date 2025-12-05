;;; init.el --- Minimal Emacs configuration -*- lexical-binding: t -*-

;; NOTE: Package installation is handled by Nix (see emacs.nix)
;; This file focuses on configuration only

;;; 1. Reset GC Threshold After Startup
;; Restore reasonable GC threshold after init completes
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
          gc-cons-percentage 0.1)))

;;; 2. Use-Package Configuration
;; use-package is built-in on Emacs 29+
(require 'use-package)
(setq use-package-always-defer t        ; Defer everything by default
      use-package-always-ensure nil     ; Nix handles installation
      use-package-expand-minimally t    ; Minimal macro expansion for faster init
      use-package-compute-statistics nil) ; Disable statistics for performance

;;; 3. Load Core Modules
;; Each module is self-contained and focused
(load (concat user-emacs-directory "modules/core-perf.el"))
(load (concat user-emacs-directory "modules/core-ui.el"))
(load (concat user-emacs-directory "modules/core-evil.el"))

;;; 4. Custom File (Keep Init Clean)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
