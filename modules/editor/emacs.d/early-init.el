;;; early-init.el --- Early initialization for performance -*- lexical-binding: t -*-

;; Performance optimizations from Doom Emacs
;; This file runs BEFORE GUI initialization and package system

;;; 1. Garbage Collection Suppression During Startup
;; Defer garbage collection during startup to avoid interruptions
;; Will be restored to reasonable values after init completes
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;;; 2. File Name Handler Optimization
;; Temporarily disable file handlers for faster startup
;; These handle remote files, compression, encryption, etc.
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file handlers after startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist my--file-name-handler-alist)))

;;; 3. Load Preference Optimization
;; Don't check timestamps on .elc files during startup
(setq load-prefer-newer nil)

;;; 4. UI Optimizations (Set Before Frame Creation)
;; Configure UI elements before the GUI initializes for cleaner startup

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; Disable unnecessary UI elements
(setq use-dialog-box nil
      use-file-dialog nil)

;; Frame parameters (set before first frame)
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (alpha-background . 95)))

;; Disable unnecessary UI modes early
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;;; 5. Native Compilation Settings
;; Suppress warnings during async native compilation
(setq native-comp-async-report-warnings-errors nil
      native-comp-warning-on-missing-source nil)

;; Optimize native compilation
(setq native-comp-speed 2
      native-comp-deferred-compilation t)

;;; 6. Package System (Defer to init.el)
;; Disable package.el since we're using Nix for package management
(setq package-enable-at-startup nil)

;;; early-init.el ends here
