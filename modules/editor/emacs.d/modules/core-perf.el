;;; core-perf.el --- Runtime performance optimizations -*- lexical-binding: t -*-

;;; GCMH - Garbage Collector Magic Hack
;; Intelligent garbage collection that defers GC until idle
;; Prevents multi-second hangs during active editing
(use-package gcmh
  :demand t  ; Load immediately
  :config
  (setq gcmh-idle-delay 'auto              ; Auto-detect idle delay
        gcmh-auto-idle-delay-factor 10     ; Conservative multiplier
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16MB during activity
  (gcmh-mode 1))

;;; Performance Settings
;; Optimize for modern systems

;; Increase process output buffer
(setq read-process-output-max (* 1024 1024))  ; 1MB (LSP performance)

;; Reduce UI responsiveness delays
(setq idle-update-delay 1.0)  ; Update UI every 1 second when idle

;; Fast scrolling
(setq fast-but-imprecise-scrolling t
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t)

;; Disable bidirectional text rendering for long lines
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Very long lines optimization
(global-so-long-mode 1)

;;; core-perf.el ends here
