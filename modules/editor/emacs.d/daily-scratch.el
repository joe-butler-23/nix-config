;;; daily-scratch.el --- Daily scratchpad configuration -*- lexical-binding: t; -*-

;; Daily scratch popup configuration (based on refile.org pattern)
(add-to-list 'display-buffer-alist
             '("daily-scratch"
               (display-buffer-pop-up-frame)
               (window-parameters (mode-line-format . none))))

(defun my/daily-scratch-cleanup-empty-timestamps ()
  "Remove timestamp entries that have nothing after the dash."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\} - *$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun my/daily-scratch-setup ()
  "Setup for daily scratch: hide modeline, insert at top of scratch, and add timestamps."
  (ignore-errors
    (when (and (buffer-file-name)
               (string-match-p "/daily/[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\.org$" (buffer-file-name)))
      ;; Hide modeline (from refile.org)
      (setq-local mode-line-format nil)
      ;; Suppress server message
      (setq-local server-visit-hook nil)
      ;; Clear echo area immediately
      (message "")

      ;; Clean up empty timestamp entries first
      (my/daily-scratch-cleanup-empty-timestamps)

      ;; Jump to scratch section and insert at top
      (widen)  ;; Ensure we're not already narrowed
      (goto-char (point-min))
      (when (re-search-forward "^\* scratch$" nil t)
        ;; Found scratch section - NARROW to it first
        (org-narrow-to-subtree)

        ;; Now insert timestamp at top of the narrowed buffer (skipping the heading)
        (goto-char (point-min)) ; Go to start of narrowed buffer (the heading)
        (forward-line 1) ; Move past the "* scratch" heading

        (let* ((time-string (format-time-string "%H:%M:%S"))
               (timestamp-entry (format "%s - " time-string)))

          (insert timestamp-entry "\n")
          (forward-line -1) ; Move back to the beginning of the newly inserted line
          (end-of-line)

          ;; Auto-save after insertion (silently)
          (when (buffer-modified-p)
            (let ((inhibit-message t))
              (save-buffer))
            ;; Clear the echo area again after save ensures we leave it blank
            (message "")))))))

;; Disable server tutorial message globally (from refile.org)
(setq server-client-instructions nil)

;; Hook for emacsclient visits (fires when client opens a file)
(add-hook 'server-visit-hook 'my/daily-scratch-setup)

;; Also hook into frame creation to handle buffer reuse (from refile.org)
(defun my/daily-scratch-frame-setup (frame)
  "Run daily-scratch setup when a new frame is created showing a daily note."
  ;; Small delay to ensure buffer is fully loaded in frame (from refile.org)
  (run-with-timer 0.1 nil
                  (lambda ()
                    (ignore-errors
                      (when (frame-live-p frame)
                        (with-selected-frame frame
                          (when (and (buffer-file-name)
                                     (string-match-p "/daily/[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\.org$" (buffer-file-name)))
                            (my/daily-scratch-setup)))))))

(add-hook 'after-make-frame-functions 'my/daily-scratch-frame-setup)
