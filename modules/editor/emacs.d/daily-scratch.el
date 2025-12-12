;;; daily-scratch.el --- Daily scratchpad configuration -*- lexical-binding: t; -*-

;; Fast daily scratch opener for Emacs daemon + emacsclient.

(defgroup my/daily-scratch nil
  "Quick capture into today's org-roam daily scratch section."
  :group 'convenience)

(defcustom my/daily-scratch-frame-name "daily-scratch"
  "Name of the dedicated daily scratch frame."
  :type 'string)

(defcustom my/daily-scratch-auto-save-delay 0.25
  "Seconds to wait before auto-saving after inserting a timestamp."
  :type 'number)

(defun my/daily-scratch--daily-dir ()
  (cond
   ((and (boundp 'org-roam-directory)
         (boundp 'org-roam-dailies-directory)
         org-roam-directory
         org-roam-dailies-directory)
    (file-name-as-directory
     (expand-file-name org-roam-dailies-directory (file-truename org-roam-directory))))
   (t
    (file-name-as-directory (file-truename "~/documents/projects/org-roam/daily/")))))

(defun my/daily-scratch--today-file ()
  (expand-file-name (format-time-string "%Y-%m-%d.org") (my/daily-scratch--daily-dir)))

(defun my/daily-scratch--ensure-file (file)
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (let ((date-title (format-time-string "%Y-%m-%d %A"))
          (date-scheduled (format-time-string "%Y-%m-%d %a")))
      (with-temp-file file
        (insert "#+title: " date-title "\n")
        (insert "#+filetags: :daily:\n\n")
        (insert "* morning\n** priorities\n\n")
        (insert "* session log\n\n")
        (insert "* habits\n")
        (insert "** TODO exercise :habit:\n")
        (insert "   SCHEDULED: <" date-scheduled " +1d>\n")
        (insert "** TODO review inbox :habit:\n")
        (insert "   SCHEDULED: <" date-scheduled " +1d>\n\n")
        (insert "* metrics\n:PROPERTIES:\n:STEPS:\n:PAGES:\n:EXERCISE_MIN:\n:END:\n\n")
        (insert "* scratch\n\n")
        (insert "* shutdown\n")
        (insert "- [ ] session end protocol completed\n")
        (insert "- [ ] inbox processed\n")))))

(defun my/daily-scratch--find-frame ()
  (let ((frames (frame-list))
        (found nil))
    (while (and frames (not found))
      (let ((frame (car frames)))
        (when (string= (frame-parameter frame 'name) my/daily-scratch-frame-name)
          (setq found frame)))
      (setq frames (cdr frames)))
    found))

(defun my/daily-scratch--get-or-create-frame ()
  (or (my/daily-scratch--find-frame)
      (make-frame `((name . ,my/daily-scratch-frame-name)
                    (frame-title-format . ,my/daily-scratch-frame-name)))))

(defun my/daily-scratch--indirect-buffer-name (file)
  (format " *daily-scratch:%s*"
          (file-name-nondirectory (file-name-sans-extension file))))

(defun my/daily-scratch--get-or-create-indirect-buffer (base-buffer file)
  "Return an indirect buffer for BASE-BUFFER's FILE.

Using an indirect buffer keeps narrowing/point local to the scratch view, so
opening the scratch does not \"zoom\" other windows that display the same file."
  (let* ((name (my/daily-scratch--indirect-buffer-name file))
         (existing (get-buffer name)))
    (cond
     ((and existing
           (buffer-live-p existing)
           (eq (buffer-base-buffer existing) base-buffer))
      existing)
     (existing
      (kill-buffer existing)
      (with-current-buffer base-buffer
        (clone-indirect-buffer name nil)))
     (t
      (with-current-buffer base-buffer
        (clone-indirect-buffer name nil))))))

(defun my/daily-scratch--goto-scratch-heading ()
  (widen)
  (goto-char (point-max))
  (unless (re-search-backward "^\\* scratch$" nil t)
    (goto-char (point-max))
    (insert "\n* scratch\n")))

(defun my/daily-scratch--cleanup-empty-timestamps ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} - *$" nil t)
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun my/daily-scratch--prepare-visible-buffer ()
  (require 'org)
  (my/daily-scratch--goto-scratch-heading)
  (org-narrow-to-subtree)
  (my/daily-scratch--cleanup-empty-timestamps)
  (goto-char (point-min))
  (forward-line 1)
  (insert (format-time-string "%H:%M:%S - ") "\n")
  (forward-line -1)
  (end-of-line))

(defun my/daily-scratch--maybe-save (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (buffer-file-name) (buffer-modified-p))
        (let ((inhibit-message t))
          (save-buffer))))))

;;;###autoload
(defun my/daily-scratch-open ()
  "Open today's daily note at the scratch section, optimized for instant feel."
  (interactive)
  (let* ((file (my/daily-scratch--today-file)))
    (my/daily-scratch--ensure-file file)
    (let* ((base-buffer (find-file-noselect file))
           (scratch-buffer (my/daily-scratch--get-or-create-indirect-buffer base-buffer file))
           (frame (my/daily-scratch--get-or-create-frame)))
      (when (frame-live-p frame)
        (set-frame-parameter frame 'frame-title-format my/daily-scratch-frame-name)
        (raise-frame frame)
        (select-frame-set-input-focus frame))
      (with-selected-frame frame
        (switch-to-buffer scratch-buffer)
        (setq-local mode-line-format nil)
        (ignore-errors (my/daily-scratch--prepare-visible-buffer))
        (run-with-idle-timer my/daily-scratch-auto-save-delay
                             nil
                             #'my/daily-scratch--maybe-save
                             base-buffer)))))

;; Disable server tutorial message globally (from refile.org)
(setq server-client-instructions nil)
