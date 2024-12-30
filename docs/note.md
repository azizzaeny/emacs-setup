note

```elisp

(defun note-open-today-local (&optional year month day)
  "Open a note for a specific YEAR, MONTH, and DAY. If not provided, use today's date. If the file doesn't exist, create it."
  (interactive)
  (unless year
    (let ((current-date (decode-time (current-time))))
      (setq year (nth 5 current-date)
            month (nth 4 current-date)
            day (nth 3 current-date))))
  (let* ((directory "~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat directory filename)))
    (find-file fullpath)
    (when (not (file-exists-p fullpath))
      (insert (format "## Note - %04d%02d%02d\n\n" year month day))
      (save-buffer))))

(defun note-open-n-day-local (n)
  "Open note n days before"
  (interactive "p")
  (let* ((target-date (time-subtract (current-time) (days-to-time n)))
         (year (string-to-number (format-time-string "%Y" target-date)))
         (month (string-to-number (format-time-string "%m" target-date)))
         (day (string-to-number (format-time-string "%d" target-date)))
         (directory "~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat directory filename)))    
     (if (file-exists-p fullpath)
        (find-file fullpath)
       (message "Note for date %s does not exist." (format-time-string "%Y-%m-%d" target-date)))))

(defun note-open-today (&optional year month day)
  "Open a note for a specific YEAR, MONTH, and DAY on a remote server using TRAMP with sshx.
If not provided, use today's date. If the file doesn't exist, create it."
  (interactive)
  (unless year
    (let ((current-date (decode-time (current-time))))
      (setq year (nth 5 current-date)
            month (nth 4 current-date)
            day (nth 3 current-date))))
  (let* ((remote-directory "/sshx:sandbox:~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat remote-directory filename)))
    (message "connecting to remote server...")
    (find-file fullpath)
    (when (not (file-exists-p fullpath))
      (insert (format "## Note - %04d%02d%02d\n\n" year month day))
      (save-buffer))))


;; (defun note-open-yesterday ()
;;   "open yesterday notes"
;;   (interactive)
;;   (note-open-n-day 1))

;; (defun note-open-n2 ()
;;   "open n day -2"
;;   (interactive)
;;   (note-open-n-day 2))

;; (defun note-open-n3 ()
;;   "open n day -3"
;;   (interactive)
;;   (note-open-n-day 3))

;; (defun note-open-n4 ()
;;   "open n day -4"
;;   (interactive)
;;   (note-open-n-day 4))

;; (defun note-open-n5 ()
;;   "open n day 5"
;;   (interactive)
;;   (note-open-n-day 5))

;; (defun note-open-n6 ()
;;   "open n day 6"
;;   (interactive)
;;   (note-open-n-day 6))

;; (defun note-open-n7 ()
;;   "open n day 7"
;;   (interactive)
;;   (note-open-n-day 7))

;; prototype mode

```

