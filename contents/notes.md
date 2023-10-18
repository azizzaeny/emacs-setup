open today notes

```elisp

(defun open-note (&optional year month day)
  "Open a note for a specific YEAR, MONTH, and DAY. If not provided, use today's date. If the file doesn't exist, create it."
  (interactive)
  (unless year
    (let ((current-date (decode-time (current-time))))
      (setq year (nth 5 current-date)
            month (nth 4 current-date)
            day (nth 3 current-date))))
  (let* ((directory "~/daily-notes-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat directory filename)))
    (find-file fullpath)
    (when (not (file-exists-p fullpath))
      (insert (format "## Note - %04d%02d%02d\n\n" year month day))
      (save-buffer))))

```

key binding

```elisp
(global-set-key (kbd "C-c n o") 'open-note)
```
