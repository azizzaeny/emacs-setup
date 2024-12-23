expand abbreviations custom snippets

```elisp
(defvar snippets (make-hash-table :test 'equal))

(defun load-snippets (snippets-file)
  "Load snippets from the Markdown file into the snippets hash table."
  (interactive)
  (if (file-exists-p (expand-file-name snippets-file))
      (with-temp-buffer
        (insert-file-contents (expand-file-name snippets-file))
        (goto-char (point-min))
        (while (re-search-forward md-block-header-snippet nil t)
          (let ((name (match-string 2))
                (start (progn (forward-line) (point)))
                (end (progn (re-search-forward md-block-end-snippet nil t)
                            (line-beginning-position))))
            (puthash name
                     (string-trim (buffer-substring-no-properties start end))
                     snippets))))
    (message "Snippets file not found: %s" (expand-file-name snippets-file))))


(defun reload-snippets ()
  (interactive)
  (load-snippets "~/.emacs.d/docs/snippet.md"))

(defun abbrev-at-point ()
  "Return the abbreviation at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_:|-!<>")
                  (point))))
    (buffer-substring-no-properties start end)))

(defun insert-content-abbrev (content)
  (interactive)
  (progn
    (backward-kill-word 1)
    (insert content)))

(defun expand-abbrev-snippet ()
  (interactive)
  (let* ((abbrev (abbrev-at-point))
         (snippet-content (gethash abbrev snippets)))
    (if snippet-content
        (insert-content-abbrev snippet-content))))

(defun region-to-single-line ()
  "Replace newlines in region with '\\n' and concatenate into a single line."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char start)
        (let ((region-text (buffer-substring-no-properties start end)))
          (setq region-text (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\"" "\\\\\"" region-text)))
          (kill-new region-text))))))

```

