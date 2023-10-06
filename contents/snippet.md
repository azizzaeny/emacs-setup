setting up snippets

```elisp

;; simple versions
(defvar custom-snippets-file-path (expand-file-name "snippets.json" user-emacs-directory)
  "Path to the custom snippets JSON file.")

(defun load-custom-snippets ()
  "Load custom snippets from JSON file."
  (when (file-exists-p custom-snippets-file-path)
    (let ((json-object-type 'hash-table))
      (json-read-file custom-snippets-file-path))))

(defvar custom-snippets-hash (load-custom-snippets)
  "Hash table of custom snippets.")

(defun reload-snippets ()
  (interactive)
  (setq custom-snippets-hash (load-custom-snippets))
  (message "snippet reloaded"))

```

customize things at point word 

```elisp

(defun abbreviation-at-point ()
  "Return the abbreviation at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_:|-!<>")
                  (point))))
    (buffer-substring-no-properties start end)))

(defun custom-snippet-expand ()
  "Expand custom snippet."
  (interactive)
  (let* ((abbrev (abbreviation-at-point))
         ;;(abbrev (thing-at-point 'word))
         ;;(expansion (gethash abbrev custom-snippets-hash)))
         (relative-snippet-path (gethash abbrev custom-snippets-hash))
         (snippet-path (when relative-snippet-path
                         (expand-file-name relative-snippet-path user-emacs-directory)))
         (expansion (when snippet-path
                      (with-temp-buffer
                        (insert-file-contents snippet-path)
                        (buffer-string)))))
    (message snippet-path)
    (if expansion
        (progn
          (backward-kill-word 1)
          (insert expansion))
      (message "No snippet found for abbreviation: %s" abbrev))))

```

```elisp
(defun custom-snippet-eval ()
  "Evaluate the content of the snippet as Emacs Lisp code."
  (interactive)
  (let* ((abbrev (thing-at-point 'word))
         (relative-snippet-path (gethash abbrev custom-snippets-hash))
         (snippet-path (when relative-snippet-path
                         (expand-file-name relative-snippet-path user-emacs-directory)))
         (code (when snippet-path
                 (with-temp-buffer
                   (insert-file-contents snippet-path)
                   (buffer-string)))))
    (if code
        (eval (read code))
      (message "No code found for abbreviation: %s" abbrev))))

```

assign key binding into it

```elisp
(global-set-key (kbd "C-c s e") 'custom-snippet-expand)
(global-set-key (kbd "C-x x") 'custom-snippet-expand)
(global-set-key (kbd "C-c s v") 'custom-snippet-eval)
(global-set-key (kbd "C-c s r") 'reload-snippets)

;; snippet eval-markdown inside
;; snippet eval-html-code inside
;; perform repl send string of contents
;; perform socket repl send string of contents
```

