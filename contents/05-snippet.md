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

(defun custom-snippet-expand ()
  "Expand custom snippet."
  (interactive)
  (let* ((abbrev (thing-at-point 'word))
         ;;(expansion (gethash abbrev custom-snippets-hash)))
         (relative-snippet-path (gethash abbrev custom-snippets-hash))
         (snippet-path (when relative-snippet-path
                         (expand-file-name relative-snippet-path user-emacs-directory)))
         (expansion (when snippet-path
                      (with-temp-buffer
                        (insert-file-contents snippet-path)
                        (buffer-string)))))
    (if expansion
        (progn
          (backward-kill-word 1)
          (insert expansion))
      (message "No snippet found for abbreviation: %s" abbrev))))

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

(global-set-key (kbd "C-c s e") 'custom-snippet-expand)
(global-set-key (kbd "C-c s r") 'custom-snippet-eval)

```
