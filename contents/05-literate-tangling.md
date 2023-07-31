
## Literate programming in markdown
(todo: explain the idea)

#### tangling
(explaint what is tangling)  

```elisp

(defvar tangle-md-file-ref "path=\\([^\s+]+\\)")

(defun tangle-md-block ()
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))    
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward tangle-md-file-ref starting-pos t) (match-string 1)) nil))			
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
        (when file-ref
          (write-to-file file-ref (buffer-substring-no-properties start-content end-pos)))
        ))))

(defun tangle-md-buffer ())

```

#### key bind 

```elisp
(global-set-key (kbd "C-c c t") 'tangle-md-block)

```
