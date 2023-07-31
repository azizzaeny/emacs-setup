### Repl Send String
Common pattern to use Sending string to repl to evaluate or aka: process simulation in ansiterm


#### ansi-term as the repl process 
(explain the idea)


```elisp

(defvar repl-active-window "*ansi-term*")
(defvar repl-bin-sh "/usr/bin/zsh")
(defvar repl-wrap-txt "%s")

(defun repl-start (&optional repl-name init-script)
  (interactive)
  (let ((current-buffer-window (selected-window)))
	(if (not repl-name)
		(setq repl-active-window "*ansi-term*")
	  (setq repl-active-window repl-name))	
	(progn (ansi-term repl-bin-sh)
		   (when init-script
			 (insert init-script)
			 (sit-for 0.1)
			 (term-send-string repl-active-window "\n"))
		   (select-window selected-window))))

(defun repl-send-str (input-str &optional repl-name init-script)
  (interactive)
  (let ((current-buffer-window (selected-window))
        (format-str (format repl-wrap-txt input-str)))
    (term-send-string repl-active-window format-str)
	(term-send-string repl-active-window "\n")
    (select-window current-buffer-window)))

```

#### Extend the idea
add sending line, sending paragrhaph, smart sending by analyzing code  

```elisp
 
(defun repl-send-line ()
  (interactive)
  (save-excursion
	(let ((init-p (point)))
	  (beginning-of-line)
	  (set-mark (point))
	  (end-of-line)
	  (repl-send-str (buffer-substring-no-properties (point) (mark)))
	  (sit-for .1)
	  (setq mark-active nil)
	  (goto-char init-p))))  


(defun repl-send-paragraph ()
  (interactive)
  (save-excursion
	(let ((init-p (point)))
	  (re-search-backward "\n[\t\n ]*\n+" nil t)
	  (skip-chars-backward "\n\t ")
	  (forward-char)
	  (set-mark (point))
	  (repl-send-str (buffer-substring-no-properties (point) init-p))
	  (sit-for .1)
	  (setq mark-active nil)
	  (goto-char init-p))))

(defun repl-send-buffer ()
  (interactive)
  (repl-send-str (buffer-substring-no-properties (point-min) (point-max))))

(defun repl-send-region ()
  (interactive)
  (save-excursion
	(when (and transient-mark-mode mark-active)
	  (repl-send-str (buffer-substring-no-properties (point) (mark))))
	(sit-for .1)
	(setq mark-active nil)))

(defun repl-send-wrap (input-str)
  (interactive)
  (repl-send-str (format repl-wrap-txt input-str)))

(defun repl-send-wrap-line ()
  (interactive)  
  (save-excursion
	(let ((init-p (point)))
	  (beginning-of-line)
	  (set-mark (point))
	  (end-of-line)
	  (repl-send-wrap (buffer-substring-no-properties (point) (mark)))
	  (setq mark-active nil)
	  (goto-char init-p))))

(defun repl-send-lisp ()
  (interactive)
  (set-mark (line-beginning-position))
  (forward-sexp)
  (repl-send-str (buffer-substring-no-properties (point) (mark)))
  (setq mark-active nil)
  (forward-sexp))

(defun repl-send-md-block ()
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))    
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward "```" starting-pos t) (match-string 1)) nil))
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
          (repl-send-str (buffer-substring-no-properties start-content end-pos)))
        )))


(defun repl-send-js ())
(defun repl-send-python ())
(defun repl-send-all-md-block ())
(defun repl-send-clojure ())
(defun repl-send-process-exit ()
  (interactive)
  (repl-send-str "process.exit(0);"))
 
```

### key binding

```elisp

(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c s") 'repl-start)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)

```
