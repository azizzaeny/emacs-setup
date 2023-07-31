(defvar md-block-header "^```elisp")
(defvar md-block-end "^```$")

(defun load-markdown (file-paths &optional evaluator)  
  (interactive)  
  (when (file-exists-p (expand-file-name file-paths))	
	(with-temp-buffer	  
	  (insert-file-contents file-paths)
	  (goto-char (point-min))
	  (while (not (eobp))
		(forward-line 1)
		(let ((starting-pos (progn (re-search-forward md-block-header (point-max) t)
								   (match-end 0)))
			  (end-pos (progn (re-search-forward md-block-end (point-max) t)
							  (match-beginning 0))))
		  (if evaluator
			  (funcall evaluator starting-pos end-pos)
			(eval-region starting-pos end-pos)))))))


(defun write-to-file (file-location contents)
  (interactive)
  (unless (file-exists-p file-location)
	(let ((dir (file-name-directory file-location)))
	  (unless (file-exists-p dir)
		(make-directory dir t))))
  (with-temp-buffer
	(insert contents)
	(write-region (point-min) (point-max) file-location)))

(defun search-first-emacs-lisp ()
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (let (( s (progn (re-search-forward "^```emacs-lisp" (point-max) t) (match-end 0) (forward-line 1) (point)))
	    ( e (progn (re-search-forward "^```$" (point-max) t) (match-beginning 0))))
	(buffer-substring-no-properties s e)))))

;; (search-first-emacs-lisp) 1093 1836

(defvar init-el-location  "~/.emacs.d/init.el")
(defvar init-md-location  "~/.emacs.d/emacs.md")

(defun generate-init-el ()
  (interactive)
  (let ((source (search-first-emacs-lisp))
		(location init-el-location))
	(write-to-file location source)))

(defun generate-emacs-md ()
  (interactive)
  (let ((source (buffer-substring-no-properties (point-min) (point-max))))
    (write-to-file init-md-location source)))

(load-markdown "./readme.md")
