(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings '(cl-functions))

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

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


(defvar md-block-header-snippet "^```\\(\\S-+\\)\\s-+name=\\(\\S-+\\)")
(defvar md-block-end-snippet "^```$")

(load-markdown "~/.emacs.d/docs/package.md")
(load-markdown "~/.emacs.d/docs/configure-package.md")
(load-markdown "~/.emacs.d/docs/visual.md")
(load-markdown "~/.emacs.d/docs/simple-httpd.md")
(load-markdown "~/.emacs.d/docs/copy-paste.md")
(load-markdown "~/.emacs.d/docs/note.md")
(load-markdown "~/.emacs.d/docs/expand-abbrev.md")
(load-markdown "~/.emacs.d/docs/repl.md")
(load-markdown "~/.emacs.d/docs/align.md")

;; (setq load-verbose t)
;; (setq debug-on-error t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-divider ((t (:foreground "dim gray"))))
 '(window-divider-first-pixel ((t (:foreground "dim gray"))))
 '(window-divider-last-pixel ((t (:foreground "dim gray")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors yaml-mode yasnippet expand-region poly-markdown polymode aggressive-indent php-mode js2-mode python-mode auto-complete counsel clojure-mode cmake-mode)))

