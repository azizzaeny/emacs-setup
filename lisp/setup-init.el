
;; more require here then do inside the each files

(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(defvar
  my-packages
  '(
    clojure-mode counsel ivy
    auto-complete python-mode js2-mode php-mode
    aggressive-indent polymode poly-markdown
    expand-region
    markdown-mode yaml-mode))

;; parinfer
;; paredit
;; rainbow-delimiters
;; smartparens
;; multi-web-mode
;; emmet-mode
;; yasnippet
;; github-theme

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(setq load-verbose t)
(setq debug-on-error t)

;; (setq server-use-tcp t)
;; (setq server-host "iodev")
(server-start)

(defun load-markdown (file-paths &optional evaluator)
  (interactive)
  (when (file-exists-p (expand-file-name file-paths user-emacs-directory))
    (with-temp-buffer
      (insert-file-contents file-paths)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line 1)
        (let ((start-pos (progn (re-search-forward "^```elisp" (point-max) t) (match-end 0)))
              (end-pos (progn (re-search-forward "^```$" (point-max) t) (match-beginning 0))))
          (if evaluator
              (funcall evaluator start-pos end-pos)
            (eval-region start-pos end-pos)))))))

(load-markdown ".../contents/01-setup-editor.md")
(load-markdown "../contents/02-search-text.md")
(load-markdown "../contents/03-programming.md")
(load-markdown "../contents/04-repl.md")
(load-markdown "../contents/05-snippet.md")
(load-markdown "../contents/06-align.md")

               
(provide 'setup-init)
