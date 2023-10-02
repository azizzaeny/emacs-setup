
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
    markdown-mode yasnippet yaml-mode))


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


;;(setq server-use-tcp t)
;;(setq server-host "iodev")
(server-start)

(require 'setup-editor)
(require 'setup-search)
(require 'setup-lisp)
(require 'setup-markdown)
(require 'setup-javascript)
(require 'setup-align)
(require 'setup-repl)

(provide 'setup)

