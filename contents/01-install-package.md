## Package

#### load melpa packages
add lists to package archives, gnu and melpa . 

```elisp
(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

```

#### install package
todo: simplify the lists   

```elisp 

(defvar
  my-packages
  '(
    clojure-mode
    counsel
    ivy
    auto-complete
    python-mode
    js2-mode
    php-mode
    aggressive-indent
    polymode
    poly-markdown
    markdown-mode
    yaml-mode ))

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

```

