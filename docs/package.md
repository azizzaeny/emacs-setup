refresh package contents

```elisp

(require 'package)
(require 'cl-lib)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

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
    expand-region
    vscode-dark-plus-theme
    treemacs 
    markdown-mode
    multiple-cursors
    simple-httpd
    emmet-mode
    web-mode
    rjsx-mode
    xref-js2
    mmm-mode
    nginx-mode
    dotenv-mode
    json-mode
    yaml-mode
    )
  )
```

configure the package

;; usefull programming elisp
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

;; this required but not yet
;; parinfer
;; paredit
;; rainbow-delimiters
;; smartparens
;; multi-web-mode
;; emmet-mode
;; yasnippet
;; github-theme
;; iy-go-to-char

```elisp

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

```

adding the list manual package

```elisp
(add-to-list 'load-path "~/.emacs.d/manual-packages/")
```
