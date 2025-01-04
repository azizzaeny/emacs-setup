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
    vscode-dark-plus-theme    
    clojure-mode
    counsel
    ivy
    aggressive-indent
    polymode
    poly-markdown
    expand-region
    markdown-mode
    multiple-cursors
    python-mode
    js2-mode
    php-mode
    emmet-mode
    web-mode
    mmm-mode
    nginx-mode
    dotenv-mode
    json-mode
    yaml-mode
    restclient
    company
	async
    )
  )
;; todo use gptel package to interact with llm
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
;; visual-regexp
;; ido-at-point
;; ido-completing-read+
;; ido-vertical-mode
;; restclient
;; helm
;; find-file-in-project
;; page-break-lines
;; auto-complete

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
