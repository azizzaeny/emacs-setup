
themes set, divider, bottom border

```elisp

(setq window-divider-default-places 'right-only)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(load-theme 'vscode-dark-plus t)
(window-divider-mode 1)

;; balck bottom border bg status
(custom-set-faces
 '(mode-line ((t (:background "#000000" :foreground "#ffffff"))))
 '(mode-line-inactive ((t (:background "#000000" :foreground "#888888")))))

;; font 
(set-face-attribute 'default nil :font "Source Code Pro for powerline")

```

visual apperaence
 
```elisp

(menu-bar-mode -1)

(set-face-foreground 'vertical-border
                     (face-background 'vertical-border nil t))

;;(display-line-numbers-mode 1)
(global-display-line-numbers-mode 1)

(setq line-number-mode t)
(setq indicate-empty-lines t)
(setq global-hl-line-mode nil)
;;(global-hl-line-mode nil)

(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq toggle-truncate-lines t)
(setq indent-tabs-mode nil)
(blink-cursor-mode -1)

```

configure the behaviour

```elisp
(setq ring-bel-function 'ignore)
(setq visible-bell t)
(setq custom-safe-themes t)
(setq ns-pop-up-frames nil)
(setq frame-title-format nil)
(setq find-file-visit-truename t)
(setq large-file-warning-threshold (* 25 1024 1024))
(setq comment-style 'extra-line)
(fset 'yes-or-no-p 'y-or-n-p)

```

backup

```elisp

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)

```


scroll

```elisp

(defvar cursor-initial-color (face-attribute 'cursor :background))
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2)  ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
```
