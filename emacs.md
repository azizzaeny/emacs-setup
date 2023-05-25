### Background
Whats the problem?. we need to generate a markdown files that markdown files itself contain init.el and load all emacs configuration. the configuration we generates also can be re-generate itself in the newer version.

How we do it?. first of all we are storing a configuration in the markdown files. that meaning we need to figure out how we identify the code blocks inside the markdown then we evaluate each block in sequences.

in order to re-generate and create initial of it. first we clone this document to specific folder on our local computer. we provide a way of installing or setting up the init.el in exact step. we go openup this files evaluates commands that generate init.el. we dont do syncing or copying out this files to ~/.emacs.d/init.el instead we generate it.


### Eamcs Shortcut

```txt
C-b backward one char
C-f forward one char
M-b backward one word
M-f forward one word
C-a begin line
C-e end line
C-p up line
C-n down line
M-< beginn document
M-> End Document
M-v up screen
C-v down screen
M-} down paragraph
M-{ up paragraph
M-h mark paragraph
C-Spc Begin Mark 
```

### Getting Started & Setup 
**Writing Emacs Configuration to The User Directory**  
Evaluate this line of codes in your emacs to start, this will create init.el and copy the emacs.d files to your `~/.emacs.d/` directory  
it would try to load emacs-lisp markdown identified syntax   

```emacs-lisp

(setq byte-compile-warnings '(cl-functions))

(setq ivy-use-virtual-buffers t)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

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

(defvar md-base-init "~/.emacs.d/emacs.md")
;; todo : fix load all markdown in the directory
(load-markdown md-base-init)

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

;;(generate-init-el)
;;(generate-emacs-md)

;; TODO: copy files or this loaded buffer into the emacs directory

```

### Base Setup Editor
setting up toolbar, tooltip, tab-width, editor behaviour, encoding, line-number-mode,

```elisp
  
;; frame window, behaviour 
(setq ns-pop-up-frames nil)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq use-dialog-box nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)
(setq ring-bel-function 'ignore)
(setq visible-bell t)
(setq custom-safe-themes t)
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq find-file-visit-truename t)
(setq large-file-warning-threshold (* 25 1024 1024))
(setq comment-style 'extra-line)
(fset 'yes-or-no-p 'y-or-n-p)
(setq redisplay-dont-pause t)

;; Backup files 

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)

;; line number and spacing

(setq line-number-mode t)
(setq indicate-empty-lines t)
(setq global-hl-line-mode t)
(setq tab-width 2)
(setq toggle-truncate-lines t)
(setq indent-tabs-mode nil)

;; Cursor, mouse-wheel, scroll

(set-default (quote cursor-type) t)
(blink-cursor-mode -1)

;; (global-whitespace-mode)
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#f9f9f9")))))

;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9])))

```

Scroll Behaviour 

```elisp
(defvar cursor-initial-color (face-attribute 'cursor :background))
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 2)  ;; keyboard scroll one line at a time
;; (setq scroll-conservatively 10000)
;; (setq scroll-preserve-screen-position t)
```

### Package Initialization

```elisp
(defun initialize-el-get ()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")  
  (unless (require 'el-get nil 'noerror)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))
(initialize-el-get)
(package-initialize)

(el-get-bundle github-theme
  :url "https://raw.githubusercontent.com/philiparvidsson/GitHub-Theme-for-Emacs/master/github-theme.el")

(el-get-bundle polymode/polymode
  :type github :pkgname "polymode/polymode")

(el-get-bundle polymode/poly-markdown
  :type github :pkgname "polymode/poly-markdown")

(el-get-bundle defunkt/markdown-mode
  :type github :pkgname "defunkt/markdown-mode")

(el-get-bundle mooz/js2-mode
  :type github :pkgname "mooz/js2-mode")

(el-get-bundle clojure-mode)
;; (el-get-bundle parinfer)
;; (el-get-bundle paredit)

(el-get-bundle rainbow-delimiters)
(el-get-bundle aggressive-indent)
;; (el-get-bundle smartparens)

(el-get-bundle fgallina/multi-web-mode
  :type github :pkgname "fgallina/multi-web-mode")

;; (el-get-bundle emmet-mode)
(el-get-bundle python-mode)
(el-get-bundle yasnippet)
;; (el-get-bundle gist)
(el-get-bundle counsel)
(el-get-bundle ivy)
(el-get-bundle auto-complete)
;; (el-get-bundle general)
(el-get-bundle php-mode)
 
```

### Setup Package
#### Ivy, Swiper, Counsel

```elisp

(require 'counsel)
(require 'ivy)
(require 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers nil)
;; (setq search-default-mode #'char-fold-to-regexp)
(setq ivy-initial-inputs-alist nil)
(setq ivy-count-format "")
(setq ivy-display-style nil)
(setq ivy-minibuffer-faces nil)
(add-to-list 'ivy-highlight-functions-alist
             '(swiper--re-builder . ivy--highlight-ignore-order))

(setq ivy-re-builders-alist
	  '((ivy-switch-buffer . ivy--regex-plus)
		(swiper . ivy--regex-plus)))

(setq company-tooltip-align-annotations t)
  
```
#### Markdown , Poly Markdown 

```elisp

;; markdown-mode
(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; polymode poly-markdown
(require 'polymode)
(require 'poly-markdown)
(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

```

#### Clojure, Parens, Lisp 

```elisp
 
;; clojure-mode
(require 'clojure-mode)
(setq clojure-indent-style 'always-indent)
(setq comment-column 0)

;; show-paren
(require 'paren)
(setq show-paren-delay 0.4)
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

```

#### Javascript , MutliWeb, Tramp, Other

```elisp

(setq-default indent-tabs-mode nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 2 200 2))
;; (setq backward-delete-char-untabify-method 'hungry)

(set-default 'truncate-lines t)

;; multiweb
;;(require 'multi-web-mode)
;;(setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;;(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;(multi-web-global-mode 1)
;;(require 'php-mode)


(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

```
#### Setup Custom Key

```elisp

;; (general-define-key
;;   "C-g"     'minibuffer-keyboard-quit
;;   "C-s"     'counsel-grep-or-swiper
;;   "C-x C-f" 'counsel-find-file
;;   "C-x ag"  'counsel-ag
;;   ;; "C-x f"   'counsel-describe-function
;;   ;; "C-x l"   'counsel-find-library
;;   "C-x f" nil
;;   "C-x <left>" nil
;;   "C-x <right>" nil)


(global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x a g") 'counsel-ag)
(global-set-key (kbd "C-x f") 'counsel-find-library)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

;; (defvar custom-bindings-map (make-;; keymap)
;;   "A keymap for custom bindings.")
;; (defconst custom-key "C-c")

;; (general-create-definer
;;   custom-key :prefix "C-c")

;; (define-minor-mode custom-bindings-mode
;;   "A mode that activates custom-bindings."
;;   t nil custom-bindings-map)

;; (custom-key
;;   "m e" 'mc/edit-lines
;;   "m n" 'mc/mark-next-lines
;;   "m e" 'emmet-expand-line)

;; (custom-bindings-mode 1)               
```

#### Repl 

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

;; TODO:
;; Create Marked things that can be swith able, so we dont define like send-process-exit, like a bookmark this things, so we have shortcut to sending all of this

```

#### Tangle Markdowns

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

#### Open Notes

```elisp

(defun open-note ()
  (interactive)
  (let ((out-dir (format "%s-%s.md" "notes" (format-time-string "%Y%m%d" (current-time)))))
	(if (file-exists-p (expand-file-name (concat "~/Desktop/active/daily-notes" out-dir)))
		(find-file out-dir)
	  (find-file out-dir)
	  (write-to-file out-dir (concat "## Notes - " (format-time-string "%Y%m%d" (current-time)))))
	))
```

#### Set Custom key 
This Line should be placed at the bottom to set custom-key

```elisp

(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l r") 'eval-region)

(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c s") 'repl-start)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)
(global-set-key (kbd "C-c c t") 'tangle-md-block)

(global-set-key (kbd "C-x v") 'ivy-push-view)
(global-set-key (kbd "C-c c a") 'aggressive-indent-indent-defun)

```
