
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
    markdown-mode
    yaml-mode
    ))

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

(defvar md-base-init "/home/aziz/test.md")
;; todo : fix load all markdown in the directory
;; (load-markdown md-base-init)

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

;; (generate-init-el)
;; (generate-emacs-md)

;; TODO: copy files or this loaded buffer into the emacs directory
(setq byte-compile-warnings '(cl-functions))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)

(setq ring-bel-function 'ignore)
(setq visible-bell t)
(setq custom-safe-themes t)
(setq ns-pop-up-frames nil)
(setq frame-title-format nil)
(setq find-file-visit-truename t)
(setq large-file-warning-threshold (* 25 1024 1024))
(setq comment-style 'extra-line)
(fset 'yes-or-no-p 'y-or-n-p)

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
(blink-cursor-mode -1)

(defvar cursor-initial-color (face-attribute 'cursor :background))
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 2)  ;; keyboard scroll one line at a time
;; (setq scroll-conservatively 10000)
;; (setq scroll-preserve-screen-position t)

;; setup pacakge

;; (require 'counsel)
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


(setq-default indent-tabs-mode nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 2 200 2))
;; (setq backward-delete-char-untabify-method 'hungry)
(set-default 'truncate-lines t)


;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


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


(defun open-note ()
  (interactive)
  (let ((out-dir (format "%s-%s.md" "notes" (format-time-string "%Y%m%d" (current-time)))))
	(if (file-exists-p (expand-file-name (concat "~/Desktop/active/daily-notes" out-dir)))
		(find-file out-dir)
	  (find-file out-dir)
	  (write-to-file out-dir (concat "## Notes - " (format-time-string "%Y%m%d" (current-time)))))
	))

(defun align-to-colon (begin end)
  "Align region to colon"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":"  ) 1 1 ))

(defun align-to-comma (begin end)
  "Align region to comma signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(defun align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

;; work with this
(defun align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))


(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l r") 'eval-region)

(global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-find-library)
(global-set-key (kbd "C-x a") 'ansi-term)
(global-unset-key (kbd "C-x m"))
;;(global-set-key (kbd "C-x m") 'multi-term)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x v") 'ivy-push-view)

(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c s") 'repl-start)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)
(global-set-key (kbd "C-c c t") 'tangle-md-block)

(global-set-key (kbd "C-c c a") 'aggressive-indent-indent-defun)
(global-set-key (kbd "C-c c c") 'align-to-colon)
(global-set-key (kbd "C-c c e") 'align-to-equals)

(global-set-key (kbd "C-c c g") 'generate-emacs-md)

(server-start)
