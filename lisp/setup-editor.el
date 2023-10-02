;;(setq load-verbose t)
;;(setq debug-on-error t)

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
(menu-bar-mode -1)

(set-face-foreground 'vertical-border
                     (face-background 'vertical-border nil t))


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
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2)  ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

(global-unset-key (kbd "C-x m"))
;;(global-set-key (kbd "C-x m") 'multi-term)

(global-set-key (kbd "C-x v v") 'ivy-push-view)
(global-set-key (kbd "C-x v p") 'ivy-pop-view)

(global-set-key (kbd "C-x c e") 'eval-region)
(global-set-key (kbd "C-x c v") 'visual-line-mode)
(global-set-key (kbd "C-x c a") 'aggressive-indent-indent-defun)

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(yas-reload-all)

;;(setq company-tooltip-align-annotations t)

(global-unset-key (kbd "C-x a"))
(global-unset-key (kbd "C-x r"))
(global-unset-key (kbd "C-t")) ;; tranpose
(global-unset-key (kbd "C-h")) ;; help
(global-unset-key (kbd "C-x C-t")) ;;transpse line

(provide 'setup-editor)

;; entity to move over backward forward
;; character C-b C-f
;; word M-b M-f
;; line C-p C-n
;; go to line beginning (or end) C-a C-e
;; sentence M-a M-e
;; paragraph M-{ M-}
;; page C-x [ C-x ]
;; sexp C-M-b C-M-f
;; function C-M-a C-M-e
;; go to buffer beginning (or end) M-< M->
;; scroll to next screen C-v
;; scroll to previous screen M-v
;; scroll left C-x <
;; scroll right C-x >
;; scroll current line to center, top, bottom C-l
;; goto line M-g g
;; goto char M-g c
;; back to indentation M-m

;; uppercase word M-u
;; lowercase word M-l
;; capitalize word M-c
;; uppercase region C-x C-u
;; lowercase region C-x C-l

;; character (delete, not kill) DEL C-d
;; word M-DEL M-d
;; line (to end of) M-0 C-k C-k
;; sentence C-x DEL M-k
;; sexp M-- C-M-k C-M-k
;; kill region C-w
;; copy region to kill ring M-w
;; kill through next occurrence of char M-z char
;; yank back last thing killed C-y
;; replace last yank with previous kill M-y

;; indent current line (mode-dependent) TAB
;; indent region (mode-dependent) C-M-\
;; indent sexp (mode-dependent) C-M-q
;; indent region rigidly arg columns C-x TAB
;; indent for comment M-;
;; insert newline after point C-o
;; move rest of line vertically down C-M-o
;; delete blank lines around point C-x C-o
;; join line with previous (with arg, next) M-^
;; delete all white space around point M-\
;; put exactly one space at point M-SPC
;; fill paragraph M-q
;; set fill column to arg C-x f
;; set prefix each line starts with C-x .
;; set mark here C-@ or C-SPC
;; exchange point and mark C-x C-x
;; set mark arg words away M-@
;; mark paragraph M-h
;; mark page C-x C-p
;; mark sexp C-M-@
;; mark function C-M-h
;; mark entire buffer C-x h

;; search forward C-s
;; search backward C-r
;; regular expression search C-M-s
;; reverse regular expression search C-M-r
;; select previous search string M-p
;; select next search string M-n
;; exit incremental search RET
;; undo effect of last character DEL
;; abort current search C-g

;; When two commands are shown, the second is a similar command for a frame instead of a window.
;; delete all other windows C-x 1 C-x 5 1
;; split window, above and below C-x 2 C-x 5 2
;; delete this window C-x 0 C-x 5 0
;; split window, side by side C-x 3
;; scroll other window C-M-v
;; switch cursor to another window C-x o C-x 5 o
;; select buffer in other window C-x 4 b C-x 5 b
;; display buffer in other window C-x 4 C-o C-x 5 C-o
;; find file in other window C-x 4 f C-x 5 f
;; find file read-only in other window C-x 4 r C-x 5 r
;; run Dired in other window C-x 4 d C-x 5 d
;; find tag in other window C-x 4 . C-x 5 .
;; grow window taller C-x ^
;; shrink window narrower C-x {
;; grow window wider C-x }

;; interactively replace a text string M-%
;; using regular expressions M-x query-replace-regexp
;; add global abbrev C-x a g
;; add mode-local abbrev C-x a l
;; add global expansion for this abbrev C-x a i g
;; add mode-local expansion for this abbrev C-x a i l
;; explicitly expand abbrev C-x a e
;; expand previous word dynamically M-/
;; execute a shell command M-!
;; execute a shell command asynchronously M-&
;; select another buffer C-x b
;; list all buffers C-x C-b
;; kill a buffer C-x k
;;  eval sexp before point C-x C-e
;; eval current defun C-M-x
;; eval region M-x eval-region
;; read and eval minibuffer M-:
;; load a Lisp library from load-path M-x load-library


;; save region in register C-x r s
;; insert register contents into buffer C-x r i
;; save value of point in register C-x r SPC
;; jump to point saved in register C-x r j

;; remove help window C-x 1
;; scroll help window C-M-v
;; apropos: show commands matching a string C-h a
;; describe the function a key runs C-h k
;; describe a function C-h f
;; get mode-specific information C-h m


;; read a file into Emacs C-x C-f
;; save a file back to disk C-x C-s
;; save all files C-x s
;; insert contents of another file into this buffer C-x i
;; replace this file with the file you really want C-x C-v
;; write buffer to a specified file C-x C-w
;; toggle read-only status of buffer C-x C-q

;; iconify Emacs (or suspend it in terminal) C-z
;; exit Emacs permanently C-x C-c
