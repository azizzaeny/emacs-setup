reloading markdown

```elisp
(defun reload-markdown ()
  (interactive)
  (load-markdown "~/.emacs.d/docs/package.md")
  (load-markdown "~/.emacs.d/docs/configure-package.md")
  (load-markdown "~/.emacs.d/docs/visual.md")
  (load-markdown "~/.emacs.d/docs/simple-httpd.md")
  (load-markdown "~/.emacs.d/docs/copy-paste.md")
  (load-markdown "~/.emacs.d/docs/note.md")
  (load-markdown "~/.emacs.d/docs/expand-abbrev.md")
  (load-markdown "~/.emacs.d/docs/repl.md")
  (load-markdown "~/.emacs.d/docs/align.md")
  (message "reloaded-markdown"))

(global-unset-key (kbd "C-x r"))
(global-set-key (kbd "C-x r m") 'reload-markdown)
```

company 

```elisp
(require 'company)
(global-company-mode)
(setq company-idle-delay
      (lambda () (if (company-in-string-or-comment) nil 0.1)))

(setq company-global-modes '(not erc-mode message-mode eshell-mode))
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-annotation-padding 1)
(setq company-tooltip-limit 4)
(setq company-format-margin-function 'company-text-icons-margin)
(company-preview-frontend t)
(company-preview-common-frontend t)


```
isearch

```elisp
(require 'restclient);
(require 'isearch)
(setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)

(setq search-highlight t)
(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0.5)
(setq lazy-highlight-no-delay-length 4)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
(setq isearch-repeat-on-direction-change t)
(setq isearch-case-fold-search t);; incasesensitive

(defun isearch-current-word ()
  "Perform an incremental search forward for the current word under the cursor."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (progn
          (isearch-mode t)
          (isearch-yank-string word))
      (isearch-forward))))

```
dired

```elisp
;;(require 'dired-details)
;; (setq-default dired-details-hidden-string "---")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
;;(setq dired-dwim-target t)
;;(dired-details-install)
```
uniqufy
```elisp

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

```

counsel ivy swiper, ido


```elisp

;; (require 'counsel)
(require 'ivy)
(require 'swiper)
(ivy-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(setq ivy-use-virtual-buffers t)
(setq ido-use-virtual-buffers t)
(setq ido-use-faces t)
(setq enable-recursive-minibuffers nil)
(setq ivy-initial-inputs-alist nil)
(setq ivy-count-format "")
(setq ivy-display-style nil)
(setq ivy-minibuffer-faces nil)

;; (add-to-list 'ivy-highlight-functions-alist
;;              '(swiper--re-builder . ivy--highlight-ignore-order))

;; (setq ivy-re-builders-alist
;; 	  '((ivy-switch-buffer . ivy--regex-plus)
;; 		(swiper . ivy--regex-plus)))

;; (setq search-default-mode #'char-fold-to-regexp)
;; (unless (ivy-state-dynamic-collection ivy-last)
;;   (completion-metadata "" minibuffer-completion-table minibuffer-completion-predicate))



```
iy-go-to-char, find-file-in-project
```elisp
(require 'iy-go-to-char)
(setq ffip-prefer-ido-mode t)

```

expand-region

```elisp

(require 'expand-region)

```


clojure paren 

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

polymarkdown, markdown

```elisp

;; polymode poly-markdown
(require 'polymode)
(require 'poly-markdown)
(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

;; markdown-mode
(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

```

js2 mode indent

```elisp
(setq-default indent-tabs-mode nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 2 200 2))
(set-default 'truncate-lines t)

(with-eval-after-load 'js2-mode
  (add-to-list 'auto-mode-alist '("\\.mjs" . js2-mode)))

```

multiple cursor

```elisp
(require 'multiple-cursors)

```

web-mode

```elisp

;; webmode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-script-padding 1)
(setq web-mode-style-padding 1)
(setq web-mode-block-padding 0)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(global-set-key (kbd "C-x w") 'web-mode)

;; (require 'company)
;; (ac-config-default)

```

delete word

```elisp
(require 'subword)

(defun delete-block-forward ()
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun delete-block-backward ()
  (interactive)
  (if (bobp)
      (message "Beginning of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-backward (string (char-syntax (char-before))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-backward)
              (point))))
      (kill-region (point) (max syntax-move-point subword-move-point)))))


(defun kill-whole-line ()
  "Kill the entire current line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun join-line ()
  "Join the following line to this one."
  (interactive)
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " "))

(defun move-line-down ()
  "Move the current line down by one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

```
key bind

```elisp
;; Multi Cursrs
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "C-x m")) ;; mail
(global-set-key (kbd "C-x m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x m p") 'mc/mark-previous-like-this)


;; Emmet
;; (global-set-key (kbd "C-j") 'emmet-expand-line)

(global-unset-key (kbd "C-t")) ;; tranpose
(global-unset-key (kbd "C-h")) ;; help
(global-unset-key (kbd "C-x C-t")) ;;transpse line

;; C-x u

;; (global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)

;; Ido Dired Ivy Swiper
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x r r") 'replace-string)
(global-set-key (kbd "C-r") 'isearch-backward)

(global-set-key (kbd "C-s") 'isearch-forward)
;;(global-set-key (kbd "C-s") 'isearch-current-word)
(global-set-key (kbd "M-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-unset-key (kbd "C-x d"))
(global-set-key (kbd "C-x f") 'ido-dired)

;; moving
(global-unset-key (kbd "C-x g")) 
(global-set-key (kbd "C-x g l") 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
;; moving expression
(global-set-key (kbd "C-f") 'forward-sexp)
(global-set-key (kbd "C-b") 'backward-sexp)
;; deleting
(global-unset-key (kbd "C-d"));
(global-set-key (kbd "C-d d") 'delete-horizontal-space)      ;
(global-set-key (kbd "C-d f") 'delete-block-forward)
(global-set-key (kbd "C-d b") 'delete-block-backward)
(global-set-key (kbd "C-d l") 'kill-whole-line)
(global-set-key (kbd "C-d <up>") 'transpose-lines)
(global-set-key (kbd "C-d <down>") 'move-line-down)
(global-set-key (kbd "C-d j") 'join-line)

;; markdown
;; (global-set-key (kbd "C-c f") 'polymode-next-chunk)
;; (global-set-key (kbd "C-c c f") 'polymode-next-chunk)
;; (global-set-key (kbd "C-c p") 'polymode-previous-chunk)
;; (global-set-key (kbd "C-c C-f") 'markdown-forward-same-level)
;; (global-set-key (kbd "C-c C-p") 'markdown-backward-same-level)
;; (global-set-key (kbd "C-c m") 'polymode-mark-or-extend-chunk)

;; push view
(global-set-key (kbd "C-x v v") 'ivy-push-view)
(global-set-key (kbd "C-x v p") 'ivy-pop-view)

;; evaling
;;(global-unset-key (kbd "C-x e"))
(global-set-key (kbd "C-x e") 'eval-region)


;; line number mode
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l l") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-x l v") 'visual-line-mode) ;; wrap-un-wrap text


;; iy-go-to-char
(global-set-key (kbd "C-x g f") 'iy-go-to-char)
(global-set-key (kbd "C-x g b") 'iy-go-to-char-backward)

;; expand region
;; (global-set-key (kbd "C-c e") 'er/expand-region)

;; restclient
(global-set-key (kbd "C-x r s") 'restclient-http-send-current-stay-in-window)


```
