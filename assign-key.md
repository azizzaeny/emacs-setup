## configure tmux send region 

```elisp 

(global-set-key (kbd "C-c t f") 'tmux-send-region-or-paragraph-with-cat)
(global-set-key (kbd "C-c t t") 'tmux-set-target)          ;; Set target
(global-set-key (kbd "C-c t r") 'tmux-send-region-to-repl) ;; Send region
(global-set-key (kbd "C-c t l") 'tmux-send-line-to-repl)   ;; Send line
(global-set-key (kbd "C-c t p") 'tmux-send-paragraph-to-repl) ;; Send paragraph
(global-set-key (kbd "C-c t c") 'tmux-send-region-or-paragraph-with-cat) 
(global-set-key (kbd "C-c t d") 'tmux-send-control-c)      ;; Send C-d
(global-set-key (kbd "C-c t z") 'tmux-send-control-z)      ;; Send C-z

```

## pull and sync remote 

```elisp 
(global-set-key (kbd "C-x C-u") 'pull-file-from-remote)
(global-set-key (kbd "C-x C-y") 'sync-file-to-remote)
```

## reload markdown 
;; (global-unset-key (kbd "C-x r"))
;; (global-set-key (kbd "C-x r m") 'reload-markdown)
set key binding

```elisp 
;; saving async
;;(global-set-key (kbd "C-x C-s") 'async-save-buffer)

;; Multi Cursor
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
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-r"))
(global-set-key (kbd "C-s r") 'isearch-backward)
(global-set-key (kbd "C-s s") 'isearch-forward)
(global-set-key (kbd "C-s f") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s d") 'counsel-grep-or-swiper)
(global-set-key (kbd "M-x") 'counsel-M-x) ;; todo: changes default search key bind
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-unset-key (kbd "C-x d"))
(global-set-key (kbd "C-x f") 'ido-dired)

;; moving
(global-unset-key (kbd "C-x g")) 
(global-set-key (kbd "C-x g l") 'goto-line)
(global-set-key (kbd "C-x g <up>") 'windmove-up)
(global-set-key (kbd "C-x g <down>") 'windmove-down)
(global-set-key (kbd "C-x g <left>") 'windmove-left)
(global-set-key (kbd "C-x g <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
;; moving expression

;; (global-set-key (kbd "C-f") 'forward-sexp)
;; (global-set-key (kbd "C-b") 'backward-sexp)

;; Jump to Matching Parentheses
;; Place the cursor near a parenthesis and press C-M-u to move outwards, or C-M-d to move inwards.
;; C-M-n Move to next block C-M-d
;; deleting
(global-unset-key (kbd "C-d"));
(global-set-key (kbd "C-d d") 'delete-horizontal-space);
(global-set-key (kbd "C-d f") 'delete-block-forward)
(global-set-key (kbd "C-d b") 'delete-block-backward)
(global-set-key (kbd "C-d l") 'kill-whole-line)
(global-set-key (kbd "C-d t") 'transpose-lines)
(global-set-key (kbd "C-d <down>") 'move-line-down)
;;(global-set-key (kbd "C-d <up>") 'move-line-up)
(global-set-key (kbd "C-d j") 'join-line)

;; (global-set-key (kbd "C-c c f") 'polymode-next-chunk)
;; markdown
(global-set-key (kbd "C-x m f") 'polymode-next-chunk)
(global-set-key (kbd "C-x m p") 'polymode-previous-chunk)
;; (global-set-key (kbd "C-c C-f") 'markdown-forward-same-level)
;; (global-set-key (kbd "C-c C-p") 'markdown-backward-same-level)
;; (global-set-key (kbd "C-c m") 'polymode-mark-or-extend-chunk)

;; push view
(global-set-key (kbd "C-x v v") 'ivy-push-view)
(global-set-key (kbd "C-x v p") 'ivy-pop-view)
(global-set-key (kbd "C-x v s") 'ivy-switch-view)

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


;; restclient
;;(global-set-key (kbd "C-x r s") 'restclient-http-send-current-stay-in-window)
(global-set-key (kbd "C-x r s") 'restclient-http-send-current-suppress-response-buffer)

;; change web mode 
;; (global-set-key (kbd "C-x w") 'web-mode)


;; popper 
(global-unset-key (kbd "C-x p")) 
(global-set-key (kbd "C-x p o") 'my-popup-toggle) ; Toggle or open popup
(global-set-key (kbd "C-x p c") 'my-popup-close)  ; Close popup


;; code folding
;; (global-set-key (kbd "C-c b m") 'hs-minor-mode)
;; (global-set-key (kbd "C-c b h") 'hs-hide-block)
;; (global-set-key (kbd "C-c b s") 'hs-show-block)

;; align
(global-set-key (kbd "C-c a c") 'align-to-colon)
(global-set-key (kbd "C-c a e") 'align-to-equals)
(global-set-key (kbd "C-c a a") 'aggressive-indent-indent-defun)

;; expand region
(global-set-key (kbd "C-c e e") 'er/expand-region)

(global-set-key (kbd "C-c s r") 'reload-snippets)
(global-set-key (kbd "C-c s e") 'expand-abbrev-snippet)
(global-set-key (kbd "C-c s l") 'region-to-single-line)

;; bookmark
;; C-x r m ;; make a bookmark
;; C-x r b ;; jump to location
;; C-x r l ;; list all bookmark
;; M-x bookmark-save
;; M-x bookamrk-delete

;; the notes
(global-set-key (kbd "C-c c n") 'note-open-today)
;; (global-set-key (kbd "C-c n 1") 'note-open-yesterday)
;; (global-set-key (kbd "C-c n 2") 'note-open-n2)
;; (global-set-key (kbd "C-c n 3") 'note-open-n3)
;; (global-set-key (kbd "C-c n 4") 'note-open-n4)
;; (global-set-key (kbd "C-c n 5") 'note-open-n5)
;; (global-set-key (kbd "C-c n 6") 'note-open-n6)
;; (global-set-key (kbd "C-c n 7") 'note-open-n7)

;; ansi-term
(global-set-key (kbd "C-x p a") 'create-ansi-proc)

;; rename buffer
(global-unset-key (kbd "C-x p r")) ;; query replace
(global-set-key (kbd "C-x p b") 'rename-buffer)
;; previous bufer & nex buffer
;; <C-prior> 'previous-buffer
;; <C-next> 'next-buffer

;; the repl
;; (global-set-key (kbd "C-x p b") 'create-browser-repl)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x c 1") 'repl-set-default-proc)
(global-set-key (kbd "C-x c 2") 'repl-set-second-proc)
(global-set-key (kbd "C-x c n") 'repl-set-mark)
(global-set-key (kbd "C-c c k") 'repl-send-mark)
(global-set-key (kbd "C-c c s") 'repl-send-last-exp)
(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c m") 'repl-send-markdown-block)

;; git
(global-unset-key (kbd "C-x p p")) ;; select project
(global-set-key (kbd "C-c p h") 'git-push)
(global-set-key (kbd "C-x p c") 'git-commit)
(global-set-key (kbd "C-x p g") 'create-git-proc)

;; simple-httpd
;;(global-set-key (kbd "C-c c s") 'httpd-start-with-port)

```
