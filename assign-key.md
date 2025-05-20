we want c-c prefix 
we want c-x prefix 

## configure send region

```elisp 

(global-set-key (kbd "C-c t t") 'zaeny/tmux-set-runtime)
(global-set-key (kbd "C-c t e") 'zaeny/tmux-send-exp)
(global-set-key (kbd "C-c t r") 'zaeny/tmux-send-region) ;; or paragrpah
(global-set-key (kbd "C-c t l") 'zaeny/tmux-send-line) ;; or paragrpah
(global-set-key (kbd "C-c t c") 'zaeny/tmux-send-cat) ;; sent wraper cat
(global-set-key (kbd "C-c t b") 'zaeny/tmux-send-whole-buffer) ;; sent whole buffer
(global-set-key (kbd "C-c t p") 'zaeny/mark-python-code-block) ;; mark python code blocks
(global-set-key (kbd "C-c t m") 'zaeny/tmux-mark-point) ;; begin mark into key bind
(global-set-key (kbd "C-c t s") 'zaeny/tmux-send-mark) ;; sent mark into current runtime

;; (global-set-key (kbd "C-c t d") 'tmux-send-control-c)      ;; Send C-d
;; (global-set-key (kbd "C-c t z") 'tmux-send-control-z)      ;; Send C-z

(global-set-key (kbd "C-c w t") 'zaeny/ws-connect-debugger)
(global-set-key (kbd "C-c w c") 'zaeny/ws-close)
(global-set-key (kbd "C-c w e") 'zaeny/ws-send-evaluate)
(global-set-key (kbd "C-c w r") 'zaeny/ws-send-region)
(global-set-key (kbd "C-c w l") 'zaeny/ws-send-line)

;; todo create region, paragraph and line
```

## pull and sync remote 

```elisp 
;; (global-set-key (kbd "C-x C-u") 'zaeny/pull-file-from-remote)
;; (global-set-key (kbd "C-x C-y") 'zaeny/sync-file-to-remote)
```

## reload markdown 

```elisp 
;; (global-unset-key (kbd "C-x r"))
;; (global-set-key (kbd "C-x r m") 'zaeny/reload-markdown)
```


## multi cursor 
```elisp 

;; Emmet
;; (global-set-key (kbd "C-j") 'emmet-expand-line)

(global-unset-key (kbd "C-t")) ;; tranpose
(global-unset-key (kbd "C-h")) ;; help
(global-unset-key (kbd "C-x C-t")) ;;transpse line

;; Ido Dired Ivy Swiper
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "C-s") 'swiper)

(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-x d"))
(global-unset-key (kbd "C-x g"))

;; default behaviour search ido, switch
(global-set-key (kbd "C-c r r") 'replace-string)
(global-set-key (kbd "C-s r") 'isearch-backward)
(global-set-key (kbd "C-s s") 'isearch-forward)
(global-set-key (kbd "C-s w") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-s d") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-s x") 'counsel-M-x)
(global-set-key (kbd "C-s l") 'goto-line)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'ido-dired)

;; line search movement
(global-set-key (kbd "C-c s a") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-c s r") 'isearch-backward)
(global-set-key (kbd "C-c s s") 'isearch-forward)
(global-set-key (kbd "C-c s w") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-c s x") 'counsel-M-x)
(global-set-key (kbd "C-c s b") 'ido-switch-buffer)
(global-set-key (kbd "C-c s f") 'ido-find-file)
(global-set-key (kbd "C-c s d") 'ido-dired)
(global-set-key (kbd "C-c s l") 'goto-line)

;; go-to-char
(global-set-key (kbd "C-c s c") 'iy-go-to-char)
;;(global-set-key (kbd "C-x s b") 'iy-go-to-char-backward)

;; Moving Multi Cursor
(global-unset-key (kbd "C-c m")) ;; mail
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)

;; Moving transpose join
(global-set-key (kbd "C-c m t") 'transpose-lines)
(global-set-key (kbd "C-c m <down>") 'zaeny/move-line-down)
(global-set-key (kbd "C-c m <up>") 'zaeny/move-line-up)
(global-set-key (kbd "C-c m j") 'zaeny/join-line)

;; Moving expression
(global-set-key (kbd "C-c C-<right>") 'forward-sexp)
(global-set-key (kbd "C-c C-<left>") 'backward-sexp)
(global-set-key (kbd "C-c m f") 'forward-sexp)
(global-set-key (kbd "C-c m b") 'backward-sexp)

;; Deleting line  
(global-unset-key (kbd "C-d"));
(global-set-key (kbd "C-d d") 'delete-horizontal-space);
(global-set-key (kbd "C-d f") 'delete-block-forward)
(global-set-key (kbd "C-d l") 'kill-whole-line)
(global-set-key (kbd "C-d b") 'delete-block-backward)

;; (global-set-key (kbd "C-c c f") 'polymode-next-chunk)

;; markdown
;; (global-set-key (kbd "C-x m f") 'polymode-next-chunk)
;; (global-set-key (kbd "C-x m p") 'polymode-previous-chunk)
(global-set-key (kbd "C-c m f") 'markdown-forward-same-level)
(global-set-key (kbd "C-c m b") 'markdown-backward-same-level)
;; (global-set-key (kbd "C-c m") 'polymode-mark-or-extend-chunk)
;; (global-set-key (kbd "C-c m p") 'markdown-previous-visible-heading)
;; (global-set-key (kbd "C-c m n") 'markdown-next-visible-heading)


;; window movement
(global-set-key (kbd "C-x g <up>") 'windmove-up)
(global-set-key (kbd "C-x g <down>") 'windmove-down)
(global-set-key (kbd "C-x g <left>") 'windmove-left)
(global-set-key (kbd "C-x g <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

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


;; restclient
;;(global-set-key (kbd "C-x r s") 'restclient-http-send-current-stay-in-window)
;; (global-set-key (kbd "C-x r s") 'restclient-http-send-current-suppress-response-buffer)

;; change web mode 
;; (global-set-key (kbd "C-x w") 'web-mode)


;; popper 
(global-unset-key (kbd "C-x p")) 
(global-set-key (kbd "C-x p o") 'zaeny/popup-toggle) ; Toggle or open popup
(global-set-key (kbd "C-x p c") 'zaeny/popup-close)  ; Close popup


;; code folding
;; (global-set-key (kbd "C-c b m") 'hs-minor-mode)
;; (global-set-key (kbd "C-c b h") 'hs-hide-block)
;; (global-set-key (kbd "C-c b s") 'hs-show-block)

;; align
(global-set-key (kbd "C-c a c") 'zaeny/align-to-colon)
(global-set-key (kbd "C-c a e") 'zaeny/align-to-equals)
(global-set-key (kbd "C-c a a") 'aggressive-indent-indent-defun)

;; expand region
(global-set-key (kbd "C-c e r") 'er/expand-region)

;; snippets 
;;(global-set-key (kbd "C-c s r") 'zaeny/reload-snippets)
(global-set-key (kbd "C-c e s") 'zaeny/expand-abbrev-snippet)

;; region to single line to sent
(global-set-key (kbd "C-c e l") 'zaeny/region-to-single-line)

;; bookmark
;; C-x r m ;; make a bookmark
;; C-x r b ;; jump to location
;; C-x r l ;; list all bookmark
;; M-x bookmark-save
;; M-x bookamrk-delete

;; the notes
(global-set-key (kbd "C-c c n") 'zaeny/note-open-today)

;; ansi-term
;;(global-set-key (kbd "C-x p a") 'create-ansi-proc)

;; rename buffer
(global-unset-key (kbd "C-x p r")) ;; query replace
(global-set-key (kbd "C-x p b") 'rename-buffer)

;; previous bufer & nex buffer
;; <C-prior> 'previous-buffer
;; <C-next> 'next-buffer

;; the repl
;; (global-set-key (kbd "C-x p b") 'create-browser-repl)
;; (global-unset-key (kbd "C-x c"))
;; (global-set-key (kbd "C-x c 1") 'repl-set-default-proc)
;; (global-set-key (kbd "C-x c 2") 'repl-set-second-proc)
;; (global-set-key (kbd "C-x c n") 'repl-set-mark)
;; (global-set-key (kbd "C-c c k") 'repl-send-mark)
;; (global-set-key (kbd "C-c c s") 'repl-send-last-exp)
;; (global-set-key (kbd "C-c c l") 'repl-send-line)
;; (global-set-key (kbd "C-c c e") 'repl-send-paragraph)
;; (global-set-key (kbd "C-c c b") 'repl-send-buffer)
;; (global-set-key (kbd "C-c c r") 'repl-send-region)
;; (global-set-key (kbd "C-c c m") 'repl-send-markdown-block)

;; git
;; (global-unset-key (kbd "C-x p p")) ;; select project
;; (global-set-key (kbd "C-c p h") 'git-push)
;; (global-set-key (kbd "C-x p c") 'git-commit)
;; (global-set-key (kbd "C-x p g") 'create-git-proc)

;; simple-httpd
;;(global-set-key (kbd "C-c c s") 'httpd-start-with-port)

;; gptel
(global-set-key (kbd "C-x g m") 'gptel-menu)
(global-set-key (kbd "C-x g s") 'gptel-send)

;; expand yas snippets
(global-set-key (kbd "C-c ;") 'emmet-expand-yas)

```
