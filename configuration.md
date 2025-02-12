## refresh package contents

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
    websocket
    gptel
    )
  )
;; todo use gptel package to interact with llm
```

## use this package

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

## install the package 
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

## adding the list manual package

```elisp
(add-to-list 'load-path "~/.emacs.d/manual-packages/")
```

## text mode configuration 

```elisp 
(add-hook 'text-mode-hook
          (lambda ()
            ;;(global-display-line-numbers-mode) ;; enable line 
            ;; (setq-default paragraph-start "\f\\|[ \t]*$"
            ;;               paragraph-separate "[ \t\f]*$") ;; hanging indent
            (setq fill-column 80)    ; Set maximum line width to 80 characters
            (auto-fill-mode 1)
            (turn-on-auto-fill))) ; Enable automatic line wrapping 

;; Save & restore sessions
;; (desktop-save-mode 1)
```

## increase performances, async saving 

```elisp 
(setq gc-cons-threshold (* 50 1000 1000)) ; Increase threshold to 50MB
(setq gc-cons-percentage 0.1) ; Adjust GC frequency
(setq create-lockfiles nil)
(setq make-backup-files nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)))) ; Lower after startup

(setq comp-deferred-compilation t) ; Enable native compilation


(require 'async)
(async-bytecomp-package-mode 1); Edit large files asynchronously using packages like async to avoid blocking

(setq create-lockfiles nil); avoid lockfiles when using tramp
(setq vc-handled-backends nil) ; Disable version control for better performance

;;(auto-save-visited-mode 1)

(so-long-enable); for better handling of large or minified files:

(global-auto-revert-mode 1)
(setq auto-revert-interval 1) ; Update every 1 second
```

### setup tramp 

```elisp 
(setq tramp-default-method "sshx")
(setq tramp-verbose 1) ; Debugging level (optional)
;;/sshx:user@remote-host:/path/to/file

(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(setq tramp-persistency-file-name "~/.emacs.d/tramp-persistency")

(setq tramp-connection-timeout 10) ;increase timeout
(setq tramp-cache-enabled t)
(setq tramp-use-ssh-controlmaster-options nil) ; Prevent blocking on ControlMaster

(setq remote-file-name-inhibit-cache nil) ; cache remote files 
(setq tramp-completion-reread-directory-timeout nil) ; Reduce rereads

;;(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='~/.ssh/tramp-%%r@%%h:%%p' -o ControlPersist=yes")
(setq tramp-async-args '("--async"))
;; working with sshfs in macs is better
;; sshfs -o cache=yes,compression=no,kernel_cache,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3 sandbox:/home/aziz/workspace ~/workspace
;; sudo diskutil umount ~/workspaces
```

## make company quicker 

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

## configure the isearch 

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

```

## configure dired 

remove those details 
```elisp
;;(require 'dired-details)
;; (setq-default dired-details-hidden-string "---")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
;;(define-key dired-mode-map (kbd "*") 'dired-create-empty-file)
;;(setq dired-dwim-target t)
;;(dired-details-install)
```

## buffer make them uniques 

```elisp
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

```

## configure ivy, swiper, ido

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
(require 'iy-go-to-char)
(setq ffip-prefer-ido-mode t)
```

## expand region

```elisp
(require 'expand-region)
```

## clojure paren 

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


## javascript mode 

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

(setq comment-style 'aligned)

```

### web mode 

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

```


## themes set, divider, bottom border

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

## visual apperaence
 
```elisp
(menu-bar-mode -1)

(set-face-foreground 'vertical-border
                     (face-background 'vertical-border nil t))

;;(display-line-numbers-mode 1)
;;(global-display-line-numbers-mode 1)

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

## configure the behaviour

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

## backup behaviour

```elisp

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)

```


## scrolling

```elisp

(defvar cursor-initial-color (face-attribute 'cursor :background))
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2)  ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

```

## gptel 

```elisp
(require 'gptel)
(setq gptel-api-key (getenv "CHATGPT_API_KEY"))
;; (message gptel-api-key)

;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
;;   :host "localhost:11434"               ;Where it's running
;;   :stream t                             ;Stream responses
;;   :models '(mistral:latest))          ;List of models

;;(gptel-make-gemini "Gemini" :key "YOUR_GEMINI_API_KEY" :stream t)

;; (gptel-make-anthropic "Claude"          ;Any name you want
;;   :stream t                             ;Streaming responses
;;   :key "your-api-key")

;; DeepSeek offers an OpenAI compatible API
;; (gptel-make-openai "DeepSeek"       ;Any name you want
;;   :host "api.deepseek.com"
;;   :endpoint "/chat/completions"
;;   :stream t
;;   :key "your-api-key"               ;can be a function that returns the key
;;   :models '(deepseek-chat deepseek-coder))

;; OpenRouter offers an OpenAI compatible API
;; (gptel-make-openai "OpenRouter"               ;Any name you want
;;   :host "openrouter.ai"
;;   :endpoint "/api/v1/chat/completions"
;;   :stream t
;;   :key "your-api-key"                   ;can be a function that returns the key
;;   :models '(openai/gpt-3.5-turbo
;;             mistralai/mixtral-8x7b-instruct
;;             meta-llama/codellama-34b-instruct
;;             codellama/codellama-70b-instruct
;;             google/palm-2-codechat-bison-32k
;;             google/gemini-pro))
```

## polymarkdown

```elisp 
;; markdown-mode
(require 'markdown-mode)
;; (with-eval-after-load 'markdown-mode
;;   (autoload 'gfm-mode "markdown-mode"
;;     "Major mode for editing GitHub Flavored Markdown files" t)
;;   (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; polymode poly-markdown
(require 'polymode)
(require 'poly-markdown)

(with-eval-after-load 'poly-markdown
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

```
