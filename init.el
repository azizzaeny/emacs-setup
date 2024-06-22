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

;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'setup-init)

;; more require here then do inside the each files
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

(when (eq system-type 'darwin) ;; Check if the system is macOS
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let* ((process-connection-type nil)
                 (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
            (process-send-string pbproxy text)
            (process-send-eof pbproxy))))) ;; Set interprogram-cut-function only for macOS


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
    impatient-mode
    simple-httpd
    emmet-mode
    yaml-mode)
  )

;; this required but not yet
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


;; (setq load-verbose t)
;; (setq debug-on-error t)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

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

(setq window-divider-default-places 'right-only)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(load-theme 'vscode-dark-plus t)

(window-divider-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-divider ((t (:foreground "dim gray"))))
 '(window-divider-first-pixel ((t (:foreground "dim gray"))))
 '(window-divider-last-pixel ((t (:foreground "dim gray")))))

(defvar cursor-initial-color (face-attribute 'cursor :background))
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2)  ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'expand-region)


;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
;; (yas-reload-all)


;; (require 'counsel)
(require 'ivy)
(require 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
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


(setq-default indent-tabs-mode nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (number-sequence 2 200 2))
(set-default 'truncate-lines t)

;;(display-line-numbers-mode 1)
(global-display-line-numbers-mode 1)

;;(require 'lsp-mode)
;;(add-hook 'js-mode-hook #'lsp)

(require 'multiple-cursors)

(require 'simple-httpd)
(require 'impatient-mode)
(setq httpd-port 19008) ;; secret port playground


;; (setq server-use-tcp t)
;; (setq server-host "iodev")
;; (server-start)
;;(load-markdown "emacs-custom.md")

;; snippet
(defvar snippets (make-hash-table :test 'equal))
(puthash "node" "foo\nbar\nbar" snippets);
(puthash "html" "<html>\n  <head>\n    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">\n  </head>\n  <body></body>\n</html>" snippets);
(puthash "http" "var {createServer, startServer response, findFile} = require('@zaeny/http');\n\nvar index = (req, res) => findFile('./index.html')\nvar notFound = () => ({body:'', headers:{}, status: 404});\n\nvar routes = {\n  'GET /': index,\n  \"GET /_\": responseBuffer,\n  \"GET /favico.ico\": notFound,  \n}\n\nvar handler = (req, res) => {\n  let resolve = routes[`${req.method} ${req.path}`];  \n  if(resolve) return resolve(req, res);\n  return index(req, res); // no not found\n}\nvar server = server  || startServer(createServer({ port: 8081, handler: (req, res) => handler(req, res)}))\nvar reload = () => responseWith(`window.location=window.location;`);\nvar watchReload = (file, callback) => require('fs').watchFile(file, { persistent:true, interval:500 }, (prev, cur)=> callback());\nvar watcher = watcher || watchReload('./index.html', reload);\n" snippets)
(puthash "addHttp" "\nvar evaluate= (...args) => {\n  let [vm=require('vm'), ctx=global, addCtx={console, require, module}] = args;\n  return (res) => {\n    let context = vm.createContext(ctx);\n    return vm.runInContext(res, Object.assign(context, addCtx));\n  }\n}\nvar addDeps = (url, file) => fetch(url).then(res => res.text()).then(evaluate());\nvar httpUrl = \"https://raw.githubusercontent.com/azizzaeny/http/main/dist/index.js\";\naddDeps(httpUrl);" snippets)
(puthash "devjs" "\nvar dev = url => fetch(url).then(res => res.text()).then(res => (eval(res), setTimeout(()=>dev(url), 200)));\n" snippets)

(defun abbrev-at-point ()
  "Return the abbreviation at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_:|-!<>")
                  (point))))
    (buffer-substring-no-properties start end)))

(defun insert-content-abbrev (content)
  (interactive)
  (progn
    (backward-kill-word 1)
    (insert content)))

(defun expand-abbrev-snippet ()
  (interactive)
  (let* ((abbrev (abbrev-at-point))
         (snippet-content (gethash abbrev snippets)))
    (if snippet-content
        (insert-content-abbrev snippet-content))))

(defun region-to-single-line ()
  "Replace newlines in region with '\\n' and concatenate into a single line."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char start)
        (let ((region-text (buffer-substring-no-properties start end)))
          (setq region-text (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\"" "\\\\\"" region-text)))
          (kill-new region-text))))))


;; repl
(defvar repl-process "*ansi-term*") ;; main repl process window
(defvar repl-wrap  "%s")

(defface my-highlight-face
  '((t (:background "##f0f8ff"))) ; Customize background color here
  "Face for highlighting text."
  :group 'basic-faces)

(defun highlight-region (start end)
  (let ((region-highlight (make-overlay start end)))
    (overlay-put region-highlight 'face 'my-highlight-face)
    (run-at-time "0.3 sec" nil #'delete-overlay region-highlight))
  (deactivate-mark))

(defun repl-start-ansi ()
  (interactive)
  (setq cmd (read-string "Enter cmd: " "/bin/zsh"))  
  (ansi-term cmd)
  (messsage "Started %s REPL" repl-process))

(defun repl-set-process ()
  "set interactive repl process"
  (interactive)
  (setq repl-process (read-string "Enter repl process: "))) ;; change main process window

(defun repl-set-wrap ()
  "set interactive repl-wrap as client"
  (interactive)
  (setq repl-wrap (read-string "Enter wrap text: ")))

(defun repl-send-content (content)
  "simulate entering into repl-process"
  (interactive)
  (with-current-buffer repl-process
    (goto-char (point-max))
    (term-line-mode)
    (insert (format repl-wrap content))
    (term-send-input)
    (term-char-mode))
   )

(defun repl-send-buffer ()
  "send the whole buffer"
  (interactive)
  (highlight-region (point-min) (point-max))
  (message "send b")  
  (repl-send-content (buffer-substring-no-properties (point-min) (point-max))))

(defun escape-backtick (str)
  "Replace all occurrences of backtick (`) with escaped form (\\`)"
  (replace-regexp-in-string "`" "\\\\`" str))

(defun repl-send-buffer-escape ()
  "send the whole buffer escape backtick"
  (interactive)
  (highlight-region (point-min) (point-max))
  (message "send b escape")  
  (repl-send-content (escape-backtick (buffer-substring-no-properties (point-min) (point-max)))))

(defun repl-send-line ()
  "Send the current line to the REPL."
  (interactive)  
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (highlight-region start end)
      (message "send n: %s %s" start end)
      (repl-send-content (buffer-substring-no-properties start end))
      ;; (repl-send-content (thing-at-point 'line t))
      )))

(defun repl-send-paragraph ()
  "Send the current paragraph to the REPL."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (highlight-region start end)
      (message "send e: %s %s" start end)      
      (repl-send-content (buffer-substring-no-properties start end))
      ;;(repl-send-content (thing-at-point 'paragraph t))
      )))


(defun repl-send-region (start end)
  "Send the region between START and END to the REPL."
  (interactive "r")
  (highlight-region start end)
  (message "send r: %s %s" start end)  
  (repl-send-content (buffer-substring-no-properties start end)))

(defun repl-send-md-block ()
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))    
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward "```" starting-pos t) (match-string 1)) nil))
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
        (highlight-region start-content end-pos)
        (message "send m: %s %s" start-content end-pos)  
        (repl-send-content (buffer-substring-no-properties start-content end-pos))
        )
      )))

(defun repl-send-eol ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:")))
    (repl-send-content (format "cat > %s <<'EOL'" file-loc))))

(defun repl-send-eol-output-region ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:"))
        (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (repl-send-content (format "cat > %s <<'EOL'\n%s\nEOL" file-loc content))))

(defun repl-send-eol-output-buffer ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:"))
        (content (buffer-substring-no-properties (point-min) (point-max))))                 
    (repl-send-content (format "cat > %s <<'EOL'\n%sEOL" file-loc content))))

(defun repl-send-reload ()
  "send default funciton reload"
  (interactive)
  (repl-send-content "reload()"))

(defun repl-send-main ()
  "send default funciton reload"
  (interactive)
  (repl-send-content "main()"))

(defun repl-send-client-region (start end)
  "Send region pre configured string"
  (interactive "r")
  (message "send r: %s %s" start end)  
  (highlight-region start end)
  (setq repl-wrap "responseWith(`%s`)")
  (repl-send-content (buffer-substring-no-properties start end))
  (setq repl-wrap "%s"))

;; align
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


(defun note-open-today (&optional year month day)
  "Open a note for a specific YEAR, MONTH, and DAY. If not provided, use today's date. If the file doesn't exist, create it."
  (interactive)
  (unless year
    (let ((current-date (decode-time (current-time))))
      (setq year (nth 5 current-date)
            month (nth 4 current-date)
            day (nth 3 current-date))))
  (let* ((directory "~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat directory filename)))
    (find-file fullpath)
    (when (not (file-exists-p fullpath))
      (insert (format "## Note - %04d%02d%02d\n\n" year month day))
      (save-buffer))))

(defun note-open-n-day (n)
  "Open note n days before"
  (interactive "p")
  (let* ((target-date (time-subtract (current-time) (days-to-time n)))
         (year (string-to-number (format-time-string "%Y" target-date)))
         (month (string-to-number (format-time-string "%m" target-date)))
         (day (string-to-number (format-time-string "%d" target-date)))
         (directory "~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat directory filename)))    
     (if (file-exists-p fullpath)
        (find-file fullpath)
       (message "Note for date %s does not exist." (format-time-string "%Y-%m-%d" target-date)))))

(defun note-open-yesterday ()
  "open yesterday notes"
  (interactive)
  (note-open-n-day 1))

(defun note-open-n2 ()
  "open n day -2"
  (interactive)
  (note-open-n-day 2))

(defun note-open-n3 ()
  "open n day -3"
  (interactive)
  (note-open-n-day 3))

(defun note-open-n4 ()
  "open n day -4"
  (interactive)
  (note-open-n-day 4))

(defun note-open-n5 ()
  "open n day 5"
  (interactive)
  (note-open-n-day 5))

(defun note-open-n6 ()
  "open n day 6"
  (interactive)
  (note-open-n-day 6))

(defun note-open-n7 ()
  "open n day 7"
  (interactive)
  (note-open-n-day 7))

;; key binding

(global-unset-key (kbd "C-t")) ;; tranpose
(global-unset-key (kbd "C-h")) ;; help
(global-unset-key (kbd "C-x C-t")) ;;transpse line

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-s") 'swiper)

(global-unset-key (kbd "C-r"))
(global-set-key (kbd "C-r") 'replace-string)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-find-library)

;; moving
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

;; markdonw
(global-set-key (kbd "C-c f") 'polymode-next-chunk)
;;(global-set-key (kbd "C-c c f") 'polymode-next-chunk)
(global-set-key (kbd "C-c p") 'polymode-previous-chunk)
;;(global-set-key (kbd "C-c C-f") 'markdown-forward-same-level)
;;(global-set-key (kbd "C-c C-p") 'markdown-backward-same-level)
;;(global-set-key (kbd "C-c m") 'polymode-mark-or-extend-chunk)

;; push view
(global-set-key (kbd "C-x v v") 'ivy-push-view)
(global-set-key (kbd "C-x v p") 'ivy-pop-view)

;; evaling
(global-unset-key (kbd "C-x e"))
(global-set-key (kbd "C-x e") 'eval-region)

;; wrap-un-wrap text
(global-set-key (kbd "C-x w v") 'visual-line-mode)

;; line number mode
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l") 'global-display-line-numbers-mode)

;; ansi-term
(global-unset-key (kbd "C-x r"))
(global-unset-key (kbd "C-x a"))
(global-set-key (kbd "C-x a a") 'ansi-term)
(global-set-key (kbd "C-x r b") 'rename-buffer)


;; code folding
(global-set-key (kbd "C-c b m") 'hs-minor-mode)
(global-set-key (kbd "C-c b h") 'hs-hide-block)
(global-set-key (kbd "C-c b s") 'hs-show-block)

;; align
(global-set-key (kbd "C-c a c") 'align-to-colon)
(global-set-key (kbd "C-c a e") 'align-to-equals)
(global-set-key (kbd "C-c a a") 'aggressive-indent-indent-defun)

;; expand region
(global-set-key (kbd "C-c e") 'er/expand-region)


;; the snippets
;; (global-set-key (kbd "C-c s e") 'custom-snippet-expand)
;; (global-set-key (kbd "C-c s v") 'custom-snippet-eval)
;; (global-set-key (kbd "C-c s r") 'reload-snippets)
(global-set-key (kbd "C-c s e") 'expand-abbrev-snippet)
(global-set-key (kbd "C-c s l") 'region-to-single-line)
                
;; the notes
(global-set-key (kbd "C-c c n") 'note-open-today)
;; (global-set-key (kbd "C-c n 1") 'note-open-yesterday)
;; (global-set-key (kbd "C-c n 2") 'note-open-n2)
;; (global-set-key (kbd "C-c n 3") 'note-open-n3)
;; (global-set-key (kbd "C-c n 4") 'note-open-n4)
;; (global-set-key (kbd "C-c n 5") 'note-open-n5)
;; (global-set-key (kbd "C-c n 6") 'note-open-n6)
;; (global-set-key (kbd "C-c n 7") 'note-open-n7)

;; the repl
(global-set-key (kbd "C-c c p") 'repl-set-process)

;; (global-set-key (kbd "C-c c c") 'repl-connect-socket);
(global-set-key (kbd "C-c c d") 'repl-disconnect-socket);
(global-set-key (kbd "C-c c s") 'repl-start-ansi)
(global-set-key (kbd "C-c c w") 'repl-set-wrap)
(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c o") 'repl-send-eol-output-region)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c g") 'repl-send-buffer-escape)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)
(global-set-key (kbd "C-c c c") 'repl-send-client-region);
(global-set-key (kbd "C-c c k") 'repl-send-reload);
(global-set-key (kbd "C-c c j") 'repl-send-main);

;; Multi Cursrs
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (load-markdown "contents/snippet.md")
;; (load-markdown "contents/markdown.md")
;; (load-markdown "contents/repl.md")
;; (load-markdown "contents/align.md")
;; (load-markdown "contents/notes.md")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors edit-indirect yaml-mode yasnippet expand-region poly-markdown polymode aggressive-indent php-mode js2-mode python-mode auto-complete counsel clojure-mode cmake-mode)))


