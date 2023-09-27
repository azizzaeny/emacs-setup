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

(global-set-key (kbd "C-c c g") 'generate-emacs-md)

;; (load-markdown "./readme.md")

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


;;(setq server-use-tcp t)
;;(setq server-host "iodev")
(server-start)

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
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 2)  ;; keyboard scroll one line at a time
;; (setq scroll-conservatively 10000)
;; (setq scroll-preserve-screen-position t)


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

(setq company-tooltip-align-annotations t)

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
(set-default 'truncate-lines t)

;; (setq backward-delete-char-untabify-method 'hungry)
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;(require 'lsp-mode)
;;(add-hook 'js-mode-hook #'lsp)


(global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-s") 'swiper)
 
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
(global-set-key (kbd "C-x p") 'ivy-pop-view)

(global-set-key (kbd "C-x e") 'eval-region)
(global-set-key (kbd "C-c c v") 'visual-line-mode)
(global-set-key (kbd "C-c c a") 'aggressive-indent-indent-defun)



(defvar repl-conn nil
  "Variable to hold the REPL connection.")

(defvar repl-type "node" "Type of REPL: 'node' or 'browser'.")

(defun switch-type ()
  "Switch the `repl-type` between 'node' and 'browser'."
  (interactive)
  (setq repl-type (if (equal repl-type "node") "browser" "node"))
  (message "Switched REPL type to %s" repl-type))

(defun connect-repl ()
  "Connect to the custom REPL server."
  (interactive)
  (let ((host "127.0.0.1")
        (port 1355)
        (coding-system-for-write 'utf-8)
        (coding-system-for-read 'utf-8))
    (setq repl-conn (open-network-stream "repl-conn" nil host port))
    (set-process-filter repl-conn (lambda (proc string)
                                    (message string)))
    (message "Connected to REPL server.")))

(defun send-code-message (code-str)
  "Send a given CODE-STR as a message to the connected REPL server."
  (let* ((path (or (buffer-file-name) "unknown"))
         (at (number-to-string (point)))
         (line (number-to-string (line-number-at-pos)))
         (file (or (file-name-nondirectory path) "unknown"))
         (data `(("code" . ,code-str)
                 ("path" . ,path)
                 ("type" . ,repl-type)
                 ("at" . ,at)
                 ("line" . ,line)
                 ("file" . ,file)))
         (msg (concat (json-encode data) "\n")))
    (process-send-string repl-conn msg)))

(defun disconnect-repl ()
  "Disconnect from the custom REPL server."
  (interactive)
  (when repl-conn
    (delete-process repl-conn)
    (setq repl-conn nil)
    (message "Disconnected from REPL server.")))

(defun send-line ()
  "Send the line where the cursor is positioned."
  (interactive)
  (let ((line-str (thing-at-point 'line t)))
    (send-code-message line-str)))

(defun send-paragraph ()
  "Send the paragraph where the cursor is positioned."
  (interactive)
  (let ((paragraph-str (thing-at-point 'paragraph t)))
    (send-code-message paragraph-str)))

(defun send-region (start end)
  "Send the current selected region from START to END."
  (interactive "r")
  (let ((region-str (buffer-substring-no-properties start end)))
    (send-code-message region-str)))


(global-set-key (kbd "C-c c w") 'switch-type)
(global-set-key (kbd "C-c c l") 'send-line)
(global-set-key (kbd "C-c c s") 'connect-repl)
(global-set-key (kbd "C-c c r") 'send-region)
(global-set-key (kbd "C-c c e") 'send-paragraph)


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

(global-set-key (kbd "C-c c t") 'tangle-md-block)

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

(global-set-key (kbd "C-c c c") 'align-to-colon)
(global-set-key (kbd "C-c c e") 'align-to-equals)


(defun tcpe-server-name (port)
  "format buffer tcp server"
  (format "tcpe:%d" port))

(defun tcpe-start (port)
  "starting tcp server listen at port"
  (interactive
   (list (read-number "Enter port number to listen" 9999)))
  (let* ((proc-name (tcpe-server-name port))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service port
                            :sentinel 'tcpe-sentinel
                            :filter 'tcpe-filter
                            :server 't)
      ;; (with-current-buffer buffer-name
      ;;   (funcall buffer-major-mode 'text-mode))
      )))

(defun tcpe-get-process (port)
  "get server process that listening on port"
  (get-process (tcpe-server-name port)))
 
(defun tcpe-stop (port)
  "stop emacs tcp server at port"
  (interactive
   (list (read-number "enter the port server" 9999)))
  (let ((server-proc (tcpe-get-process port)))
    (delete-process server-proc)))

(defun tcpe-filter (proc string)
  "evalualte it on top of wrapper"
  (eval (car (read-from-string (format "(progn %s)" string )))))

(defun tcpe-log (proc string)
  (let ((buffer (process-contact proc :buffer))
        (inhibit-read-only t))
    (and buffer (get-buffer buffer)
         (with-current-buffer buffer
           ;; (display-buffer buffer)
           (let ((moving (= (point) (point-max))))
             (save-excursion
               (goto-char (point-max))
               (insert string))
             (if moving (goto-char (point-max))))))))

(defun tcpe-sentinel (proc msg)
  (cond
   ((string-match "open from .*\n" msg)
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client connected added to client list \n")))    
   ((string= msg "connection broken by remote peer\n")
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client quit \n")))
   ((eq (process-status proc) 'closed)
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "delete clients")))))

;; (let ((proc (make-network-process :name "my sock"
;;                                   :host 'local    ;; or hostname string
;;                                   :service 9999)))
;;   ;;(process-send-string proc "(message \"hello socket world\")")
;;   (process-send-string proc "(find)")
;;   (sleep for 3)
;;   (delete-process proc))

;;(find-file "06-align-to-sign.md")



;; (defun my-open-file-in-frame (file-path frame-name)
;;   "Open the file at FILE-PATH in the frame named FRAME-NAME."
;;   (interactive "fOpen file: \nsIn frame: ")
;;   (let ((frame (get-frame-by-name frame-name)))
;;     (when frame
;;       (select-frame-set-input-focus frame)
;;       (find-file file-path))))

;; (defun my-list-frames ()
;;   "List all frames by name."
;;   (interactive)
;;   (let ((frame-names (mapcar 'frame-parameter (frame-list) 'name)))
;;     (message "Frames: %s" (mapconcat 'identity frame-names ", "))))

;; (defun select-frame-by-name (name)
;;   "Select the frame with the given NAME."
;;   (interactive "sFrame name: ")
;;   (let* ((frames (frame-list))
;;          (frame (seq-find (lambda (f) (string-equal (frame-parameter f 'name) name)) frames)))
;;     (if frame
;;         (select-frame-set-input-focus frame)
;;       (error "No frame with name %s" name))))

(defun find-file-in-frame (file frame-name)
  "Find FILE in the frame named FRAME-NAME."
  (interactive "fFile to open: \nsFrame name: ")
  (let ((frame (seq-find (lambda (f) (string-equal (frame-parameter f 'name) frame-name)) (frame-list))))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (find-file file))
      (error "No frame with name %s" frame-name))))

