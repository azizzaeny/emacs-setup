repl, sent tmux 

```elisp
(defvar tmux-last-target nil
  "Stores the last tmux target used.")

(defun tmux-set-target (target)
  "Set the tmux TARGET for subsequent commands."
  (interactive "sTmux target (e.g., session:window.pane): ")
  (setq tmux-last-target target)
  (message "Tmux target set to: %s" tmux-last-target))

(defun tmux-get-target ()
  "Get the current tmux target, or prompt if none is set."
  (or tmux-last-target
      (read-string "No target set. Specify tmux target (e.g., session:window.pane): ")))

(defun tmux-send-keys (target command)
  "Send a COMMAND string to a specific tmux TARGET (session:window.pane).
Escape special characters like $ and \" before sending."
  (let* ((step1 (replace-regexp-in-string "\"" "\\\"" command t t))
         ;; Handle dollar sign differently - just escape it without capturing
         (step2 (replace-regexp-in-string "\\$" "\\$" step1 t t))
         ;; Handle semicolon by escaping it
         (step3 (replace-regexp-in-string ";" "\\;" step2 t t))
         (formatted-command (concat "tmux send-keys -t " target " \"" step3 "\" C-m")))
    (start-process-shell-command "tmux-send-keys" nil formatted-command)
    (message "Sent to tmux: %s" command)))

;; (defun tmux-send-keys (target command)
;;   "Send a COMMAND string to a specific tmux TARGET (session:window.pane).
;; Escape special characters for shell interpretation."
;;   (let* ((step1 (replace-regexp-in-string "\"" "\\\"" command t t))
;;          (step2 (replace-regexp-in-string "\\$" "\\$" step1 t t))
;;          (step3 (replace-regexp-in-string ";" "\\;" step2 t t))
;;          ;; Additional shell special characters that might need escaping
;;          (step4 (replace-regexp-in-string "|" "\\|" step3 t t))
;;          (step5 (replace-regexp-in-string ">" "\\>" step4 t t))
;;          (step6 (replace-regexp-in-string "<" "\\<" step5 t t))
;;          (step7 (replace-regexp-in-string "&" "\\&" step6 t t))
;;          (step8 (replace-regexp-in-string "`" "\\`" step7 t t))
;;          (step9 (replace-regexp-in-string "(" "\\(" step8 t t))
;;          (step10 (replace-regexp-in-string ")" "\\)" step9 t t))
;;          (formatted-command (concat "tmux send-keys -t " target " \"" step10 "\" C-m")))
;;     (start-process-shell-command "tmux-send-keys" nil formatted-command)
;;     (message "Sent to tmux: %s" command)))

(defun tmux-send-control-key (key)
  "Send a control KEY (e.g., C-c, C-d, C-z) to the last tmux target."
  (let ((target (tmux-get-target))
        (control-key (pcase key
                       ('C-c "C-c")
                       ('C-d "C-d")
                       ('C-z "C-z")
                       (_ (error "Unsupported control key")))))
    (let ((formatted-command (format "tmux send-keys -t %s %s" target control-key)))
      (start-process-shell-command "tmux-send-keys" nil formatted-command)
      (message "Sent to tmux: %s" control-key))))

(defun tmux-send-last-command (command)
  "Send COMMAND to the last tmux target."
  (tmux-send-keys (tmux-get-target) command))

(defun tmux-send-line-to-repl ()
  "Send the current line to the tmux target."
  (interactive)
  (let ((line-text (thing-at-point 'line t)))
    (tmux-send-last-command line-text)))

(defun tmux-send-region-to-repl (start end)
  "Send the selected region (START to END) to the tmux target."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (tmux-send-last-command region-text)))

(defun tmux-send-paragraph-to-repl ()
  "Send the current paragraph to the tmux target."
  (interactive)
  (let ((paragraph-text (thing-at-point 'paragraph t)))
    (tmux-send-last-command paragraph-text)))

(defun tmux-send-control-c ()
  "Send C-c to the tmux target."
  (interactive)
  (tmux-send-control-key 'C-c))

(defun tmux-send-control-d ()
  "Send C-d to the tmux target."
  (interactive)
  (tmux-send-control-key 'C-d))

(defun tmux-send-control-z ()
  "Send C-z to the tmux target."
  (interactive)
  (tmux-send-control-key 'C-z))

(global-set-key (kbd "C-c t t") 'tmux-set-target)          ;; Set target
(global-set-key (kbd "C-c t r") 'tmux-send-region-to-repl) ;; Send region
(global-set-key (kbd "C-c t l") 'tmux-send-line-to-repl)   ;; Send line
(global-set-key (kbd "C-c t p") 'tmux-send-paragraph-to-repl) ;; Send paragraph
(global-set-key (kbd "C-c t c") 'tmux-send-control-c)      ;; Send C-c
(global-set-key (kbd "C-c t d") 'tmux-send-control-d)      ;; Send C-d
(global-set-key (kbd "C-c t z") 'tmux-send-control-z)      ;; Send C-z

```

test sending javascript 

```js 
function test(){ return 1; };
test();

// "sending multiple lines "
function test(){
  return 2;
};

test();

function writing_error(){
  asdfs
```

tmux, mosh, and workflow 
text specifications, faster  

```elisp 
(add-hook 'text-mode-hook
          (lambda ()
            (global-display-line-numbers-mode) ;; enable line 
            (setq-default paragraph-start "\f\\|[ \t]*$"
                          paragraph-separate "[ \t\f]*$") ;; hanging indent
            (setq fill-column 80)    ; Set maximum line width to 80 characters
            (auto-fill-mode 1)
            (turn-on-auto-fill))) ; Enable automatic line wrapping 

```

async saving , increase performances 
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

;; (defun async-save-via-scp ()
;;   "Save the current buffer asynchronously to the remote server using scp.
;; The file will be saved in the same directory as the local file."
;;   (interactive)
;;   (let ((buffer-file (buffer-file-name)))
;;     (if buffer-file
;;         (let* ((local-directory (file-name-directory buffer-file))   ;; Get local directory
;;                (file-name (file-name-nondirectory buffer-file))       ;; Get the file name
;;                (remote-directory "~/")                    ;; Example remote base directory
;;                (remote-destination (concat remote-directory
;;                                            (substring local-directory (length (expand-file-name "~")))  ;; Keep directory structure
;;                                            file-name)))
;;           ;; Run async shell command to upload the file with scp
;;           (async-shell-command
;;            (concat "scp " buffer-file " sandbox:" remote-destination))
;;           (message "Buffer is not visiting a file!")))))

;; (defun async-save-via-rsync-silent ()
;;   "Save the current buffer asynchronously to a remote server using rsync, suppressing output."
;;   (interactive)
;;   (let ((buffer-file (buffer-file-name)))
;;     (if buffer-file
;;         (let* ((local-directory (file-name-directory buffer-file))   ;; Get local directory
;;                (file-name (file-name-nondirectory buffer-file))       ;; Get the file name
;;                (remote-directory "~/")                    ;; Remote base directory (customize as needed)
;;                (remote-destination (concat remote-directory
;;                                            (substring local-directory (length (expand-file-name "~")))  ;; Keep directory structure
;;                                            file-name)))
;;           ;; Use rsync for faster file transfer, hide the output buffer
;;           (let ((async-process-buffer "*async-save-rsync*"))
;;             (async-shell-command
;;              (concat "rsync -avz --checksum " buffer-file " sandbox:" remote-destination)
;;              async-process-buffer)
;;             (set-process-sentinel (get-buffer-process async-process-buffer)
;;                                   (lambda (process event)
;;                                     (when (string= event "finished\n")
;;                                       (kill-buffer async-process-buffer)))))
;; 		  (message "Buffer is not visiting a file!")))))

;; (defun run-async-shell-command-silent (command)
;;   "Run a shell command asynchronously, suppressing the output buffer.
;; The process remains available in the background for inspection if needed."
;;   (let ((buffer (generate-new-buffer " *hidden-async-shell*")))
;;     (async-shell-command command buffer)
;;     (with-current-buffer buffer
;;       (delete-other-windows)) ;; Ensure buffer isn't visible
;;     buffer)) ;; Return buffer in case user wants to inspect

(defun run-async-shell-command-silent (command)
  "Run a shell command asynchronously, suppressing the output buffer and killing it after execution.
The process remains available in the background while running, and the current buffer is unaffected."
  (let ((buffer (generate-new-buffer " *hidden-async-shell*")))
    (async-shell-command command buffer)
    (with-current-buffer buffer
      ;; Ensure buffer is not displayed
      (delete-other-windows))
    ;; Set up a process sentinel to kill the buffer once the command is done
    (set-process-sentinel
     (get-buffer-process buffer)
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (kill-buffer (process-buffer process)))))
    buffer)) ;; Return buffer in case user needs to inspect it before termination

(defun pull-file-from-remote (relative-remote-file)
  "Pull a file from the remote server, ensuring directories are created locally.
RELATIVE-REMOTE-FILE is the path relative to /home/aziz/workspaces on the server."
  (interactive "sRelative remote file (e.g., mydir/dir/anotherdir/file.md): ")
  (let* ((remote-base "/home/aziz/workspaces/")
         (remote-file (concat remote-base relative-remote-file))
         (local-base "~/workspaces/")
         (local-file (expand-file-name relative-remote-file local-base))
         (local-dir (file-name-directory local-file)))
    ;; Ensure local directory exists
    (unless (file-directory-p local-dir)
      (make-directory local-dir t))
    ;; Pull file from remote
    (run-async-shell-command-silent
     (format "scp sandbox:%s %s" remote-file local-file))))

(defun save-and-sync-to-remote ()
  "Save the current file and sync it to the remote server, ensuring directories exist remotely."
  (interactive)
  (save-buffer)
  (let* ((local-base "~/workspaces/")
         (remote-base "/home/aziz/workspaces/")
         (relative-path (file-relative-name (buffer-file-name) (expand-file-name local-base)))
         (remote-file (concat remote-base relative-path))
         (remote-dir (file-name-directory remote-file)))
    ;; Create remote directory if it doesn't exist
    (run-async-shell-command-silent
     (format "ssh sandbox 'mkdir -p %s'" remote-dir))
    ;; Sync file to remote
    (run-async-shell-command-silent
     (format "rsync -avz --checksum %s sandbox:%s"
             (shell-quote-argument (buffer-file-name))
             (shell-quote-argument remote-file)))))

(global-set-key (kbd "C-x C-y") 'save-and-sync-to-remote)
(global-set-key (kbd "C-x C-u") 'pull-file-from-remote)

```

tramp setup 

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

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='~/.ssh/tramp-%%r@%%h:%%p' -o ControlPersist=yes")
(setq tramp-async-args '("--async"))

;; working with sshfs in macs is better
;; sshfs -o cache=yes,compression=no,kernel_cache,reconnect,ServerAliveInterval=15,ServerAliveCountMax=3 sandbox:/home/aziz/workspace ~/workspace
;; sudo diskutil umount ~/workspaces

```

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
  (load-markdown "~/.emacs.d/docs/set-key.md")  
  (message "reloaded-markdown"))

;; (global-unset-key (kbd "C-x r"))
;; (global-set-key (kbd "C-x r m") 'reload-markdown)
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
;;(define-key dired-mode-map (kbd "*") 'dired-create-empty-file)
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
;; (with-eval-after-load 'markdown-mode
;;   (autoload 'gfm-mode "markdown-mode"
;;     "Major mode for editing GitHub Flavored Markdown files" t)
;;   (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

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

;; (require 'company)
;; (ac-config-default)

```

configuration

business owner
