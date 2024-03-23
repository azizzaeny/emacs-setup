### Snippet System
**snippet**

```elisp

;; simple versions
(defvar custom-snippets-file-path (expand-file-name "snippets.json" user-emacs-directory)
  "Path to the custom snippets JSON file.")

(defun load-custom-snippets ()
  "Load custom snippets from JSON file."
  (when (file-exists-p custom-snippets-file-path)
    (let ((json-object-type 'hash-table))
      (json-read-file custom-snippets-file-path))))

(defvar custom-snippets-hash (load-custom-snippets)
  "Hash table of custom snippets.")

(defun reload-snippets ()
  (interactive)
  (setq custom-snippets-hash (load-custom-snippets))
  (message "snippet reloaded"))

;; customize things at point word

(defun abbreviation-at-point ()
  "Return the abbreviation at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_:|-!<>")
                  (point))))
    (buffer-substring-no-properties start end)))

(defun custom-snippet-expand ()
  "Expand custom snippet."
  (interactive)
  (let* ((abbrev (abbreviation-at-point))
         ;;(abbrev (thing-at-point 'word))
         ;;(expansion (gethash abbrev custom-snippets-hash)))
         (relative-snippet-path (gethash abbrev custom-snippets-hash))
         (snippet-path (when relative-snippet-path
                         (expand-file-name relative-snippet-path user-emacs-directory)))
         (expansion (when snippet-path
                      (with-temp-buffer
                        (insert-file-contents snippet-path)
                        (buffer-string)))))
    (message snippet-path)
    (if expansion
        (progn
          (backward-kill-word 1)
          (insert expansion))
      (message "No snippet found for abbreviation: %s" abbrev))))

(defun custom-snippet-eval ()
  "Evaluate the content of the snippet as Emacs Lisp code."
  (interactive)
  (let* ((abbrev (thing-at-point 'word))
         (relative-snippet-path (gethash abbrev custom-snippets-hash))
         (snippet-path (when relative-snippet-path
                         (expand-file-name relative-snippet-path user-emacs-directory)))
         (code (when snippet-path
                 (with-temp-buffer
                   (insert-file-contents snippet-path)
                   (buffer-string)))))
    (if code
        (eval (read code))
      (message "No code found for abbreviation: %s" abbrev))))
```

### Evaluation and repl
**repl** and **evaluation**

```elisp
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
  (setq repl-wrap (read-string "Enter wrap wrap: ")))

(defun repl-send-content (content)
  "simulate entering into repl-process"
  (interactive)
  (with-current-buffer repl-process
    (term-line-mode)
    (insert (format repl-wrap content))
    (term-send-input)
    (term-char-mode)))

(defun repl-send-buffer ()
  "send the whole buffer"
  (interactive)
  (highlight-region (point-min) (point-max))
  (message "send b")  
  (repl-send-content (buffer-substring-no-properties (point-min) (point-max))))

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

(defun repl-send-eol-output-buffer ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:"))
        (content (buffer-substring-no-properties (point-min) (point-max))))                 
    (repl-send-content (format "cat > %s <<'EOL'\n%sEOL" file-loc content))))

(message "repl loaded")
```

### Markdown 
**markdown**

```elisp
(message "markdown loaded")

(defvar md-block-header "^```elisp")
(defvar md-block-end "^```$")

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

### Align
**align**

```elisp
(message "align loaded")

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

```

### Notes
**note**

```elisp

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

```
### TCP Client
**tcp-client**

```emacs-lisp

(defun my-tcp-client (host port)
  "Connect to HOST on PORT via TCP."
  (let ((buffer (generate-new-buffer "*TCP Client*")))
    (set-process-filter
     (open-network-stream "tcp-client" buffer host port)
     'my-tcp-client-filter)
    (display-buffer buffer)))

(defun strip-ansi-escape-sequences (string)
  "Strip ANSI escape sequences from STRING."
  (replace-regexp-in-string "\\(\\x1b\\[[0-9;]+[a-zA-Z]\\)" "" string))

(defun my-tcp-client-filter (process string)
  "Filter function to handle received data."
  (with-current-buffer (process-buffer process)
    (insert string)
    (goto-char (point-max))
    (while (re-search-backward "\n" (line-beginning-position) t)
      (let ((line (buffer-substring (point) (line-end-position))))
        (message (strip-ansi-escape-sequences line)))
      (delete-region (point-min) (1+ (line-end-position))))))

;; Example usage
(my-tcp-client "51.255.87.159" 30300)

```
another try
```emacs-lisp

(require 'json)
(require 'subr-x)
(require 'ansi-color)

(defvar my-tcp-client-buffer "*TCP Client*")

(defun my-tcp-client-connect (host port)
  "Connect to HOST on PORT."
  (let ((process (open-network-stream "my-tcp-client" my-tcp-client-buffer host port)))
    (set-process-filter process 'my-tcp-client-receive)))

;; (defun my-tcp-client-receive (process message)
;;   "Process received MESSAGE from PROCESS."
;;   (with-current-buffer (get-buffer-create my-tcp-client-buffer)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (insert (concat (substring-no-properties message) "\n")))
;;     (message "%s" (substring-no-properties message))))

(defun my-tcp-client-receive (process message)
  "Process received MESSAGE from PROCESS."
  (with-current-buffer (get-buffer-create my-tcp-client-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert (ansi-color-apply message) "\n"))
    (message "%s" (ansi-color-apply message))))

;; (defun my-tcp-client-receive (process message)
;;   "Process received MESSAGE from PROCESS."
;;   (let ((clean-message (replace-regexp-in-string "\r" "" (ansi-color-apply message))))
;;     (message "%s" clean-message)))

;; (defun my-tcp-client-receive (process message)
;;   "Process received MESSAGE from PROCESS."
;;   (with-current-buffer (get-buffer-create my-tcp-client-buffer)
;;     (save-excursion
;;       (goto-char (point-max))
;;       (insert (ansi-color-apply (replace-regexp-in-string "\r" "" message)) "\n"))
;;     (message "%s" (ansi-color-apply (replace-regexp-in-string "\r" "" message)))))


(defun my-tcp-client-hide-buffer ()
  "Hide the TCP client buffer, showing only new messages."
  (interactive)
  (let ((window (get-buffer-window my-tcp-client-buffer)))
    (when window
      (delete-window window))))

(defun my-tcp-client-disconnect ()
  "Disconnect from the TCP server."
  (interactive)
  (let ((process (get-buffer-process my-tcp-client-buffer)))
    (when process
      (delete-process process)
      (kill-buffer my-tcp-client-buffer))))

(defun my-tcp-client (host port)
  "Connect to HOST on PORT as a TCP client."
  (interactive "sHost: \nnPort: ")
  (my-tcp-client-connect host port))
```

```emacs-lisp

(defvar my-tcp-client-process nil
  "Variable to store the TCP client process.")

(defun my-tcp-client (host port)
  "Connect to HOST on PORT via TCP."
  (interactive "sEnter host: \nnEnter port: ")
  (setq my-tcp-client-process (open-network-stream "my-tcp-client" 
                                                    "*TCP Client*" 
                                                    host port))
  (set-process-filter my-tcp-client-process 'my-tcp-client-filter))

(defun my-tcp-client-filter (proc message)
  "Filter function to handle incoming messages from the TCP server."
  (with-current-buffer "*TCP Client*"
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert message)
      (insert "\n")
      (delete-duplicate-lines (point-min) (point-max))
      (if (> (count-lines (point-min) (point-max)) 10)
          (progn
            (goto-char (point-min))
            (delete-region (line-beginning-position) (line-end-position))
            (insert "...\n")))
      (when (called-interactively-p 'any)
        (pop-to-buffer (current-buffer))
        (message "%s" (substring-no-properties message))
        ))))

(defun my-tcp-client-send (message)
  "Send MESSAGE over the TCP connection."
  (interactive "sEnter message: ")
  (if (process-live-p my-tcp-client-process)
      (progn
        (process-send-string my-tcp-client-process message)
        (process-send-string my-tcp-client-process "\n"))
    (message "TCP connection is not established.")))

```

### Key binding
**binding keys settings**
rules for windows we use `C-x`

```elisp

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
;; C-c C-n or C-c C-p: Move to the next or previous heading (alternative).
;; C-M-a (markdown-beginning-of-code-block): Move to the beginning of the current code block.
;; C-M-e (markdown-end-of-code-block): Move to the end of the current code block
(global-set-key (kbd "C-c n") 'polymode-next-chunk)
(global-set-key (kbd "C-c p") 'polymode-previous-chunk)
(global-set-key (kbd "C-c m") 'polymode-mark-or-extend-chunk)

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
(global-set-key (kbd "C-c s e") 'custom-snippet-expand)
(global-set-key (kbd "C-c s v") 'custom-snippet-eval)
(global-set-key (kbd "C-c s r") 'reload-snippets)


;; the notes
;; (global-set-key (kbd "C-c n o") 'note-open-today)
;; (global-set-key (kbd "C-c n 1") 'note-open-yesterday)
;; (global-set-key (kbd "C-c n 2") 'note-open-n2)
;; (global-set-key (kbd "C-c n 3") 'note-open-n3)
;; (global-set-key (kbd "C-c n 4") 'note-open-n4)
;; (global-set-key (kbd "C-c n 5") 'note-open-n5)
;; (global-set-key (kbd "C-c n 6") 'note-open-n6)
;; (global-set-key (kbd "C-c n 7") 'note-open-n7)

;; the repl
(global-set-key (kbd "C-c c p") 'repl-set-process)
(global-set-key (kbd "C-c c c") 'repl-connect-socket);
(global-set-key (kbd "C-c c d") 'repl-disconnect-socket);
(global-set-key (kbd "C-c c s") 'repl-start-ansi)
(global-set-key (kbd "C-c c w") 'repl-switch-type)
(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c o") 'repl-send-eol-output-buffer)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)

;; Define a prefix keymap
;; (define-prefix-command 'my-custom-prefix-map)
;; (global-set-key (kbd "C-c c") 'my-custom-prefix-map)

;; ;; Define a keymap for the sequence "b u" under the prefix
;; (define-key my-custom-prefix-map (kbd "b u") 'my-custom-command)

;; ;; Define the custom command bound to "b u"
;; (defun my-custom-command ()
;;   (interactive)
;;   (message "You pressed C-c c b u! This is a custom command."))
```
