how we parse json 
```lisp 
(let* ((json-str "{\"type\": \"hello\", \"payload\": \"world\"}")
       (hash (json-parse-string json-str :object-type 'hash-table)))
  (message "Type: %s" (gethash "type" hash))
  (message "Payload: %s" (gethash "payload" hash)))
```

##  websocket to repl node --inspect chrome debugger 

```elisp 
(require 'websocket)
(require 'json)

(defvar zaeny/ws-connection nil
  "Holds the WebSocket connection.")

(defun zaeny/ws-connect (url)
  "Connect to a WebSocket server at URL and set up message handling."
  (setq zaeny/ws-connection
        (websocket-open url
         :on-message (lambda (_ws frame)
                       (zaeny/ws-handle-message frame))
         :on-close (lambda (_ws)
                     (message "WebSocket closed"))))
  (message "Connected to %s" url))

(defun zaeny/escape-quotes (str)
  "Escape double quotes in STR for use in JSON."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun zaeny/sent-runtime-evalaute (str)
  (format "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"%s\"}}" str))

(defun zaeny/ws-send (json-str)
  "Send a JSON string JSON-STR over the WebSocket connection."
  (if (websocket-openp zaeny/ws-connection)
      (progn
        (websocket-send-text zaeny/ws-connection json-str)
        (message "Sent: %s" json-str))
    (message "WebSocket connection is not open!")))

(defun zaeny/ws-handle-message (frame)
  "Handle incoming WebSocket FRAME by extracting and parsing the message."
  (let* ((text (websocket-frame-text frame))  ; Extract the text part of the frame
         (parsed-hash (json-parse-string text :object-type 'hash-table)))
    (message "Received: %s" text)
    (message "Parsed into hash: %s" (json-encode (gethash "result" parsed-hash)))))

(defun zaeny/ws-close ()
 "Close the WebSocket connection."
 (interactive)
 (when (websocket-openp zaeny/ws-connection)
    (websocket-close zaeny/ws-connection)
    (setq zaeny/ws-connection nil)
  (message "WebSocket connection closed.")))

(defun zaeny/ws-send-evaluate (start end)
  "Send the expression from the region or the last expression in the buffer to the WebSocket server."
  (interactive "r")
  (let* ((input (if (use-region-p) (buffer-substring-no-properties start end) ; If region is active, use it
                  (thing-at-point 'sexp t)))                   ; Otherwise, grab the last expression
         (escaped-input (zaeny/escape-quotes input))
         (json-str (zaeny/sent-runtime-evalaute escaped-input)))
    (zaeny/ws-send json-str)))

(defun zaeny/ws-send-region (start end)
  "Send the expression from the region or the last expression in the buffer to the WebSocket server."
  (interactive "r")
  (let* ((input (if (use-region-p) (buffer-substring-no-properties start end) ; If region is active, use it
                  (thing-at-point 'paragraph t)))                   ; Otherwise, grab the last expression
         (escaped-input (zaeny/escape-quotes input))
         (json-str (zaeny/sent-runtime-evalaute escaped-input)))
    (zaeny/ws-send json-str)))

(defun zaeny/ws-send-line (start end)
  "Send the expression from the region or the last expression in the buffer to the WebSocket server."
  (interactive "r")
  (let* ((input (if (use-region-p) (buffer-substring-no-properties start end) ; If region is active, use it
                  (thing-at-point 'line t)))                   ; Otherwise, grab the last expression
         (escaped-input (zaeny/escape-quotes input))
         (json-str (zaeny/sent-runtime-evalaute escaped-input)))
    (zaeny/ws-send json-str)))

(defun zaeny/ws-connect-debugger ()
  "Prompt the user for a WebSocket URL and then connect to it."
  (interactive)
  (let ((url (read-string "enter url: ")))  ; Default URL
    (zaeny/ws-connect url)))

;; (zaeny/ws-connect "ws://127.0.0.1:9229/0ae38f22-6cf3-4d51-bed4-9d9038c20a70")
;; (zaeny/ws-send "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"console.log('hallo world')\"}}")
;; (zaeny/ws-close)
;; (zaeny/ws-send (zaeny/sent-runtime-evalaute (zaeny/escape-quotes "console.log(\"hello world escaped\")")))
;; (global-set-key (kbd "C-c t r") 'zaeny/ws-send-evaluate)
;; console.log("hellowww browws")
;; (zaeny/ws-connect-debugger)
```
## tmux sent simulate 

```elisp 

(defvar zaeny/tmux-runtime nil
  "runtime target to sent")

(defun zaeny/tmux-set-runtime (target)
  "set tmux target runtime"
  (interactive "sTmux runtime : session:window.pane ")
  (setq zaeny/tmux-runtime target)
  (message "set tmux runtime to % s" zaeny/tmux-runtime))

(defun zaeny/tmux-get-runtime ()
  "get active target"
  (or zaeny/tmux-runtime
      (progn (setq zaeny/tmux-runtime (read-string "session:window.pane ")) zaeny/tmux-runtime )))

(defun zaeny/tmux-send (target str)
  "sent to tmux"
  (let* ((command (concat "tmux send-keys -t " target " \"" str "\" C-m"))) ;C-m
    (start-process-shell-command "tmux-send-keys" nil command)
    (message "Sent to tmux")))

(defun zaeny/escape-quote-str (str)
  "escape special char like $ and \" before sending"
  (let* ((step1 (replace-regexp-in-string "\"" "\\\"" str t t))
         (step2 (replace-regexp-in-string "\\$" "\\$" step1 t t))
         (step3 (replace-regexp-in-string "`" "\\`" step2 t t)))
    step3))

(defun zaeny/tmux-send-runtime (str)
  "sent to last runtime"
  (zaeny/tmux-send (zaeny/tmux-get-runtime) (zaeny/escape-quote-str str)))

(defun zaeny/tmux-send-exp (start end)
  "sent s expressions or region"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                  (thing-at-point 'sexp t))))
    (zaeny/tmux-send-runtime str)))

(defun zaeny/tmux-send-region (start end)
  "sent region or paragraph"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                  (thing-at-point 'paragraph t))))
    (zaeny/tmux-send-runtime str)))

(defun zaeny/tmux-send-line (start end)
  "sent region or line"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                  (thing-at-point 'line t))))
    (zaeny/tmux-send-runtime str)))

(defun zaeny/tmux-send-cat (start end)
  "sent wrap region into cat"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                (thing-at-point 'paragraph t)))
         (filename (read-string "Filename: ")))
    (zaeny/tmux-send-runtime (format "cat > %s <<'EOF'\n%s\nEOF" filename str))))

(defun zaeny/tmux-send-whole-buffer ()
  "Send the entire buffer to tmux."
  (interactive)
  (let ((str (buffer-string)))
    (zaeny/tmux-send-runtime str)))

(defun zaeny/tmux-send-paragraph ()
  "Send current paragraph to tmux."
  (interactive)
  (let ((str (thing-at-point 'paragraph t)))
    (zaeny/tmux-send-runtime str)))

(defun zaeny/tmux-send-python-code-block ()
  "Send Python code block to tmux."
  (interactive)
  (let ((start (save-excursion (python-nav-beginning-of-block) (point)))
        (end (save-excursion (python-nav-end-of-block) (point))))
    (let ((str (buffer-substring-no-properties start end)))
      (zaeny/tmux-send-runtime str))))

(defun zaeny/mark-python-code-block ()
  "Mark the current Python code block."
  (interactive)
  (let ((start (save-excursion (python-nav-beginning-of-block) (point)))
        (end (save-excursion (python-nav-end-of-block) (point))))
    (push-mark start t t)  ; Set mark at block start
    (goto-char end)))     ; Move point to block end

(defvar zaeny/mark-str "")

(defun zaeny/tmux-mark-point (start end)
  "mark this string"
  (interactive "r")
  (setq zaeny/mark-str (buffer-substring-no-properties start end))
  (message (format "marked %s" zaeny/mark-str)))
  

(defun zaeny/tmux-send-mark ()
  "sent the marked stirg into runtime"
  (interactive)
  (zaeny/tmux-send-runtime zaeny/mark-str))
  

;; zaeny/tmux-runtime
;; (zaeny/tmux-get-runtime)
;; (zaeny/tmux-send "runtime:2" "ls")
;; (zaeny/tmux-set-runtime "runtime:2")
;; (zaeny/tmux-send-runtime "echo \"hello\"")
;; ;; (tmux-send-runtime "C-z")
;; (global-set-key (kbd "C-c t r") 'zaen/ytmux-send-cat)

```

## add isearch current word 
```elisp 
(defun zaeny/isearch-current-word ()
  "Perform an incremental search forward for the current word under the cursor."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (progn
          (isearch-mode t)
          (isearch-yank-string word))
      (isearch-forward))))

```
## reloading markdown 
```elisp
;; todo update this markdown old
(defun zaeny/reload-markdown ()
  (interactive)
  (zaeny/load-markdown "~/.emacs.d/configuration.md")
  (zaeny/load-markdown "~/.emacs.d/extend-custom.md")
  (zaeny/load-markdown "~/.emacs.d/assign-key.md")
  (message "reloaded-markdown"))
```


test session tmux 

```sh 
#creating sessions
tmux has-session -t new-session 2>/dev/null || tmux new-session -d -s new-session 'cp foo bar'
SESSION="new-session" COMMAND="cp foo bar" tmux has-session -t "$SESSION" 2>/dev/null || tmux new-session -d -s "$SESSION" "$COMMAND"

```

## pull and sync from remote 

```elisp 

(defvar zaeny/remote-file-cache (make-hash-table :test 'equal)
  "Hash table to store mappings of local cache files to their remote locations.")

(defun zaeny/get-remote-path (input-string)
  "Extract the remote path from a string like sandbox:/home/path."
  (let* ((parts (split-string input-string ":"))
         (remote-path (cadr parts))) ;; Get the second part
    remote-path))

(defun normalize-path (path)
  "Normalize PATH by resolving symbolic links and expanding the absolute path."
  (file-truename (expand-file-name path)))

(defun zaeny/pull-file-from-remote (remote-path)
  "Pull a file from the remote server and open it in a temporary buffer.
REMOTE-PATH should be a full path like sandbox:/home/user/workspaces/file.txt."
  (interactive "sRemote path (e.g., sandbox:/home/user/workspaces/file.txt): ")
  (let* ((cache-dir (expand-file-name (format "~/.emacs.d/.cache/%s"  (zaeny/get-remote-path remote-path))))
         (local-cache-file cache-dir))
    ;; Ensure cache directory exists
    (unless (file-directory-p cache-dir)
      (make-directory (file-name-directory local-cache-file) t))
    ;; Pull the file from the remote server
    (run-async-shell-command-silent
     (format "scp %s %s" (shell-quote-argument remote-path) (shell-quote-argument local-cache-file)))
    ;; Store the mapping in the hash table
    (puthash (normalize-path local-cache-file) remote-path remote-file-cache)
    ;; Open the file in a temporary buffer
    (find-file local-cache-file)))

(defun print-hash-keys-and-values (hash-table)
  "Print all keys and values from the given HASH-TABLE."
  (maphash
   (lambda (key value)
     (message "Key: %s, Value: %s" key value))
   hash-table))

(defun zaeny/sync-file-to-remote ()
  "Sync the current buffer's file to the remote server using rsync."
  (interactive)
  (let* ((local-file (buffer-file-name))
         (remote-path (gethash local-file remote-file-cache)))
    (if remote-path
        (message "synch" remote-path local-file)
        (run-async-shell-command-silent
         (format "rsync -avz --checksum %s %s"
                 (shell-quote-argument local-file)
                 (shell-quote-argument remote-path)))
      (message "No remote path found for this file."))))

(defun zaeny/run-async-shell-command-silent (command)
  "Run a shell command asynchronously, suppressing the output buffer."
  (let ((buffer (generate-new-buffer " *hidden-async-shell*")))
    (async-shell-command command buffer)
    (set-process-sentinel
     (get-buffer-process buffer)
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (kill-buffer (process-buffer process)))))))

```
## copy clipboard and paste clipboard
copy paste problem on remote

```elisp
(when (eq system-type 'darwin) ;; Check if the system is macOS
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let* ((process-connection-type nil)
                 (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
            (process-send-string pbproxy text)
            (process-send-eof pbproxy))))) ;; Set interprogram-cut-function only for macOS

(defun zaeny/copy-kill-ring-to-remote-clipboard ()
  "Copy the Emacs kill ring content to a remote clipboard file for later retrieval."
  (interactive)
  (let ((text (current-kill 0)))
    (shell-command (concat "echo " (shell-quote-argument text) " > ~/clipboard/clipboard.txt"))))

(when (eq system-type 'gnu/linux) ;; Check if the system is Linux
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (with-temp-buffer
            (insert text)  ;; Insert the copied/cut text
            (write-file "~/clip"))))) ;; Write to the clipboard file on the server

(defun zaeny/update-clipboard-from-file ()
  "Read clipboard content from the file and update Emacs kill-ring."
  (when (file-exists-p "~/clipboard/clipboard.txt")
    (with-temp-buffer
      (insert-file-contents "~/clipboard/clipboard.txt")
      (push (buffer-string) kill-ring))))

```


## note

```elisp
(defun zaeny/note-open-today (&optional year month day)
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

(defun zaeny/note-open-today-remote (&optional year month day)
  "Open a note for a specific YEAR, MONTH, and DAY on a remote server using TRAMP with sshx.
If not provided, use today's date. If the file doesn't exist, create it."
  (interactive)
  (unless year
    (let ((current-date (decode-time (current-time))))
      (setq year (nth 5 current-date)
            month (nth 4 current-date)
            day (nth 3 current-date))))
  (let* ((remote-directory "/sshx:sandbox:~/daily-note-contents/contents/")
         (filename (format "notes-%04d%02d%02d.md" year month day))
         (fullpath (concat remote-directory filename)))
    (message "connecting to remote server...")
    (find-file fullpath)
    (when (not (file-exists-p fullpath))
      (insert (format "## Note - %04d%02d%02d\n\n" year month day))
      (save-buffer))))
```

## expand abbreviations custom snippets

```elisp
(defvar zaeny/snippets (make-hash-table :test 'equal))

(defun zaeny/load-snippets (snippets-file)
  "Load snippets from the Markdown file into the snippets hash table."
  (interactive)
  (if (file-exists-p (expand-file-name snippets-file))
      (with-temp-buffer
        (insert-file-contents (expand-file-name snippets-file))
        (goto-char (point-min))
        (while (re-search-forward md-block-header-snippet nil t)
          (let ((name (match-string 2))
                (start (progn (forward-line) (point)))
                (end (progn (re-search-forward md-block-end-snippet nil t)
                            (line-beginning-position))))
            (puthash name
                     (string-trim (buffer-substring-no-properties start end))
                     zaeny/snippets))))
    (message "Snippets file not found: %s" (expand-file-name snippets-file))))


(defun zaeny/reload-snippets ()
  (interactive)
  (zaeny/load-snippets "~/.emacs.d/docs/snippet.md")) ;; todo: change directory

(zaeny/load-snippets "~/.emacs.d/docs/snippet.md")

(defun zaeny/abbrev-at-point ()
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

(defun zaeny/expand-abbrev-snippet ()
  (interactive)
  (let* ((abbrev (zaeny/abbrev-at-point))
         (snippet-content (gethash abbrev zaeny/snippets)))
    (if snippet-content
        (insert-content-abbrev snippet-content))))

(defun zaeny/region-to-single-line ()
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

```
## ansi repl ideas 

send to repl/terminal, send into an incremental execution environment
add send javascript, send plain, send wrap, send paragrap, send region and send buffer
set wrap send plain, 
send specific command
control ansi process

```elisp

(defun zaeny/create-ansi-proc ()
  "create ansi process with name"  
  (interactive)
  (let ((name (read-string "proc name (default: ansi-term): " nil nil "ansi-term"))
        (bin (read-string "bin (default: /bin/zsh): " nil nil "/bin/zsh")))
    (if (get-buffer (format "*%s*" name))
        (get-buffer-process (format "*%s*" name))
      (get-buffer-process (ansi-term bin name)))))

```
create browser repl server

```elisp

;; todo use different approach
(defun zaeny/create-browser-repl ()
  "create node.js browser repl and send evaluate content to repl"
  (interactive)
  (let ((current-buffer (current-buffer))
        (port (read-string "port (default: 5050): " nil nil "5050"))
        (bin (read-string "bin (default: /bin/zsh): " nil nil "/bin/zsh"))    
        (repl-script (expand-file-name "~/.emacs.d/docs/browser-repl.js")))
    (if (get-buffer "*browser-repl*")
        (message "*browser-repl* already exists")
      (let (proc (get-buffer-process (ansi-term bin "browser-repl")))
        (comint-send-string proc (format "PORT=%s node %s\n" port repl-script))
        (switch-to-buffer current-buffer)
        (message "Browser repl created")))))

;; todo: create with difference port
;; (global-set-key (kbd "C-x p b") 'create-browser-repl)

```


## git helper commit ansi term

```elisp

;; todo use better approach for this

(defun zaeny/create-git-proc ()
  "create persistence process git process"
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (get-buffer "*git*")
        (get-buffer-process "*git*")
      (get-buffer-process (ansi-term "/bin/zsh" "git"))
      (switch-to-buffer current-buffer))))

(defun zaeny/git-commit ()
  "helper to quick commit C-c g m"
  (interactive)
  (save-excursion
    (let ((msg (read-string "commit messsage: "))
          (proc (zaeny/create-git-proc)))
      (comint-send-string proc "git add .\n")
      (comint-send-string proc (format "git commit -m \"%s\"\n" msg))
      (message "Git add and commit initiated."))))

(defun zaeny/git-push ()
  "helper to quick push C-c g p"
  (interactive)
  (let ((proc (zaeny/create-git-proc)))
    (comint-send-string proc "git push origin HEAD\n")
    (message "Git push initiated.")))

```

## align

```elisp

;; align
(defun zaeny/align-to-colon (begin end)
  "Align region to colon"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":"  ) 1 1 ))

(defun zaeny/align-to-comma (begin end)
  "Align region to comma signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun zaeny/align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(defun zaeny/align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

;; work with this
(defun zaeny/align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))

```

## popper like, bare hand

```elisp 

(defvar zaeny/popup-window nil
  "The current popup window, if any.")

(defun zaeny/popup-open-or-switch (buffer-name)
  "Open a temporary popup window and display the specified BUFFER-NAME.
If the popup is already open, switch to the specified buffer."
  (interactive "BBuffer name: ")
  (let* ((buffer (get-buffer-create buffer-name))
         (popup-window (or zaeny/popup-window
                           (split-window (frame-root-window)
                                         (floor (* 0.75 (window-total-height)))
                                         'below))))
    (set-window-buffer popup-window buffer)
    (setq zaeny/popup-window popup-window)
    (select-window popup-window)))

(defun zaeny/popup-close ()
  "Close the current popup window, if it exists."
  (interactive)
  (when (window-live-p zaeny/popup-window)
    (delete-window zaeny/popup-window)
    (setq zaenypopup-window nil)))

(defun zaeny/popup-toggle ()
  "Toggle the popup window.
If it exists, close it. Otherwise, prompt for a buffer to open."
  (interactive)
  (if (window-live-p zaeny/popup-window)
      (zaeny/popup-close)
    (let ((buffer-name (read-buffer "Buffer name to display: ")))
      (zaeny/popup-open-or-switch buffer-name))))

```

## delete word

```elisp
(require 'subword)

(defun zaeny/delete-block-forward ()
  "delete forward line"
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun zaeny/delete-block-backward ()
  "delete backward line"
  (interactive)
  (if (bobp)
      (message "Beginning of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-backward (string (char-syntax (char-before))))
              (point)))
           (subword-move-point
            (save-excursion
              (subword-backward)
              (point))))
      (kill-region (point) (max syntax-move-point subword-move-point)))))


(defun zaeny/kill-whole-line ()
  "Kill the entire current line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun zaeny/join-line ()
  "Join the following line to this one."
  (interactive)
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " "))

(defun zaeny/move-line-down ()
  "Move the current line down by one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun zaeny/move-line-up ()
  "Move the current line up by one."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
```


