how we parse json 
```lisp 
(let* ((json-str "{\"type\": \"hello\", \"payload\": \"world\"}")
       (hash (json-parse-string json-str :object-type 'hash-table)))
  (message "Type: %s" (gethash "type" hash))
  (message "Payload: %s" (gethash "payload" hash)))
```

##  websocket to repl node --inspect chrome debugger 

```lisp 
(require 'websocket)
(require 'json)

(defvar ws-connection nil
  "Holds the WebSocket connection.")

(defun ws-connect (url)
  "Connect to a WebSocket server at URL and set up message handling."
  (setq ws-connection
        (websocket-open url
         :on-message (lambda (_ws frame)
                       (ws-handle-message frame))
         :on-close (lambda (_ws)
                     (message "WebSocket closed"))))
  (message "Connected to %s" url))

(defun escape-quotes (str)
  "Escape double quotes in STR for use in JSON."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun sent-runtime-evalaute (str)
  (format "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"%s\"}}" str))

(defun ws-send (json-str)
  "Send a JSON string JSON-STR over the WebSocket connection."
  (if (websocket-openp ws-connection)
      (progn
        (websocket-send-text ws-connection json-str)
        (message "Sent: %s" json-str))
    (message "WebSocket connection is not open!")))

(defun ws-handle-message (frame)
  "Handle incoming WebSocket FRAME by extracting and parsing the message."
  (let* ((text (websocket-frame-text frame))  ; Extract the text part of the frame
         (parsed-hash (json-parse-string text :object-type 'hash-table)))
    (message "Received: %s" text)
    (message "Parsed into hash: %s" (json-encode (gethash "result" parsed-hash)))))

(defun ws-close ()
  "Close the WebSocket connection."
  (when (websocket-openp ws-connection)
    (websocket-close ws-connection)
    (setq ws-connection nil)
    (message "WebSocket connection closed.")))

(defun ws-send-evaluate (start end)
  "Send the expression from the region or the last expression in the buffer to the WebSocket server."
  (interactive "r")
  (let* ((input (if (use-region-p) (buffer-substring-no-properties start end) ; If region is active, use it
                  (thing-at-point 'sexp t)))                   ; Otherwise, grab the last expression
         (escaped-input (escape-quotes input))
         (json-str (sent-runtime-evalaute escaped-input)))
    (ws-send json-str)))

(defun ws-connect-debugger ()
  "Prompt the user for a WebSocket URL and then connect to it."
  (interactive)
  (let ((url (read-string "Enter WebSocket URL: " "ws://127.0.0.1:9229/e74e75ca-a7a8-4a28-ab4b-2e9743b3e827")))  ; Default URL
    (ws-connect url)))

;; Assign the function to a keybinding
;; (global-set-key (kbd "C-c t r") 'ws-send-evaluate)

;; (ws-connect "ws://127.0.0.1:9229/e74e75ca-a7a8-4a28-ab4b-2e9743b3e827")
;; (ws-send "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"console.log('hallo world')\"}}")
;; (ws-send "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"console.log('hallo world')\"}}")
;; (ws-close)
;; (ws-send (sent-runtime-evalaute (escape-quotes "console.log(\"hello world\")")))
;; console.log("hellowww browws")

```
## tmux sent simulate 

```lisp 

(defvar tmux-runtime nil
  "runtime target to sent")

(defun tmux-set-runtime (target)
  "set tmux target runtime"
  (interactive "sTmux runtime : session:window.pane")
  (setq tmux-runtime target)
  (message "set tmux runtime to % s" tmux-runtime))

(defun tmux-get-runtime ()
  "get active target"
  (or tmux-runtime
      (progn (setq tmux-runtime (read-string "session:window.pane ")) tmux-runtime )))

(defun tmux-send (target str)
  "sent to tmux"
  (let* ((command (concat "tmux send-keys -t " target " \"" str "\" C-m")))
    (start-process-shell-command "tmux-send-keys" nil command)
    (message "Sent to tmux :%s" command)))

(defun escape-quote-str (str)
  "escape special char like $ and \" before sending"
  (let* ((step1 (replace-regexp-in-string "\"" "\\\"" str t t))
         (step2 (replace-regexp-in-string "\\$" "\\$" step1 t t))
         (step3 (replace-regexp-in-string "`" "\\`" step2 t t)))
    step3))

(defun tmux-send-runtime (str)
  "sent to last runtime"
  (tmux-send (tmux-get-runtime) (escape-quote-str str)))

(defun tmux-send-exp (start end)
  "sent s expressions or region"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                  (thing-at-point 'sexp t))))
    (tmux-send-runtime str)))

(defun tmux-send-region (start end)
  "sent region or paragraph"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                  (thing-at-point 'paragraph t))))
    (tmux-send-runtime str)))

(defun tmux-send-cat (start end)
  "sent wrap region into cat"
  (interactive "r")
  (let* ((str (if (use-region-p) (buffer-substring-no-properties start end)
                (thing-at-point 'paragraph t)))
         (filename (read-string "Filename: ")))
    (tmux-send-runtime (format "cat > %s <<'EOF'\n%s\nEOF" filename str))))

(global-set-key (kbd "C-c t r") 'tmux-send-cat)

;; tmux-runtime
;; (tmux-get-runtime)
;; (tmux-send "runtime:2" "ls")
;; (tmux-set-runtime "runtime:2")
;; (tmux-send-runtime "echo \"hello\"")

;; (tmux-send-runtime "C-z")

```


## add isearch current word 
```elisp 

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
## reloading markdown 
```elisp
;; todo update this markdown old
(defun reload-markdown ()
  (interactive)
  (load-markdown "~/.emacs.d/configuration.md")
  (load-markdown "~/.emacs.d/extend-custom.md")
  (load-markdown "~/.emacs.d/assign-key.md")
  (message "reloaded-markdown"))
```

## repl, sent to tmux  session

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
         (step2 (replace-regexp-in-string "\\$" "\\$" step1 t t))
         (step3 (replace-regexp-in-string "`" "\\`"  step2 t t))
         (formatted-command (concat "tmux send-keys -t " target " \"" step3 "\" C-m")))
    (start-process-shell-command "tmux-send-keys" nil formatted-command)
    (message "Sent to tmux: %s" command)))

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

(defun tmux-send-region-or-paragraph-with-cat (start end)
  "Send the selected region or the current paragraph to the tmux target,
wrapped in a 'cat > FILENAME <<'EOF' ... EOF' block."
  (interactive "r")
  (let* ((filename (read-string "Enter filename: "))
         (content (if (use-region-p)
                      (buffer-substring-no-properties start end)
                    (thing-at-point 'paragraph t)))
         (wrapped-content (format "cat > %s <<'EOF'\n%s\nEOF" filename content)))
    (tmux-send-last-command wrapped-content)))

(defun tmux-send-region-evaluate (start end)
  "send region to last command"
  (interactive "r")
  (let* ((expression (if (use-region-p)
                     (buffer-substring-no-properties start end)
                     (thing-at-point 'paragraph t)))
         (escape-expr (replace-regexp-in-string "\"" "\\\"" expression t t))
        (json-command (format "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"%s\"}}" escape-expr)))
    (tmux-send-last-command json-command)))

(global-set-key (kbd "C-c t b") 'tmux-send-region-evaluate)

```

we just neeed to get hash with 
(gethash $hash key)
(puthash key val)
(make-hash-table :test 'equal)
console.log("hello work")

test it 

```lisp
(tmux-send-last-command "node")
(tmux-send-keys "work:0" ".exit")
(tmux-set-target "runtime:2")
(tmux-send-last-command "curl -s http://localhost:9222/json | jq  '.[0] | .id, .webSocketDebuggerUrl'")
(tmux-send-last-command "websocat ws://localhost:9222/devtools/page/209FA35662274B4BB840F70BB75EF2E5")
(tmux-send-last-command "{\"id\": 1, \"method\":\"Target.activateTarget\", \"params\":{ \"targetId\":\"209FA35662274B4BB840F70BB75EF2E5\"}}")
(tmux-send-last-command "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"console.log('hallo world')\"}}")
(tmux-send-last-command "{\"id\": 1, \"method\":\"Runtime.evaluate\", \"params\":{ \"expression\":\"console.log(\\\\\"hallo double quote world\\\\\")\"}}")
;; to coubler some we need functions on top of it
(replace-regexp-in-string "\"" "\\\"" "console.log(\"hello world\")" t t)


```

bare bones 

```sh 
creating sessions
tmux has-session -t new-session 2>/dev/null || tmux new-session -d -s new-session 'cp foo bar'
SESSION="new-session" COMMAND="cp foo bar" tmux has-session -t "$SESSION" 2>/dev/null || tmux new-session -d -s "$SESSION" "$COMMAND"

```

## pull and sync from remote 

```elisp 

(defvar remote-file-cache (make-hash-table :test 'equal)
  "Hash table to store mappings of local cache files to their remote locations.")

(defun get-remote-path (input-string)
  "Extract the remote path from a string like sandbox:/home/path."
  (let* ((parts (split-string input-string ":"))
         (remote-path (cadr parts))) ;; Get the second part
    remote-path))

(defun normalize-path (path)
  "Normalize PATH by resolving symbolic links and expanding the absolute path."
  (file-truename (expand-file-name path)))

(defun pull-file-from-remote (remote-path)
  "Pull a file from the remote server and open it in a temporary buffer.
REMOTE-PATH should be a full path like sandbox:/home/user/workspaces/file.txt."
  (interactive "sRemote path (e.g., sandbox:/home/user/workspaces/file.txt): ")
  (let* ((cache-dir (expand-file-name (format "~/.emacs.d/.cache/%s"  (get-remote-path remote-path))))
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
    (find-file local-cache-file)
    )
  )
(defun print-hash-keys-and-values (hash-table)
  "Print all keys and values from the given HASH-TABLE."
  (maphash
   (lambda (key value)
     (message "Key: %s, Value: %s" key value))
   hash-table))

(defun sync-file-to-remote ()
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

(defun run-async-shell-command-silent (command)
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

(defun copy-kill-ring-to-remote-clipboard ()
  "Copy the Emacs kill ring content to a remote clipboard file for later retrieval."
  (interactive)
  (let ((text (current-kill 0)))
    (shell-command (concat "echo " (shell-quote-argument text) " > ~/clipboard/clipboard.txt"))))

(when (eq system-type 'gnu/linux) ;; Check if the system is Linux
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (with-temp-buffer
            (insert text)  ;; Insert the copied/cut text
            (write-file "~/clipboard/clipboard.txt"))))) ;; Write to the clipboard file on the server

(defun update-clipboard-from-file ()
  "Read clipboard content from the file and update Emacs kill-ring."
  (when (file-exists-p "~/clipboard/clipboard.txt")
    (with-temp-buffer
      (insert-file-contents "~/clipboard/clipboard.txt")
      (push (buffer-string) kill-ring))))

```


## note

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

;; (defun note-open-n-day-local (n)
;;   "Open note n days before"
;;   (interactive "p")
;;   (let* ((target-date (time-subtract (current-time) (days-to-time n)))
;;          (year (string-to-number (format-time-string "%Y" target-date)))
;;          (month (string-to-number (format-time-string "%m" target-date)))
;;          (day (string-to-number (format-time-string "%d" target-date)))
;;          (directory "~/daily-note-contents/contents/")
;;          (filename (format "notes-%04d%02d%02d.md" year month day))
;;          (fullpath (concat directory filename)))    
;;      (if (file-exists-p fullpath)
;;         (find-file fullpath)
;;        (message "Note for date %s does not exist." (format-time-string "%Y-%m-%d" target-date)))))

(defun note-open-today-remote (&optional year month day)
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


;; (defun note-open-yesterday ()
;;   "open yesterday notes"
;;   (interactive)
;;   (note-open-n-day 1))

;; (defun note-open-n2 ()
;;   "open n day -2"
;;   (interactive)
;;   (note-open-n-day 2))

;; (defun note-open-n3 ()
;;   "open n day -3"
;;   (interactive)
;;   (note-open-n-day 3))

;; (defun note-open-n4 ()
;;   "open n day -4"
;;   (interactive)
;;   (note-open-n-day 4))

;; (defun note-open-n5 ()
;;   "open n day 5"
;;   (interactive)
;;   (note-open-n-day 5))

;; (defun note-open-n6 ()
;;   "open n day 6"
;;   (interactive)
;;   (note-open-n-day 6))

;; (defun note-open-n7 ()
;;   "open n day 7"
;;   (interactive)
;;   (note-open-n-day 7))

;; prototype mode

```

## expand abbreviations custom snippets

```elisp
(defvar snippets (make-hash-table :test 'equal))

(defun load-snippets (snippets-file)
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
                     snippets))))
    (message "Snippets file not found: %s" (expand-file-name snippets-file))))


(defun reload-snippets ()
  (interactive)
  (load-snippets "~/.emacs.d/docs/snippet.md"))

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

```
## ansi repl ideas 

send to repl/terminal, send into an incremental execution environment
add send javascript, send plain, send wrap, send paragrap, send region and send buffer
set wrap send plain, 
send specific command

control ansi process

```elisp

(defun create-ansi-proc ()
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

(defun create-browser-repl ()
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

## repl 

```elisp
;; prefix

(setq repl-default-proc "ansi-term")
(setq repl-second-proc "browser-repl") ;; we can sent to secondary using b, c is primary

(defun repl-set-default-proc ()
  "set default process for evaluation"
  (interactive)
  (setq repl-default-roc (read-string "set default proc (default: ansi-term): " nil nil "ansi-term")))

(defun repl-set-second-proc ()
  "set second process for evaluation"
  (interactive)
  (setq repl-second-roc (read-string "set second proc (default: browser-repl): " nil nil "browser-repl")))
;; todo: assign key

(defface my-highlight-face
  '((t (:background "##f0f8ff"))) ; Customize background color here
  "Face for highlighting text."
  :group 'basic-faces)

(defun repl-create-proc (name)
  "create persistence repl process"
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (get-buffer (format "*%s*" name))
        (get-buffer-process (format "*%s*" name))
      (get-buffer-process (ansi-term "/bin/zsh" name))
      (switch-to-buffer current-buffer) 
      (message "repl-process created"))));; TODO: fix need to sent twice when starting

(defun repl-send-to (proc str)
  "repl send message to proces"
  (interactive)
  (let ((proc (repl-create-proc proc)))
    (comint-send-string proc str)
    (comint-send-string proc "\n")  
    (message "repl sent .")))

;; main functions
(defun repl-send-last-exp (&optional proc)
  "send last expression"
  (interactive)
  (let* ((begin (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (end (point))
         (str (buffer-substring-no-properties begin end)))
    ;;(highlight-region begin end)
    (repl-send-to (or proc repl-default-proc) str)))

(defun repl-send-line (&optional proc)
  "send line"
  (interactive)
  (let* ((begin (save-excursion (beginning-of-line) (point)))
         (end (save-excursion (end-of-line) (point)))
         (str (buffer-substring-no-properties begin end)))
    ;;(highlight-region begin end)
    (repl-send-to (or proc repl-default-proc) str)))

(defun repl-send-paragraph (&optional proc)
  "Send region if selected, otherwise send the current paragraph."
  (interactive)
    (let* ((start (progn (backward-paragraph) (point)))
           (end (progn (forward-paragraph) (point)))
           (str (buffer-substring-no-properties start end)))
      ;;(highlight-region begin end)      
      (repl-send-to (or proc repl-default-proc) str)))

(defun repl-send-region (&optional proc)
  "Send the selected region to the process PROC."
  (interactive)
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (repl-send-to (or proc repl-default-proc) 
                    (buffer-substring-no-properties start end)))))

;; todo: sent buffer or region
(defun repl-send-buffer (&optional proc)
  "send the whole buffer"
  (interactive)
  ;;(highlight-region (point-min) (point-max))
  (repl-send-to (or proc repl-default-proc) (buffer-substring-no-properties (point-min) (point-max))))

(defun repl-send-markdown-block (&optional proc)
  "send current markdown code block, search block backwarnd and then forward"
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward "```" starting-pos t) (match-string 1)) nil))
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
        ;;(highlight-region start-content end-pos)
        (repl-send-to (or proc repl-default-proc) (buffer-substring-no-properties start-content end-pos))))))

;; TODO: send markdown block using defined proc in the code blocks it selfs

(defun repl-b-send-last-exp ()
  "send last expression to secondary proc"
  (interactive)
  (repl-send-last-exp repl-second-proc))

(defun repl-b-send-line ()
  "send line to secondary proc"
  (interactive)
  (repl-send-line repl-second-proc))

;; todo: buffer is sent or region selected
(defun repl-b-send-buffer ()
  "send buffer to secondary proc"
  (interactive)
  (repl-send-buffer repl-second-proc))

(defun repl-b-send-paragraph ()
  "send paragraph to secondary proc"
  (interactive)
  (repl-send-paragraph repl-second-proc))

(defun repl-b-send-region ()
  "send region to secondary proc"
  (interactive)
  (repl-send-region repl-second-proc))

(defun repl-b-send-markdown-block ()
  "send markdown-block to secondary proc"
  (interactive)
  (repl-send-markdown-block repl-second-proc))


(setq repl-mark "reload()") ;; default mark

(defun repl-set-mark ()
  (interactive)
  (setq repl-mark (read-string "set repl-mark: ")))

(defun repl-send-mark () ;; todo: make it able to send to the second process
  "send marked interctive"
  (interactive)
  (repl-send-to repl-default-proc repl-mark))

;; todo: send buffer or region ;;;

```

## git helper commit ansi term

```elisp

(defun create-git-proc ()
  "create persistence process git process"
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (get-buffer "*git*")
        (get-buffer-process "*git*")
      (get-buffer-process (ansi-term "/bin/zsh" "git"))
      (switch-to-buffer current-buffer))))

(defun git-commit ()
  "helper to quick commit C-c g m"
  (interactive)
  (save-excursion
    (let ((msg (read-string "commit messsage: "))
          (proc (create-git-proc)))
      (comint-send-string proc "git add .\n")
      (comint-send-string proc (format "git commit -m \"%s\"\n" msg))
      (message "Git add and commit initiated."))))

(defun git-push ()
  "helper to quick push C-c g p"
  (interactive)
  (let ((proc (create-git-proc)))
    (comint-send-string proc "git push origin HEAD\n")
    (message "Git push initiated.")))

```

## align

```elisp

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

```

## popper like, bare hand

```elisp 

(defvar my-popup-window nil
  "The current popup window, if any.")

(defun my-popup-open-or-switch (buffer-name)
  "Open a temporary popup window and display the specified BUFFER-NAME.
If the popup is already open, switch to the specified buffer."
  (interactive "BBuffer name: ")
  (let* ((buffer (get-buffer-create buffer-name))
         (popup-window (or my-popup-window
                           (split-window (frame-root-window)
                                         (floor (* 0.75 (window-total-height)))
                                         'below))))
    (set-window-buffer popup-window buffer)
    (setq my-popup-window popup-window)
    (select-window popup-window)))

(defun my-popup-close ()
  "Close the current popup window, if it exists."
  (interactive)
  (when (window-live-p my-popup-window)
    (delete-window my-popup-window)
    (setq my-popup-window nil)))

(defun my-popup-toggle ()
  "Toggle the popup window.
If it exists, close it. Otherwise, prompt for a buffer to open."
  (interactive)
  (if (window-live-p my-popup-window)
      (my-popup-close)
    (let ((buffer-name (read-buffer "Buffer name to display: ")))
      (my-popup-open-or-switch buffer-name))))

```

## delete word

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

