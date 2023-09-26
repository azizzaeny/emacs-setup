sending code through network repl server

```lisp
;; experiementing testing sending basic message to tcp
(defun send-code-message ()
  "send code msg to custom REPL Server"
  (let* ((host "127.0.0.1")
         (port 1355)
         (msg "{\"code\": \"console.log('bar bar');\"}\n")
         (coding-system-for-write 'utf-8)
         (coding-system-for-read 'utf-8)
         (conn (open-network-stream "repl-conn" nil host port))         
         )
    (process-send-string conn msg)
    (set-process-filter conn (lambda (proc string)
                               (message string)))
    (process-send-eof conn)
    )
  )

(send-code-message)
```


M-x connect-repl will connect to given repl
M-x send-line is sending current line
M-x send-paragraph sending paragraph
M-x send-region after selecting region send it
M-x disconnect-repl

every send is send with code

```elisp

(defvar repl-conn nil
  "Variable to hold the REPL connection.")

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

```
key binding

```elisp
(global-set-key (kbd "C-c c l") 'send-line)
(global-set-key (kbd "C-c c s") 'connect-repl)
(global-set-key (kbd "C-c c r") 'send-region)
(global-set-key (kbd "C-c c e") 'send-paragraph)
```
