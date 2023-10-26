setup repl ansi socket browser

```elisp


(defvar repl-conn nil
  "Variable to hold the REPL connection for socket-based REPLs.")

(defvar current-repl-process nil
  "Current REPL process for ansi-term REPLs.")

(message "repl loaded")

(defvar repl-type "node" "Type of REPL: 'node', 'browser', or 'ansi'.")

(defun switch-type ()
  "Switch the `repl-type` to cycle between 'node', 'browser', and 'ansi'."
  (interactive)
  (setq repl-type (cond
                   ((equal repl-type "node") "browser")
                   ((equal repl-type "browser") "ansi")
                   ((equal repl-type "ansi") "node")))
  (message "Switched REPL type to %s" repl-type))

(defun connect-repl ()
  "Connect to the socket-based REPL server if the type is 'node' or 'browser'."
  (interactive)
  (when (member repl-type '("node" "browser"))
    (let ((host "127.0.0.1")
          (port 1355))
      (setq repl-conn (open-network-stream "repl-conn" nil host port))
      (message "Connected to %s REPL server" repl-type))))

(defun disconnect-repl ()
  "Disconnect from the current REPL."
  (interactive)
  (cond
   ((member repl-type '("node" "browser"))
    (when repl-conn
      (delete-process repl-conn)
      (setq repl-conn nil)
      (message "Disconnected from %s REPL server" repl-type)))
   ((equal repl-type "ansi")
    (when current-repl-process
      (delete-process current-repl-process)
      (setq current-repl-process nil)
      (message "Closed %s REPL" repl-type)))))

(defun start-ansi-repl (&optional cmd)
  "Start an ansi-term REPL using CMD or default to /bin/bash."
  (interactive)
  (setq cmd (or cmd "/bin/sh"))
  (setq repl-type "ansi")  ; Set the repl-type to 'ansi'
  (ansi-term cmd)
  (setq current-repl-process (get-buffer-process (current-buffer)))
  (term-line-mode) ; Enable line mode for easier sending of content
  (message "Started %s REPL" cmd))

(defun send-to-term-ansi-line ()
  (interactive)
  (with-current-buffer (process-buffer current-repl-process)
    (term-line-mode) ; Enable line mode for easier sending of content
    (goto-char (process-mark current-repl-process))
    (insert content)
    (term-send-input)))

(defun send-to-term-ansi-char (content)
  (interactive)
  (term-send-string "*ansi-term*" content)
  ;;(term-send-input)
  )

(defun send-to-repl (content)
  "Send CONTENT to the appropriate REPL based on `repl-type`."
  (cond
   ;; For socket-based REPLs
   ((member repl-type '("node" "browser"))
    (when repl-conn
      (process-send-string repl-conn (concat content "\n"))))
   ;; For ansi-term based REPLs
   ((equal repl-type "ansi")
    (when current-repl-process
       ;; (send-to-term-ansi-line)
      (send-to-term-ansi-char content)
     ))   
   (t (message "Unknown REPL type: %s" repl-type))))

(defun send-line ()
  "Send the current line to the REPL."
  (interactive)
  (send-to-repl (thing-at-point 'line t)))

(defun send-paragraph ()
  "Send the current paragraph to the REPL."
  (interactive)
  (send-to-repl (thing-at-point 'paragraph t)))

(defun send-region (start end)
  "Send the region between START and END to the REPL."
  (interactive "r")
  (send-to-repl (buffer-substring-no-properties start end)))

(defun send-reload ()
  "sending common function on javascript to the REPL"
  (interactive)
  (send-to-repl "reload()"))

```

customzie key bind

```elisp

(global-set-key (kbd "C-c c c") 'connect-repl)
(global-set-key (kbd "C-c c s") 'start-ansi-repl)
;;(global-set-key (kbd "C-x a") 'start-ansi-repl)
(global-set-key (kbd "C-c c w") 'switch-type)
(global-set-key (kbd "C-c c l") 'send-line)
(global-set-key (kbd "C-c c r") 'send-region)
(global-set-key (kbd "C-c c o") 'send-reload)
(global-set-key (kbd "C-c c e") 'send-paragraph)

```

