setup repl ansi socket browser

```elisp

(defvar repl-conn nil
  "Variable to hold the REPL connection for socket-based REPLs.")

(defvar current-repl-process nil
  "Current REPL process for ansi-term REPLs.")

(defvar repl-type "ansi" "Type of REPL: 'node', 'browser', or 'ansi'.")
(defvar repl-process "*ansi-term*") ;; main repl process window
(defvar repl-wrap  "%s")

(defun repl-set-process ()
  "set interactive repl process"
  (interactive)
  (setq repl-process (read-string "Enter repl process: "))) ;; change main process window

(defun repl-set-wrap ()
  "set interactive repl-wrap as client"
  (interactive)
  (setq repl-wrap (read-string "Enter wrap wrap: ")))

(defun repl-switch-type ()
  "Switch the `repl-type` to cycle between 'node', 'browser', and 'ansi'."
  (interactive)
  (setq repl-type (cond
                   ((equal repl-type "node") "browser")
                   ((equal repl-type "browser") "ansi")
                   ((equal repl-type "ansi") "node")))
  (message "Switched REPL type to %s" repl-type))

(defun repl-connect-socket ()
  "Connect to the socket-based REPL server if the type is 'node' or 'browser'."
  (interactive)
  (when (member repl-type '("node" "browser"))
    (let ((host "127.0.0.1")
          (port 1355))
      (setq repl-conn (open-network-stream "repl-conn" nil host port))
      (message "Connected to %s REPL server" repl-type))))

;; TODO: fix this repl-conn dont wait repl process
(defun repl-disconnect-socket ()
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

(defun repl-start-ansi (&optional cmd)
  "Start an ansi-term REPL using CMD or default to /bin/bash."
  (interactive)
  (setq cmd (or cmd "/bin/bash"))
  (setq repl-type "ansi")  ; Set the repl-type to 'ansi'
  (ansi-term cmd)
  ;; (setq current-repl-process (get-buffer-process (current-buffer)))
  ;; (term-line-mode) ; Enable line mode for easier sending of content
  (message "Started %s REPL" cmd))

;; two type send-to send-to-term-ansi-line send-to-term-ansi-char
(defun repl-send-to-term-ansi-line () ;; not used 
  (interactive)
  (with-current-buffer (process-buffer repl-process)
    (term-line-mode) ; Enable line mode for easier sending of content
    (goto-char (process-mark repl-process))
    (insert content)
    (term-send-input)))

(defun repl-send-to-term-ansi-char (content) ;; used this 
  (interactive)
  (term-send-string repl-process content)
  (term-send-input))

(defun repl-send-content (content)
  "Send CONTENT to the appropriate REPL based on `repl-type`."
  (cond
   ((member repl-type '("node" "browser"))    ;; For socket-based REPLs
    (when repl-conn
      (process-send-string repl-conn (concat content "\n"))))
   ((equal repl-type "ansi") ;; For ansi-term based REPLs
      ;;(send-to-term-ansi-line)
     (repl-send-to-term-ansi-char (format repl-wrap content)))   
   (t (message "Unknown REPL type: %s" repl-type))))


(defun repl-send-line ()
  "Send the current line to the REPL."
  (interactive)
  (repl-send-content (thing-at-point 'line t)))

(defun repl-send-paragraph ()
  "Send the current paragraph to the REPL."
  (interactive)
  (repl-send-content (thing-at-point 'paragraph t)))

(defun repl-send-region (start end)
  "Send the region between START and END to the REPL."
  (interactive "r")
  (repl-send-content (buffer-substring-no-properties start end)))

(defun repl-send-reload ()
  "sending common function on javascript to the REPL"
  (interactive)
  (repl-send-content "reload()"))

(defun repl-send-md-block ()
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))    
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward "```" starting-pos t) (match-string 1)) nil))
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
          (repl-send-content (buffer-substring-no-properties start-content end-pos)))
        )))

(defun repl-send-buffer ()
  "send the whole buffer"
  (interactive)
  (repl-send-content (buffer-substring-no-properties (point-min) (point-max))))

(message "repl loaded")

```

customzie key bind

```elisp

(global-set-key (kbd "C-c c p") 'repl-set-process)
(global-set-key (kbd "C-c c c") 'repl-connect-socket);
(global-set-key (kbd "C-c c d") 'repl-disconnect-socket);
(global-set-key (kbd "C-c c s") 'repl-start-ansi)
(global-set-key (kbd "C-c c w") 'repl-switch-type)
(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c r") 'repl-send-region)
(global-set-key (kbd "C-c c o") 'repl-send-reload)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c m") 'repl-send-md-block)

```

