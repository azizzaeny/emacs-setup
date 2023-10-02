(message "repl nil")
;; (defvar repl-conn nil
;;   "Variable to hold the REPL connection.")

;; (defvar repl-type "node" "Type of REPL: 'node' or 'browser'.")

;; (defun switch-type ()
;;   "Switch the `repl-type` between 'node' and 'browser'."
;;   (interactive)
;;   (setq repl-type (if (equal repl-type "node") "browser" "node"))
;;   (message "Switched REPL type to %s" repl-type))

;; (defun connect-repl ()
;;   "Connect to the custom REPL server."
;;   (interactive)
;;   (let ((host "127.0.0.1")
;;         (port 1355)
;;         (coding-system-for-write 'utf-8)
;;         (coding-system-for-read 'utf-8))
;;     (setq repl-conn (open-network-stream "repl-conn" nil host port))
;;     (set-process-filter repl-conn (lambda (proc string)
;;                                     (message string)))
;;     (message "Connected to REPL server.")))

;; (defun send-code-message (code-str)
;;   "Send a given CODE-STR as a message to the connected REPL server."
;;   (let* ((path (or (buffer-file-name) "unknown"))
;;          (at (number-to-string (point)))
;;          (line (number-to-string (line-number-at-pos)))
;;          (file (or (file-name-nondirectory path) "unknown"))
;;          (data `(("code" . ,code-str)
;;                  ("path" . ,path)
;;                  ("type" . ,repl-type)
;;                  ("at" . ,at)
;;                  ("line" . ,line)
;;                  ("file" . ,file)))
;;          (msg (concat (json-encode data) "\n")))
;;     (process-send-string repl-conn msg)))

;; (defun disconnect-repl ()
;;   "Disconnect from the custom REPL server."
;;   (interactive)
;;   (when repl-conn
;;     (delete-process repl-conn)
;;     (setq repl-conn nil)
;;     (message "Disconnected from REPL server.")))

;; (defun send-line ()
;;   "Send the line where the cursor is positioned."
;;   (interactive)
;;   (let ((line-str (thing-at-point 'line t)))
;;     (send-code-message line-str)))

;; (defun send-paragraph ()
;;   "Send the paragraph where the cursor is positioned."
;;   (interactive)
;;   (let ((paragraph-str (thing-at-point 'paragraph t)))
;;     (send-code-message paragraph-str)))

;; (defun send-region (start end)
;;   "Send the current selected region from START to END."
;;   (interactive "r")
;;   (let ((region-str (buffer-substring-no-properties start end)))
;;     (send-code-message region-str)))


;; (global-set-key (kbd "C-c c w") 'switch-type)
;; (global-set-key (kbd "C-c c l") 'send-line)
;; (global-set-key (kbd "C-c c s") 'connect-repl)
;; (global-set-key (kbd "C-c c r") 'send-region)
;; (global-set-key (kbd "C-c c e") 'send-paragraph)


;; (defun tcpe-server-name (port)
;;   "format buffer tcp server"
;;   (format "tcpe:%d" port))

;; (defun tcpe-start (port)
;;   "starting tcp server listen at port"
;;   (interactive
;;    (list (read-number "Enter port number to listen" 9999)))
;;   (let* ((proc-name (tcpe-server-name port))
;;          (buffer-name (format "*%s*" proc-name)))
;;     (unless (process-status proc-name)
;;       (make-network-process :name proc-name :buffer buffer-name
;;                             :family 'ipv4 :service port
;;                             :sentinel 'tcpe-sentinel
;;                             :filter 'tcpe-filter
;;                             :server 't)
      ;; (with-current-buffer buffer-name
      ;;   (funcall buffer-major-mode 'text-mode))
;;      )))

;; (defun tcpe-get-process (port)
;;   "get server process that listening on port"
;;   (get-process (tcpe-server-name port)))
 
;; (defun tcpe-stop (port)
;;   "stop emacs tcp server at port"
;;   (interactive
;;    (list (read-number "enter the port server" 9999)))
;;   (let ((server-proc (tcpe-get-process port)))
;;     (delete-process server-proc)))

;; (defun tcpe-filter (proc string)
;;   "evalualte it on top of wrapper"
;;   (eval (car (read-from-string (format "(progn %s)" string )))))

;; (defun tcpe-log (proc string)
;;   (let ((buffer (process-contact proc :buffer))
;;         (inhibit-read-only t))
;;     (and buffer (get-buffer buffer)
;;          (with-current-buffer buffer
;;            ;; (display-buffer buffer)
;;            (let ((moving (= (point) (point-max))))
;;              (save-excursion
;;                (goto-char (point-max))
;;                (insert string))
;;              (if moving (goto-char (point-max))))))))

;; (defun tcpe-sentinel (proc msg)
;;   (cond
;;    ((string-match "open from .*\n" msg)
;;     (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client connected added to client list \n")))    
;;    ((string= msg "connection broken by remote peer\n")
;;     (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client quit \n")))
;;    ((eq (process-status proc) 'closed)
;;     (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "delete clients")))))

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

