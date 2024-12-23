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
repl 

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

git helper commit ansi term

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

