copy paste problem

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

copy clipboard and paste clipboard
