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

(global-set-key (kbd "C-c C-y") 'copy-kill-ring-to-remote-clipboard)

(when (eq system-type 'gnu/linux) ;; Check if the system is Linux
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (with-temp-buffer
            (insert text)  ;; Insert the copied/cut text
            (write-file "~/clipboard/clipboard.txt"))))) ;; Write to the clipboard file on the server

```

```lisp 
;; (unless (display-graphic-p)
;;   (when (executable-find "xclip")
;;     (setq select-enable-clipboard t)
;;     (setq interprogram-cut-function
;;           (lambda (text &optional push)
;;             (with-temp-buffer
;;               (insert text)
;;               (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard"))))
;;     (setq interprogram-paste-function
;;           (lambda ()
;;             (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
;;               (unless (string= (car kill-ring) xclip-output)
;;                 xclip-output))))))
```

```lisp
;; (defun fetch-local-clipboard-to-emacs ()
;;   "Fetch the local macOS clipboard and insert it into Emacs kill ring."
;;   (interactive)
;;   (let ((text (shell-command-to-string "pbpaste")))
;;     (kill-new text)
;;     (message "Clipboard synced to Emacs kill ring.")))
;; (global-set-key (kbd "C-c C-l") 'fetch-local-clipboard-to-emacs)
```
