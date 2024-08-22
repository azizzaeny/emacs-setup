copy paste problem

```elisp
(when (eq system-type 'darwin) ;; Check if the system is macOS
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let* ((process-connection-type nil)
                 (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
            (process-send-string pbproxy text)
            (process-send-eof pbproxy))))) ;; Set interprogram-cut-function only for macOS

```
