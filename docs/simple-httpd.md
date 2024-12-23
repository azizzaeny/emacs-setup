simple httpd

```elisp

(require 'simple-httpd)

(defun httpd-start-with-port (port)
  "Prompt for a port number and start simple-httpd with that port."
  (interactive "nEnter port number: ")
  (setq httpd-port port)
  (httpd-start)
  (message "simple-httpd started on port %d" httpd-port))

;; (setq httpd-port 19008) ;; secret port playground

;; (setq server-use-tcp t)
;; (setq server-host "iodev")
;; (server-start)
```


