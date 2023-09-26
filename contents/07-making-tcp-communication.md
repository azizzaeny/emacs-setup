## TCP Client Server
(explain the idea)

#### concept 
interactive start a client given buffer-name, when start input read number of port that to creating a tcp server, when there is a message assume it is a lisp message that we can evaluate
eval it in the current emacs.

#### tcp server evaluation

```lisp
;; abbrev: tcpe => tcp-eval server

(defun tcpe-server-name (port)
  "format buffer tcp server"
  (format "tcpe:%d" port))

(defun tcpe-start (port)
  "starting tcp server listen at port"
  (interactive
   (list (read-number "Enter port number to listen" 9999)))
  (let* ((proc-name (tcpe-server-name port))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service port
                            :sentinel 'tcpe-sentinel
                            :filter 'tcpe-filter
                            :server 't)
      ;; (with-current-buffer buffer-name
      ;;   (funcall buffer-major-mode 'text-mode))
      )))

(defun tcpe-get-process (port)
  "get server process that listening on port"
  (get-process (tcpe-server-name port)))
 
(defun tcpe-stop (port)
  "stop emacs tcp server at port"
  (interactive
   (list (read-number "enter the port server" 9999)))
  (let ((server-proc (tcpe-get-process port)))
    (delete-process server-proc)))

(defun tcpe-filter (proc string)
  "evalualte it on top of wrapper"
  (eval (car (read-from-string (format "(progn %s)" string )))))

(defun tcpe-log (proc string)
  (let ((buffer (process-contact proc :buffer))
        (inhibit-read-only t))
    (and buffer (get-buffer buffer)
         (with-current-buffer buffer
           ;; (display-buffer buffer)
           (let ((moving (= (point) (point-max))))
             (save-excursion
               (goto-char (point-max))
               (insert string))
             (if moving (goto-char (point-max))))))))

(defun tcpe-sentinel (proc msg)
  (cond
   ((string-match "open from .*\n" msg)
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client connected added to client list \n")))    
   ((string= msg "connection broken by remote peer\n")
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "client quit \n")))
   ((eq (process-status proc) 'closed)
    (tcpe-log proc (format "%s %s: %s" (current-time-string) proc "delete clients")))))

;; (let ((proc (make-network-process :name "my sock"
;;                                   :host 'local    ;; or hostname string
;;                                   :service 9999)))
;;   ;;(process-send-string proc "(message \"hello socket world\")")
;;   (process-send-string proc "(find)")
;;   (sleep for 3)
;;   (delete-process proc))

;;(find-file "06-align-to-sign.md")
```


#### reference 
- [simple tcp client example](https://stackoverflow.com/questions/6162967/simple-tcp-client-examples-in-emacs-elisp)
- [jclosure emacs eval tcp-server](https://gist.github.com/jclosure/cb34dbd813c6bd1e3c4e128ad87d69c7)
