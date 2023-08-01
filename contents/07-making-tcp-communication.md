## TCP Client Server
(explain the idea)

#### concept 
interactive start a client given buffer-name, when start input read number of port that to creating a tcp server, when there is a message assume it is a lisp message that we can evaluate
eval it in the current emacs.


#### Ex1: 
```lisp

(defvar tcp-listen-port 9999
  "port of the server")

(defvar tcp-listen-host "127.0.0.1"
  "host the server")

;; start by making client listener
(defun tcp-listen-start nil
  "start network tcp client"
  (interactive)
  (make-network-process :name "tcp-listen" :buffer "*tcp-listen*" :family 'ipv4 :host tcp-listen-host :service tcp-listen-port :sentinel 'tcp-listen-sentinel :filter 'tcp-listen-filter))

(defun tcp-listen-stop nil
  "stop network tcp client"
  (interactive)
  (delete-process "tcp-listen"))

(defun tcp-listen-filter (proc string)
  (message string)) ;; just print it

(defun tcp-listen-sentinel (proc msg)
  (when (string msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))

;;https://stackoverflow.com/questions/6162967/simple-tcp-client-examples-in-emacs-elisp
;;(tcp-listen-start)
;; (sleep for 300)
;;(tcp-listen-stop)
```

#### Ex2:
```lisp

(defun tcp-server-name (port)
  "format buffer tcp server"
  (format "tcp-server:%d" port))

(defun tcp-listen-start (port)
  "starting tcp server listen at port"
  (interactive
   (list (read-number "Enter port number to listen" 9999)))
  (let* ((proc-name (tcp-server-name port))
         (buffer-name (format "*%s*" proc-name)))
    (unless (process-status proc-name)
      (make-network-process :name proc-name :buffer buffer-name
                            :family 'ipv4 :service port
                            :sentinel 'tcp-server-sentinel
                            :filter 'tcp-server-filter
                            :server 't)
      (with-current-buffer buffer-name
        (funcall buffer-major-mode 'text-mode)))))

(defun tcp-server-get-process (port)
  "get server process that listening on port"
  (get-process (tcp-server-name port)))

(defun tcp-server-stop (port)
  "stop emacs tcp server at port"
  (interactive
   (list (read-number "enter the port server" 9999)))
  (let ((server-proc (tcp-server-get-process port)))
    (delete-process server-proc)))

(defun tcp-server-filter (proc string)
  (eval (car (read-from-string (format "(progn %s)" string )))))

(defun tcp-server-render-buffer (proc string)
  (let ((buffer (process-contact proc :buffer))
        (inhibit-read-only t))
    (and buffer (get-buffer buffer)
         (with-current-buffer buffer
           (display-buffer buffer)
           (let ((moving (= (point) (point-max))))
             (save-excursion
               (goto-char (point-max))
               (insert string))
             (if moving (goto-char (point-max))))))))

(defun tcp-server-log (client string)
  (tcp-server-render-buffer client (format "%s %s: %s" (current-time-string) client string)))

(defun tcp-server-sentinel (proc msg)
  (cond
   ((string-match "open from .*\n" msg)
    (tcp-server-log proc "client connected added to client list \n"))
   ((string= msg "connection broken by remote peer\n")
    (tcp-server-log proc "client quit \n"))
   ((eq (process-status proc) 'closed)
    (tcp-server-log proc "delete clients"))))


(let ((proc (make-network-process :name "my sock"
                                  :host 'local    ;; or hostname string
                                  :service 9999)))
  (process-send-string proc "(message \"hello socket world\")")
  (sleep for 300)
  (delete-process proc))

;; https://gist.github.com/jclosure/cb34dbd813c6bd1e3c4e128ad87d69c7

```

sample nodejs tcp server client
 
```js

var net = require('net');

var server = net.createServer(function(socket) {
  socket.write('Echo server\r\n');
  socket.pipe(socket);
});

server.listen(9999, '127.0.0.1');
//;; nc localhost 9999

```
sample nodejs tcp client

```js

var net = require("net");
var client = new net.Socket();

client.connect(9999, '127.0.0.1', function() {
	console.log('Connected');
	client.write('Hello, server! Love, Client.');
});

client.on('data', function(data) {
	console.log('Received: ' + data);
	client.destroy(); // kill client after server's response
});

client.on('close', function() {
	console.log('Connection closed');
});

```
