
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

;; ansi-term
(global-set-key (kbd "C-x p a") 'create-ansi-proc)

;; rename buffer
(global-set-key (kbd "C-x r b") 'rename-buffer)

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

(global-set-key (kbd "C-x p b") 'create-browser-repl)

```


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


(defun repl-send-region (&option proc)
  "Send region"
  (interactive "r")
  ;;(highlight-region begin end)
  (repl-send-to (or proc repl-default-proc) (buffer-substring-no-properties start end)))

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
  (repl-send-to repl-default-proc  repl-mark))

;; todo: send buffer or region
(global-set-key (kbd "C-c c 1") 'repl-set-default-proc)
(global-set-key (kbd "C-c c 2") 'repl-set-second-proc)
(global-set-key (kbd "C-c c n") 'repl-set-mark)

(global-set-key (kbd "C-c c k") 'repl-send-mark)
(global-set-key (kbd "C-c c s") 'repl-send-last-exp)
(global-set-key (kbd "C-c c l") 'repl-send-line)
(global-set-key (kbd "C-c c e") 'repl-send-paragraph)
(global-set-key (kbd "C-c c b") 'repl-send-buffer)

(global-set-key (kbd "C-c c m") 'repl-send-markdown-block)
(global-unset-key (kbd "C-c b"))
(global-set-key (kbd "C-c b s") 'repl-b-send-last-exp)
(global-set-key (kbd "C-c b l") 'repl-b-send-line)
(global-set-key (kbd "C-c b b") 'repl-b-send-buffer)
(global-set-key (kbd "C-c b e") 'repl-b-send-paragraph)
(global-set-key (kbd "C-c b m") 'repl-b-send-markdown-block)

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

(global-set-key (kbd "C-x p g") 'create-git-proc)

(defun git-commit ()
  "helper to quick commit C-c g m"
  (interactive)
  (save-excursion
    (let ((msg (read-string "commit messsage: "))
          (proc (create-git-proc)))
      (comint-send-string proc "git add .\n")
      (comint-send-string proc (format "git commit -m \"%s\"\n" msg))
      (message "Git add and commit initiated."))))

(global-set-key (kbd "C-c g c") 'git-commit)

(defun git-push ()
  "helper to quick push C-c g p"
  (interactive)
  (let ((proc (create-git-proc)))
    (comint-send-string proc "git push origin HEAD\n")
    (message "Git push initiated.")))

(global-set-key (kbd "C-c g p") 'git-push)
;; todo: top level project dir
;; git helper to quick commit



```

repl nodejs

```elisp
(defun create-node-repl ()
  "create node.js repl"
  (interactive)
  (if (get-buffer "*Node*")
      (get-buffer-process "*Node*")      
    (get-buffer-process (ansi-term "node" "Node"))))

(global-set-key (kbd "C-x p n") 'create-node-repl)
```

experiment create send to repl

```lisp

;; repl
(defvar repl-process "*ansi-term*") ;; main repl process window
(defvar repl-wrap  "%s")

(defface my-highlight-face
  '((t (:background "##f0f8ff"))) ; Customize background color here
  "Face for highlighting text."
  :group 'basic-faces)

(defun highlight-region (start end)
  (let ((region-highlight (make-overlay start end)))
    (overlay-put region-highlight 'face 'my-highlight-face)
    (run-at-time "0.3 sec" nil #'delete-overlay region-highlight))
  (deactivate-mark))

(defun repl-start-ansi ()
  (interactive)
  (setq cmd (read-string "Enter cmd: " "/bin/zsh"))  
  (ansi-term cmd)
  (messsage "Started %s REPL" repl-process))

(defun repl-set-process ()
  "set interactive repl process"
  (interactive)
  (setq repl-process (read-string "Enter repl process: "))) ;; change main process window

(defun repl-set-wrap ()
  "set interactive repl-wrap as client"
  (interactive)
  (setq repl-wrap (read-string "Enter wrap text: ")))

(defun repl-escape-char (content)
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\"" "\\\\\"" content)))

(defun repl-send-content (content)
  "simulate entering into repl-process"
  (interactive)
  (with-current-buffer repl-process
    (goto-char (point-max))
    (term-line-mode)
    (insert (format repl-wrap content))    
    (term-send-input)
    (term-char-mode)))

(defun repl-send-buffer ()
  "send the whole buffer"
  (interactive)
  (highlight-region (point-min) (point-max))
  (message "send b")  
  (repl-send-content (buffer-substring-no-properties (point-min) (point-max))))

(defun escape-backtick (str)
  "Replace all occurrences of backtick (`) with escaped form (\\`)"
  (replace-regexp-in-string "`" "\\\\`" str))

(defun repl-send-buffer-escape ()
  "send the whole buffer escape backtick"
  (interactive)
  (highlight-region (point-min) (point-max))
  (message "send b escape")  
  (repl-send-content (escape-backtick (buffer-substring-no-properties (point-min) (point-max)))))

(defun repl-send-line ()
  "Send the current line to the REPL."
  (interactive)  
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (highlight-region start end)
      (message "send n: %s %s" start end)
      (repl-send-content (buffer-substring-no-properties start end))
      ;; (repl-send-content (thing-at-point 'line t))
      )))

(defun repl-send-paragraph ()
  "Send the current paragraph to the REPL."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (highlight-region start end)
      (message "send e: %s %s" start end)      
      (repl-send-content (buffer-substring-no-properties start end))
      ;;(repl-send-content (thing-at-point 'paragraph t))
      )))


(defun repl-send-region (start end)
  "Send the region between START and END to the REPL."
  (interactive "r")
  (highlight-region start end)
  (message "send r: %s %s" start end)  
  (repl-send-content (buffer-substring-no-properties start end)))

(defun repl-send-md-block ()
  (interactive)
  (save-excursion
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))    
          (end-pos (progn (re-search-forward md-block-end (point-max) t) (match-beginning 0))))
      (let ((file-ref (or (progn (re-search-backward "```" starting-pos t) (match-string 1)) nil))
            (start-content (progn (goto-char starting-pos) (beginning-of-line) (forward-line 1) (point))))
        (highlight-region start-content end-pos)
        (message "send m: %s %s" start-content end-pos)  
        (repl-send-content (buffer-substring-no-properties start-content end-pos))
        )
      )))

(defun repl-send-eol ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:")))
    (repl-send-content (format "cat > %s <<'EOL'" file-loc))))

(defun repl-send-eol-output-region ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:"))
        (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (repl-send-content (format "cat > %s <<'EOL'\n%s\nEOL" file-loc content))))

(defun repl-send-eol-output-buffer ()
  "utility tools to make it easier for us to create files for this entire buffer"
  (interactive)
  (let ((file-loc (read-string "Path:"))
        (content (buffer-substring-no-properties (point-min) (point-max))))                 
    (repl-send-content (format "cat > %s <<'EOL'\n%sEOL" file-loc content))))

(defun repl-send-reload ()
  "send default funciton reload"
  (interactive)
  (repl-send-content "reload()"))

(defun repl-send-main ()
  "send default funciton reload"
  (interactive)
  (repl-send-content "main()"))

(defun repl-send-client-region (start end)
  "Send region pre configured string"
  (interactive "r")
  (message "send r: %s %s" start end)  
  (highlight-region start end)
  (setq repl-wrap "responseWith(`%s`)")
  (repl-send-content (buffer-substring-no-properties start end))
  (setq repl-wrap "%s"))

(defun js-comint-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (let* ((b (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (e (if (and (boundp 'evil-mode)
                     evil-mode
                     (eq evil-state 'normal))
                (+ 1 (point))
              (point)))
         (str (buffer-substring-no-properties b e)))
    
```

experiment test creating node.js

```lisp

(defun start-node-repl ()
  "Start a Node.js REPL in `ansi-term`."
  (interactive)
  (ansi-term "node" "Node.js REPL"))

(defun switch-to-or-start-node-repl ()
  "Switch to an existing Node.js REPL or start a new one."
  (interactive)
  (let ((buffer (get-buffer "*Node.js REPL*")))
    (if buffer
        (switch-to-buffer buffer)
      (start-node-repl))))

(defun start-node-repl-with-hello ()
  "Start a Node.js REPL in `ansi-term`, define and invoke the `hello` function."
  (interactive)
  (ansi-term "node" "Node.js REPL")
  (let ((proc (get-buffer-process "*Node.js REPL*")))
    (comint-send-string proc "function hello(){ return 1;}\nhello();\n")
    (comint-send-string proc "hello()\n")))

(defvar node-repl-timer nil
  "Timer to delay sending `hello(1)` to the Node.js REPL.")

(defun send-hello-to-node-repl ()
  "Send `hello(1)` to the Node.js REPL if it exists."
  (let ((buffer (get-buffer "*Node.js REPL*")))
    (when buffer
      (let ((proc (get-buffer-process buffer)))
        (when proc
          (comint-send-string proc "release();\n"))))))

(defun schedule-send-hello-to-node-repl ()
  "Schedule sending `hello(1)` to the Node.js REPL after a delay."
  (when node-repl-timer
    (cancel-timer node-repl-timer))
  (setq node-repl-timer (run-with-idle-timer 0.2 nil 'send-hello-to-node-repl)))


(defun start-or-switch-to-node-repl-with-hello ()
  "Start a Node.js REPL in `ansi-term` if it doesn't already exist,
change the working directory to the current buffer's directory,
define and invoke the `hello` function."
  (interactive)
  (let* ((buffer (get-buffer "*Node.js REPL*"))
         (current-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (if buffer
        (with-current-buffer buffer
          (let ((proc (get-buffer-process buffer)))
            (when proc
              (comint-send-string proc "console.log('process exists');"))))
      (ansi-term "node" "Node.js REPL")
      (let ((proc (get-buffer-process "*Node.js REPL*")))
        (when proc
          (comint-send-string proc (format "process.env.CONTEXT='%s';process.chdir('%s');\n" current-dir current-dir))
          (comint-send-string proc "var evaluate=(...args)=>{\n  let [vm=require('vm'), ctx=global] = args;  return (res) => vm.runInContext(res, Object.assign(vm.createContext(ctx), {console, require, module}));\n};\nvar deps=(url) => fetch(url).then(res => res.text()).then(evaluate());\nvar createServer, startServer, findFile, response;\nvar esprima, escodegen;\nvar server = server || null;\nvar main = () => {\n  server = createServer({ port: 8081, handler: (req, res) => handler(req, res)})\n  startServer(server); console.log('started server', 8081);  \n}\nvar version = 0;\nvar requests = [];\nvar release = () => {\n  version = version + 1;\n  console.log('release', version);\n  requests.forEach(([req, res]) =>{\n    let path = req.pathname === \"/\" ? \"index.html\" : req.pathname;    \n    return res.end(getFile(`${process.env.CONTEXT}/${path}`, '404'));\n  });\n  requests = [];\n}\n\nvar parseCode = (code, es5) => {\n  if(es5){\n    return esprima.parseScript(code, { range: true, tolerant: true});\n  }else{\n    return esprima.parseModule(code, { range: true, tolerant: true });\n  }\n}\nvar generateCode = (ast) => {\n  return escodegen.generate(ast);\n}\nvar transformType = {\n  'VariableDeclaration': (node)=>{\n    return {\n      \"type\": \"ExpressionStatement\",\n      \"expression\": {\n        \"type\": \"AssignmentExpression\",\n        \"operator\": \"=\",\n        \"left\": node.declarations[0].id,        \n        \"right\": node.declarations[0].init,\n        \"range\": []\n      },\n      \"range\": node.range\n    }\n  },\n  'FunctionDeclaration': (node)=>{\n    return {\n      \"type\": \"ExpressionStatement\",\n      \"expression\": {\n        \"type\": \"AssignmentExpression\",\n        \"operator\": \"=\",\n        \"left\": node.id,        \n        \"right\": {\n          \"type\": \"ArrowFunctionExpression\",\n          \"id\": null,\n          \"params\": node.params,\n          \"body\": node.body\n        },\n        \"range\": []\n      },\n      \"range\": node.range\n    }\n  }\n}\nvar traverse  = (acc, node, index) => {\n  if(transformType[node.type]){\n    let transform = transformType[node.type](node);\n    return acc.concat(transform);\n  }else{\n    return acc.concat(node);\n  }\n}\nvar transformCode = (ast) => {\n  return {\n    ...ast,\n    body: ast.body.reduce(traverse, []).flat().filter(e=> e !== null),\n    sourceType: \"module\",\n    ecmaVersion: \"latest\"\n  }\n}\nvar generateSafeCode = (code, context) => {\n  return generateCode(transformCode(parseCode(code)));\n}\n\nvar ext = (file) => require('path').extname(file).slice(1);\nvar responseFileVersion = (version, path) => {\n  let fullPath = `${process.env.CONTEXT}/${path}`;\n  let file = getFile(fullPath, '404');\n  let isJs = ext(fullPath) === \"js\";\n  return {\n    status: 200,\n    headers: {'x-version': version},\n    body: ((file !== '404' && isJs) ? generateSafeCode(file) : file)\n  }\n}\nvar getFile = (filePath, defaultContent) => fs.existsSync(filePath) ? fs.readFileSync(filePath, 'utf-8') : defaultContent;\nvar handler = (req, res) => {\n  let path = req.pathname === \"/\" ? \"index.html\" : req.pathname;\n  let req_version = parseInt(req.headers['x-version']) || 0;\n  if(!req_version){\n    return responseFileVersion(version, path);\n  }     \n  if(version === req_version){\n    return responseFileVersion(version, path);\n  }\n  requests.push([req, res]);\n  return null; \n}\nvar watch = (dir, callback) => require('fs').watch(\n  dir,\n  (prev, cur)=> callback()\n);\n")
          (comint-send-string proc "watch(process.env.CONTEXT, release);\nPromise.all([\n  deps('https://cdn.jsdelivr.net/npm/escodegen-browser@1.11.1/escodegen.browser.js'),\n  deps('https://cdn.jsdelivr.net/npm/esprima@4.0.1'),\n  deps('https://raw.githubusercontent.com/azizzaeny/http/main/index.js'),\n]).then(main);\n")
          )))))

(global-set-key (kbd "C-c s n") 'start-or-switch-to-node-repl-with-hello)

(defun repl-send-markdown-block (&optional proc)
  "Send current markdown code block to a REPL process based on markdown parameter.
Search for block backward and forward, extract language and proc if available."
  (interactive)
  (save-excursion
    ;; Find the beginning of the code block (``` ...)
    (let ((starting-pos (progn (re-search-backward "^```" (point-min) t) (match-end 0)))
          (end-pos (progn (re-search-forward "^```" (point-max) t) (match-beginning 0)))
          lang proc-param)
      
      ;; Extract the language and optional proc parameter from the block start line
      (save-excursion
        (goto-char starting-pos)
        (when (looking-at "\\([a-zA-Z]+\\)\\(?:[ \t]+proc=\\([a-zA-Z0-9]+\\)\\)?")
          (setq lang (match-string 1))        ;; Capture language, e.g., js
          (setq proc-param (match-string 2)))) ;; Capture proc, e.g., node

      ;; Adjust starting position to skip the opening ``` line
      (let ((start-content (progn (goto-char starting-pos) (forward-line 1) (point))))
        
        ;; Highlight the code block
        (highlight-region start-content end-pos)
        
        ;; Send the code block to the appropriate process (either proc-param or default)
        (repl-send-to (or proc-param (or proc repl-default-proc))
                      (buffer-substring-no-properties start-content end-pos))))))

;; Example key binding
(global-set-key (kbd "C-c c b") 'repl-send-markdown-block)

```

keybind repl

```lisp
;; the repl
;; (global-set-key (kbd "C-c c p") 'repl-set-process)

;; (global-set-key (kbd "C-c c c") 'repl-connect-socket);
;; (global-set-key (kbd "C-c c d") 'repl-disconnect-socket);
;; (global-set-key (kbd "C-c c s") 'repl-start-ansi)

;; (global-set-key (kbd "C-c c w") 'repl-set-wrap)
;; (global-set-key (kbd "C-c c l") 'repl-send-line)
;; (global-set-key (kbd "C-c c r") 'repl-send-region)
;; (global-set-key (kbd "C-c c o") 'repl-send-eol-output-region)
;; (global-set-key (kbd "C-c c b") 'repl-send-buffer)
;; (global-set-key (kbd "C-c c g") 'repl-send-buffer-escape)
;; (global-set-key (kbd "C-c c e") 'repl-send-paragraph)
;; (global-set-key (kbd "C-c c m") 'repl-send-md-block)
;; (global-set-key (kbd "C-c c c") 'repl-send-client-region);
;; (global-set-key (kbd "C-c c k") 'repl-send-reload);
;; (global-set-key (kbd "C-c c j") 'repl-send-main);

```
