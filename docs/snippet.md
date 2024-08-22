
snippet 

```elisp
;; snippet
(defvar snippets (make-hash-table :test 'equal))
(puthash "tailwind" "<script src=\"https://cdn.tailwindcss.com/3.4.3\"></script>" snippets)
(puthash "viewport" "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">" snippets)
(puthash "html" "<!doctype html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"UTF-8\"/>\n    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">    \n    <script src=\"https://cdn.tailwindcss.com/3.4.3\"></script>\n    <script> console.clear();</script>\n    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n    <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@100..900&family=Playfair+Display:ital,wght@0,400..900;1,400..900&display=swap\" rel=\"stylesheet\">\n  </head>\n  <body class=\"antialiased font-sans m-0 p-0\">\n    \n  </body>\n</html>\n" snippets);
(puthash "jsmain" "\n    <script src=\"https://cdn.jsdelivr.net/gh/azizzaeny/hiccup@main/dist/index.def.js\"></script>\n    <script src=\"https://cdn.jsdelivr.net/gh/azizzaeny/clojure.core@main/dist/core.def.js\"></script>\n    <script>\n     var cs = (str) => ({ \"class\": str });\n     var className = (str) => ({ \"class\": str });\n     var isTarget = (e) => e.target.getAttribute('data-target');\n     var pushState = (e) => history.pushState(null, '', e.target.href);     \n     var reRender = (renderer) => (e) => (isTarget(e) ? (\n       e.preventDefault(e),\n       pushState(e),\n       renderer()\n     ) : null);\n     var routes = {}\n     var renderPage = () => {};\n     var main = () => {};\n     window.onload = main;\n     window.onclick = reRender(main);\n     window.onpopstate = main;\n    </script>" snippets)
(puthash "hiccup" "<script src=\"https://cdn.jsdelivr.net/gh/azizzaeny/hiccup@main/dist/index.def.js\"></script>" snippets)
(puthash "jsclojure" "<script src=\"https://cdn.jsdelivr.net/gh/azizzaeny/clojure.core@main/dist/core.def.js\"></script>" snippets)
(puthash "http" "var createServer, startServer, findFile\nvar server = server || null;\nvar main = () => {\n  server = createServer({ port: 8081, handler: (req, res) => handler(req, res)})\n  startServer(server); console.log('started server', 8081);  \n}\nvar routes = {\n  'GET /': 'mainHandler'\n};\nvar mainHandler = (req, res) => findFile(`${process.env.CONTEXT}/index.html`);\nvar handler = (req, res) => {\n  let resolve = routes[`${req.method} ${req.pathname}`];\n  if(resolve && global[resolve]) return global[resolve](req, res);\n  return response('404');\n}\nPromise.all([\n  deps('https://raw.githubusercontent.com/azizzaeny/clojure.core/main/dist/core.js'),  \n  deps('https://raw.githubusercontent.com/azizzaeny/http/main/index.js'),\n]).then(main);\n" snippets)
(puthash "binnode" "node -e \"var evaluate=(...args)=>{ let [vm=require('vm'), ctx=global] = args;  return (res) => vm.runInContext(res, Object.assign(vm.createContext(ctx), {console, require, module}));}; var deps=(url) => fetch(url).then(res => res.text()).then(evaluate()); process.env.CONTEXT='$1'; evaluate()(require('fs').readFileSync('./$1/index.js'));\" -i\n" snippets)
(puthash "jsdev" "      var version = 0;\n      var parseResponse = (res) => {\n        let ver = res.headers.get('x-version') || 0;\n        let next = parseInt(ver) + 1;\n        version = next;\n        return res.text();\n      }      \n      var setResponse = (res) => {\n        try{\n          eval(res);\n        }catch(err){\n          console.log(err);\n        }\n      }\n      var dev = url => fetch(url, {headers:{\"x-version\": version}}).then(parseResponse).catch((err) => (setResponse(res), setTimeout(()=> dev(url), 200)) ).then(res => (setResponse(res), setTimeout(()=> dev(url), 200)));" snippets)
(puthash "watchfile" "\nvar watch = (file, callback) => require('fs').watchFile(\n  file,\n  {persistent:true, interval:200 },\n  (prev, cur)=> callback()\n);\n" snippets)
(puthash "route" "var createParams = (match, route) => {\n  let keys = (route.match(/:\w+/g) || []).map(key => key.substring(1));\n  let values = match.slice(1);\n  return keys.reduce((acc, key, index) => (acc[key] = values[index], acc),{});\n}\nvar matchRoute = (pathname, routes) => {\n  for (let route in routes) {\n    let routeRegex = new RegExp('^' + route.replace(/:\w+/g, '(\\w+)') + '$');\n    let match = pathname.match(routeRegex);\n    if (match) {\n      let params = createParams(match, route);\n      return { match: routes[route], params };\n    }\n  }\n  return null;\n}\n" snippets)
(puthash "evaluate" "var evaluate=(...args)=>{\n  let [vm=require('vm'), ctx=global] = args;\n  return (res) => vm.runInContext(res, Object.assign(vm.createContext(ctx), {console, require, module}));\n};\nvar deps=(url) => fetch(url).then(res => res.text()).then(evaluate());\n" snippets)
;;(puthash "captureCode" "\nvar captureCodeBlocks = (markdown) => {\n  let codeBlockRegex = /```(\\w+)((?:\\s+\\w+=[\\w./-]+)*)\s*([\s\S]*?)```/g;\n  let matches = markdown.matchAll(codeBlockRegex);\n  let codeBlocks = Array.from(matches, match => {\n    let language = match[1];\n    let attributesString = match[2].trim();\n    let code = match[3].trim();\n    let params = attributesString.split(/\s+/).reduce((acc, attr)=>{\n      let [key, value] = attr.split('=');\n      if (key && value) {\n        acc[key] = value;\n      }\n      return acc;\n    }, {});    \n    return { language, params, code };\n  });\n  return codeBlocks;\n}\n" snippets)
(puthash "readFile" "var readFile = (file) => fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null;" snippets)

```

abbreviation at point

```elisp

(defun abbrev-at-point ()
  "Return the abbreviation at point."
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_:|-!<>")
                  (point))))
    (buffer-substring-no-properties start end)))

(defun insert-content-abbrev (content)
  (interactive)
  (progn
    (backward-kill-word 1)
    (insert content)))

(defun expand-abbrev-snippet ()
  (interactive)
  (let* ((abbrev (abbrev-at-point))
         (snippet-content (gethash abbrev snippets)))
    (if snippet-content
        (insert-content-abbrev snippet-content))))

(defun region-to-single-line ()
  "Replace newlines in region with '\\n' and concatenate into a single line."
  (interactive)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char start)
        (let ((region-text (buffer-substring-no-properties start end)))
          (setq region-text (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\"" "\\\\\"" region-text)))
          (kill-new region-text))))))

```


keybind snippet

```elisp

;; expand region
(global-set-key (kbd "C-c e") 'er/expand-region)


;; the snippets
;; (global-set-key (kbd "C-c s e") 'custom-snippet-expand)
;; (global-set-key (kbd "C-c s v") 'custom-snippet-eval)
;; (global-set-key (kbd "C-c s r") 'reload-snippets)
(global-set-key (kbd "C-c s e") 'expand-abbrev-snippet)
(global-set-key (kbd "C-c s l") 'region-to-single-line)

```
