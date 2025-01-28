### tailwind
```html name=scriptTailwind
<script src=\"https://cdn.tailwindcss.com/3.4.3\"></script>
```

### view port
```html name=metaViewport
<meta name="viewport" content="width=device-width,initial-scale=1.0">
```

### html tailwind initial
```js name=docHtml
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0">    
    <script src="https://cdn.tailwindcss.com/3.4.3"></script>
  </head>
  <body class="antialiased">    
  </body>
</html>
```

### watch file
```js name=watchFile
var watchFile = (file, callback) => require('fs').watchFile(
  file,
  {persistent:true, interval:200 },
  (prev, cur)=> callback()
);
```

### watch dir 
```js name=watchDir
var watchDir = (file, callback) => require('fs').watch(
  file,
  {persistent:true },
  (prev, cur)=> callback()
)
```

### Read file
```js name=readFile
var readFile = (file) => require('fs').existsSync(file) ? require('fs').readFileSync(file, 'utf8') : null;
```

### node eval
```sh name=nodeEval
node -e "var evaluate=(...args)=>{ let [vm=require('vm'), ctx=global] = args;  return (res) => vm.runInContext(res, Object.assign(vm.createContext(ctx), {console, require, module}));}; var deps=(url) => fetch(url).then(res => res.text()).then(evaluate()); process.env.CONTEXT='$1'; evaluate()(require('fs').readFileSync('./$1/index.js'));" -i
```
todo: node eval cat 

### evaluate deps
```js name=evaluateVm
var evaluate=(res)=> require('vm').runInContext(res, Object.assign(require('vm').createContext(global), {console, require, module}));
var deps=(url) => fetch(url).then(res => res.text()).then(evaluate);
```

### basic routing
```js name=routeParams
var createParams = (match, route) => {
  let keys = (route.match(/:w+/g) || []).map(key => key.substring(1));
  let values = match.slice(1);
  return keys.reduce((acc, key, index) => (acc[key] = values[index], acc),{});
}
var matchRoute = (pathname, routes) => {
  for (let route in routes) {
    let routeRegex = new RegExp('^' + route.replace(/:w+/g, '(\w+)') + '$');
    let match = pathname.match(routeRegex);
    if (match) {
      let params = createParams(match, route);
      return { match: routes[route], params };
    }
  }
  return null;
}
```

### docker node
```sh name=dockerNodeMount
docker run --name test --rm -it -w /work --network host -v $(pwd):/work node:20-alpine /bin/sh -c "node && /bin/sh"
```

### docker node 
```sh name=dockerNode 
docker run --name test --rm -it -w /work --network host node:20-alpine node
docker exec -it test /bin/sh
```

### docker redis
```sh name=dockerRedisConfig
cat > conf/redis.conf <<EOF
bind 0.0.0.0
port 6379
appendonly yes
maxmemory 8192mb
requirepass redispass
save 900 1
save 60 100
EOF
docker run --rm -it --name redis-stack --network=host --workdir=/work -v "$(pwd)/conf/":/work/conf redis/redis-stack-server:7.2.4 /bin/sh
redis-server conf/redis.conf
```
```sh name=dockerRedis
docker run --rm -it --name redis-stack --network=host --workdir=/work -v "$(pwd)/conf/":/work/conf redis/redis-stack-server:7.2.4
```

### ssh forwarding to local
ssh -L 13045:localhost:13405 -AJ kbm-knife kbm-redis
```sh name=sshForward
ssh -L 11001:localhost:11001 server
```
## ssh jump 
```sh name=sshJump
ssh -AJ jumServer targetServer
```

### Basic HTTP Server
```js name=httpServer
var parseRequest = (request, buffer) => (request.$parsed = require('url').parse(request.url, true), request.params = Object.assign({}, request.$parsed.query), request.pathname = request.$parsed.pathname, request.body = buffer, request);
var writeResponse = (ctx, request, response) => ctx === null ? null : (response.writeHead(ctx.status || 404, ctx.headers || {}), response.write(ctx.body || ''), response.end());
var createServer = (handler) => require('http').createServer((req, res)=>{
  let buffer = [];
  req.on('data', chunk => (chunk ? buffer.push(chunk) : null));
  req.on('end', async ()  => (parseRequest(req, buffer), writeResponse(await handler(req, res),  req, res))); 
});
var handler = (req, res) => ({body: `hellow buddy ${req.buffer}`});
var server = createServer((req, res) => handler(req, res));
server.listen(8080);
```

### Simple HTTP Server 
```js name=simpleHttp
```

### Simple HTTP Handler
```js name=handleRoute
var routes = {
  'GET /': 'serve'
};
var handler = (req, res) => {
  let resolved = routes[`${req.method} ${req.pathname}`];
  if (resolved && global[resolved]) return global[resolved](req, res);  
  return { body: `not-found`}
}
```

### Capture Code Blocks

```js name=getCodeBlocks
var getCodeBlocks = (markdown) =>  Array.from(markdown.matchAll(/\`\`\`(\w+)((?:\s+\w+=[\w./-]+)*)\s*([\s\S]*?)\`\`\`/g), match => {
  return Object.assign({ lang: match[1], content: match[3].trim()}, match[2].trim().split(/\s+/).reduce((acc, attr)=>{
    let [key, value] = attr.split('=');
    return (key && value) ? (acc[key] = value, acc) : acc;
  }, {}));
});
```

### Eval markdown

```js name=evalMarkdown
var captureCodeBlocks = (markdown) => {
  let codeBlockRegex = /```(\w+)((?:\s+\w+=[\w./-]+)*)\s*([\s\S]*?)```/g;
  let matches = markdown.matchAll(codeBlockRegex);
  return Array.from(matches, match => {
    let attr = match[2].trim();
    let params = attr.split(/\s+/).reduce((acc, attr)=>{
      let [key, value] = attr.split('=');
      return (key && value) ? (acc[key] = value, acc) : acc;
    }, {});    
    return { lang: match[1] , params, content: match[3].trim() };
  });
}
var readFile = (file) => require('fs').existsSync(file) ? require('fs').readFileSync(file, 'utf8') : null;
var evaluate=(res)=> require('vm').runInContext(res, Object.assign(require('vm').createContext(global), {console, require, module}));
var deps=(url) => fetch(url).then(res => res.text()).then(evaluate);
var state = state || {};
var isRunnable = (value) => (value.lang === "js" && value.params.eval === "true" && value.params.runtime==='node');
var isServeable = (value) => (value.params.serve === 'true' && value.params.path );
var concatBlock = (block, fn) => block.reduce((acc, value)=>(fn(value) ? acc.concat(`\n${value.content}`) : acc), '');
var groupBlock = (block, fn) => block.reduce((acc, value)=>{  
  if(fn(value)){
    let path = value.params.path;
    if(!acc[path]) acc[path] = `\n${value.content}`;
    if(acc[path]) acc[path].concat(`\n${value.content}`);
    return acc;
  }
  return acc;
}, {});
var reload = () =>{
  state.block = captureCodeBlocks(readFile(`${process.cwd()}/readme.md`));
  state.codes = concatBlock(state.block, isRunnable);    
  state.files = groupBlock(state.block, isServeable);
  evaluate(state.codes);
}
reload();
```

### runtime index
```js name=evaluateRuntime
var evaluate = (res)=> require('vm').runInContext(res, Object.assign(require('vm').createContext(global), {console, require, module, setTimeout, setInterval }));
var captureCodeBlocks = (markdown) =>  Array.from(markdown.matchAll(/\`\`\`(\w+)((?:\s+\w+=[\w./-]+)*)\s*([\s\S]*?)\`\`\`/g), match => {
  return Object.assign({ lang: match[1], content: match[3].trim()}, match[2].trim().split(/\s+/).reduce((acc, attr)=>{
    let [key, value] = attr.split('=');
    return (key && value) ? (acc[key] = value, acc) : acc;
  }, {}));
});
var readFile = (file) => require('fs').existsSync(file) ? require('fs').readFileSync(file, 'utf8') : null;
var runtime = (path) => evaluate(captureCodeBlocks(readFile(path)).filter(i=> i.runtime && i.runtime === 'node').map(i=> i.content).join('\n\n'));
runtime('./index.md');
```

### Patch require
```js name=requirePatch
const Module = require('module');
const originalRequire = Module.prototype.require;
Module.prototype.require = function patchedRequire(modulePath) {
  delete require.cache[require.resolve(modulePath)];
  return originalRequire.call(this, modulePath);
};

```

### fetch
```js name=loadRepl
var loadRepl = url => fetch(url).then(res => res.text()).then(res => (eval(res), setTimeout(()=>loadRepl(url), 200)));
```

### html string
```js name=htmlFn
var html = (strings, ...values) => String.raw({ raw: strings }, ...values);
```

### simple http server 
```js name=simpeTcpHttp

var net = require('net');
var $http1 = 'HTTP/1.1'
var $endLine = '\r\n';
var $internalServerError = [500, {}, 'Internal Server Error'];
var $toKeyVal = ([key,  value]) => `${key}: ${value}`;
var $lineHeaders = (headers, line) => {
  let [key, value] = line.split(': ');
  return { ...headers, [key.toLowerCase()]: value };
}
var parseHeaders = lines => lines.filter(line => line.includes(': ')).reduce($lineHeaders, {});
var parseRequest = (data) => {
  let request = data.toString();
  let [firstLine, ...lines] = request.split($endLine);
  let [method, path, protocol] = firstLine.split(' ');
  let lastLine = lines.indexOf('');
  let headers = parseHeaders(lines.slice(0, lastLine));
  let body = lines.slice(lastLine + 1).join($endLine);
  let parsed = new URL(`http://${headers.host}${path}`);  
  let urlObject = {
    path: parsed.pathname,
    query: parsed.search,
    params: Object.fromEntries(new URLSearchParams(parsed.search))
  };
  return {method, path, protocol, headers, body, ...urlObject };
};
var statusText = (status) => ({
  200: 'OK',
  404: 'Not Found',
  500: 'Internal Server Error'
}[status]);
var createResponse = ( status, headers ={}, body='') =>{
  let response = {
    'Content-Length': Buffer.from(body).length,
    'Content-Type': 'text/plain',
    'Date': new Date().toUTCString(),
    'Connection': 'close',
    ...headers    
  };
  let responseHeaders = Object.entries(response).map($toKeyVal).join($endLine);
  return [
    `${$http1} ${status} ${statusText(status)}`,
    responseHeaders,
    '',
    body
  ].join($endLine)  
};
var writeSocketData = (socket, handler) => async (data) => {
  try{
    let request = parseRequest(data);
    let {status, headers, body} = await handler(request);
    let response = createResponse(status, headers, body);
    socket.write(response);
  }catch(err){
    console.log(err);
    socket.write(createResponse(...$internalServerError));    
  };
  socket.end();
};
var handleConnection = handler => (socket) => socket.on('data', writeSocketData(socket, handler)).on('error', _ => socket.end());
var createServer = (port, handler) => {
  let server = net.createServer(handleConnection(handler));
  server.listen(port);
  return server;
}

```

### http serve file dir 
```js name=httpServe 

var parseRequest = (request, buffer) => (request.$parsed = require('url').parse(request.url, true), request.params = Object.assign({}, request.$parsed.query), request.pathname = request.$parsed.pathname, request.body = buffer, request);
var writeResponse = (ctx, request, response) => ctx === null ? null : (response.writeHead(ctx.status || 404, ctx.headers || {}), response.write(ctx.body || ''), response.end());
var createServer = (handler) => require('http').createServer((req, res)=>{
  let buffer = [];
  req.on('data', chunk => (chunk ? buffer.push(chunk) : null));
  req.on('end', async ()  => (parseRequest(req, buffer), writeResponse(await handler(req, res),  req, res))); 
});
var fs = require('fs');
var serveIndex = (pathname) => fs.existsSync(`.${pathname}`) && fs.statSync(`.${pathname}`).isDirectory() && fs.existsSync(`.${pathname}/index.html`) && fs.readFileSync(`.${pathname}/index.html`).toString();
var serveFile = (pathname) => {
  let type = { 'html': 'text/html',  'css': 'text/css',  'js': 'application/javascript', 'png': 'image/png',  'jpg': 'image/jpeg',  'svg': 'image/svg+xml' }  ;
  let ext = require('path').extname(pathname).slice(1);
  let contentType = type[ext] || 'plain/text';
  let content = fs.existsSync(`.${pathname}`) && fs.readFileSync(`.${pathname}`);
  if(content) return [content.toString(), contentType];
  return [null, null];
};
var handler = (req, res) =>{
  let index = serveIndex(req.pathname);
  if(index) return { status : 200, headers: {'Content-Type': 'text/html'}, body: index };
  let [file, type] = serveFile(req.pathname);
  if(file) return { status: 200, headers: {'Content-Type': type }, body: file};
  return {status: 404, headers: {'Content-Type': 'text/plain'}, body: '404'}
};
var server = createServer((req, res) => handler(req, res));
server.listen(8080)

```

### nginx header conf 
```conf name=nginxHeader 
add_header                Cache-Control  "public, must-revalidate, proxy-revalidate, max-age=0";
proxy_set_header          X-Forwarded-For $proxy_add_x_forwarded_for;
proxy_set_header          X-NginX-Proxy true;
proxy_set_header          X-Real-IP $remote_addr;
proxy_set_header          X-Forwarded-Proto https;
proxy_hide_header         X-Frame-Options;
proxy_set_header          Accept-Encoding "";
proxy_http_version        1.1;
proxy_set_header          Upgrade $http_upgrade;
proxy_set_header          Connection "upgrade";
proxy_set_header          Host $host;
proxy_cache_bypass        $http_upgrade;
proxy_max_temp_file_size  0;
proxy_redirect            off;
proxy_read_timeout        240s;
```

### nginx default domain 
```conf name=nginxDefault
server {
    listen 80 default_server;
    listen [::]:80 default_server;
    server_name ~^(?<subdomain>.+)\.azizzaeny\.com$ azizzaeny.com;
    rewrite ^ https://$server_name$request_uri? permanent;        
    root /var/www/www;
    location / {
         return 301 https://$host$request_uri;        
    }
}
server {
    listen 443 ssl;
    listen [::]:443 ssl;
    server_name www.azizzaeny.com azizzaeny.com;
    location / {
    index index.html;
        root /var/www/landing;
        #include /etc/nginx/header.conf;
        #proxy_pass http://0.0.0.0:10010/;
    }
}
```
### nginx subdomain 
```conf 
server {
    listen 443 ssl;
    listen [::]:443 ssl;
    server_name draft.azizzaeny.com; 
    location /{
        include /etc/nginx/header.conf;
        proxy_pass http://0.0.0.0:8080;
    }
}
```

### certbot generate ssl 
```sh name=certbot
certCmd="certbot certonly --manual -d *.azizzaeny.com -d azizzaeny.com --agree-tos  --preferred-challenges dns-01 --register-unsafely-without-email"
/usr/bin/docker run --rm -it --name certbot --network host --entrypoint "" -v "/home/aziz/sandbox/deployement/credentials:/etc/letsencrypt/:rw" -w /etc/letsencrypt certbot/certbot:v2.0.0 /bin/sh -c "$certCmd"
```

### nginx start 
```sh name=nginxStart
docker run -d --rm --name gateway --network host -v $(pwd)/credentials:/etc/letsencrypt  -v $(pwd)/conf/header.conf:/etc/nginx/header.conf -v $(pwd)/conf/subdomain/:/etc/nginx/conf.d -v $(pwd)/public:/var/www:rw nginx:latest
```

### nginx reload 
```sh name=nginxReload
docker exec gateway bin/sh -c "nginx -t && nginx -s reload"
```

### node cat 
```sh name=nodeCat 
node -e "$(cat <<'EOF'
console.log('Server running at http://localhost:8080/');
EOF
)"
```
### browser repl 
```sh name=browserRepl
docker run --name local-first --rm -it -w /work --network host node:20-alpine /bin/sh
cat > /work/tmp.js <<'EOF'
var os = require('os');
var repl = require('repl');
var vm = require('vm');
var hosts = `https://draft.azizzaeny.com` || `http://localhost:8080`;
var parseRequest = (request, buffer) => (request.$parsed = require('url').parse(request.url, true), request.params = Object.assign({}, request.$parsed.query), request.pathname = request.$parsed.pathname, request.body = buffer, request);
var writeResponse = (ctx, request, response) => ctx === null ? null : (response.writeHead(ctx.status || 404, ctx.headers || {}), response.write(ctx.body || ''), response.end());
var createServer = (port, handler) => {
  let server = require('http').createServer((req, res)=>{
    let buffer = [];
    req.on('data', chunk => (chunk ? buffer.push(chunk) : null));
    req.on('end', async ()  => (parseRequest(req, buffer), writeResponse(await handler(req, res),  req, res))); 
  });
  return (server.listen(port), server);
}
var cors = (origin='*', method='GET, POST, PUT, DELETE, OPTIONS', headers='Content-Type, Authorization') => ({
  'Access-Control-Allow-Origin': origin,
  'Access-Control-Allow-Methods': method,
  'Access-Control-Allow-Headers': headers
});
var bufferContent = bufferContent || [];
var bufferRelease = (txt) => {
  bufferContent.forEach((res) => (res.writeHead(200, cors()), res.end(txt)));
  bufferContent = [];
  return true;
}
var defaultHtml = `
 <html>
   <head>
     <meta charset='UTF-8'/>
     <meta name='viewport' content='width=device-width,initial-scale=1.0'>    
     <script src='https://cdn.tailwindcss.com/3.4.3'></script>
     <script src='${hosts}/client.js'></script>
   </head>
   <body class='antialiased'>
   </body>
 </html>
`;
var defaultClient =`
  var createScript = (id, content) => {
    let _script = () => Object.assign(document.createElement('script'), {id: id ? id : generateId(), innerHTML:content});
    let findScript = document.querySelector('script[id='+id+']');
    if(!findScript) return document.head.appendChild(_script());
    return (findScript.parentNode.removeChild(findScript), document.head.appendChild(_script()))
  }
  var loadRepl = url => fetch(url, {mode: 'cors'}).then(res => res.text()).then(res => (createScript('_repl', res), setTimeout(()=>loadRepl(url), 200)));
  loadRepl('${hosts}/_repl');
  console.log('Browser Repl: connected');
`;
var typeHtml = {'Content-Type': 'text/html'};
var typeJs = {'Content-Type': 'text/javascript'};
var defaultHeaders = (type) => Object.assign(cors(), type);
var serveIndex = (req) => ({ status: 200, headers: defaultHeaders(typeHtml), body: defaultHtml});
var serveClient = (req) => ({ status: 200, headers: defaultHeaders(typeJs), body: defaultClient});
var handler = (req, res) => {
  let routes = {
    'GET /': serveIndex,
    'GET /client.js': serveClient
  };
  let found = routes[`${req.method} ${req.pathname}`];
  if(found) return found(req);
  return (bufferContent.push(res), null);
};
var bufferCmds = bufferCmds = [];
var bufferTimer = null;
var browserEval = (cmd, context, filename, callback) => {
  (cmd = cmd.replace(/;+$/, ''));
  if (cmd.trim()) {
    try {
      new vm.Script(cmd);
      bufferCmds.push(cmd);
      if(bufferTimer) clearTimeout(bufferTimer);
      bufferTimer = setTimeout( _ => (bufferRelease(bufferCmds.join(' \n ')), bufferCmds = []), 200);
      callback(null);
    } catch (e) {
      if (e instanceof SyntaxError) {
        callback(new repl.Recoverable(e));
      } else {
        bufferCmds.push(cmd);
        if(bufferTimer) clearTimeout(bufferTimer); 
        bufferTimer = setTimeout( _ => (bufferRelease(bufferCmds.join(' \n ')), bufferCmds=[]), 200);
        callback(e);
      }
    }
  } else {
    callback(null);
  }
}
var server = server || createServer(8080, (req, res)=> handler(req, res));
console.log(`Browser Repl created at port ${process.env.PORT}, REPL started`);
var replServer = replServer || repl.start({
  prompt: '> ',
  eval: browserEval,
  ignoreUndefined: true
});
EOF
node /work/tmp.js
```
