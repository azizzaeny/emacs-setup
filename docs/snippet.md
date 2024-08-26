### tailwind
```html name=tailwind
<script src=\"https://cdn.tailwindcss.com/3.4.3\"></script>
```

### view port
```html name=viewport
<meta name="viewport" content="width=device-width,initial-scale=1.0">
```

### html tailwind initial
```js name=html
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
```js name=watch
var watch = (file, callback) => require('fs').watchFile(
  file,
  {persistent:true, interval:200 },
  (prev, cur)=> callback()
);
```

### basic http
```js name=httpBasic
var createServer, startServer, findFile
var server = server || null;
var main = () => {
  server = createServer({ port: 8081, handler: (req, res) => handler(req, res)})
  startServer(server); console.log('started server', 8081);  
}
var routes = {
  'GET /': 'mainHandler'
};
var mainHandler = (req, res) => findFile(`${process.env.CONTEXT}/index.html`);
var handler = (req, res) => {
  let resolve = routes[`${req.method} ${req.pathname}`];
  if(resolve && global[resolve]) return global[resolve](req, res);
  return response('404');
}
```

### node eval
```sh name=nodeEval
node -e "var evaluate=(...args)=>{ let [vm=require('vm'), ctx=global] = args;  return (res) => vm.runInContext(res, Object.assign(vm.createContext(ctx), {console, require, module}));}; var deps=(url) => fetch(url).then(res => res.text()).then(evaluate()); process.env.CONTEXT='$1'; evaluate()(require('fs').readFileSync('./$1/index.js'));" -i
```

### evaluate deps
```js name=evaluate
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

```sh name=dockerNode
docker run --name "$container_name" --rm -it -w /work --network host -v $(pwd):/work node:20-alpine /bin/sh -c "node && /bin/sh"
```

### Simple HTTP Server

```js name=httpServer

var parseRequest = (request, buffer) => (request.$parsed = require('url').parse(request.url, true), request.params = Object.assign({}, request.$parsed.query), request.pathname = request.$parsed.pathname, request);
var responseWrite = (ctx, request, response) => (
  response.writeHead(ctx.status || 404, ctx.headers || {}), response.write(ctx.body || ''), response.end()
);
var processRequest = (ctx) => (request, response) => {
  let buffer = [];
  request.on('data', chunk => (chunk ? buffer.push(chunk) : null));
  request.on('end', _  => setTimeout(async ()=>  responseWrite( await ctx.handler( parseRequest(request, buffer), response),request,response), 0)) 
};
var httpServer = (ctx) => require('http').createServer(processRequest(ctx)).listen(ctx.port);

```
### Simple Handler

```js name=handler
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

```js name=captureCode
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

```

### Read file
```js name=readFile
var readFile = (file) => require('fs').existsSync(file) ? require('fs').readFileSync(file, 'utf8') : null;
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
