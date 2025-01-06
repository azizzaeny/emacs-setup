// ;; @azizzaeny: browser-repl
var os = require('os');
var repl = require('repl');
var vm = require('vm');

var parseRequest = (request, buffer) => (request.$parsed = require('url').parse(request.url, true), request.params = Object.assign({}, request.$parsed.query), request.pathname = request.$parsed.pathname, request);

var responseWrite = (ctx, request, response) => {
  if(!ctx) return null;
  return (response.writeHead(ctx.status || 404, ctx.headers || {}), response.write(ctx.body || ''), response.end())
}
var processRequest = (ctx) => (request, response) => {
  let buffer = [];
  request.on('data', chunk => (chunk ? buffer.push(chunk) : null));
  request.on('end', _  => setTimeout(async ()=>  responseWrite( await ctx.handler( parseRequest(request, buffer), response),request,response), 0)) 
};

var httpServer = (ctx) => require('http').createServer(processRequest(ctx)).listen(ctx.port);

var readFile = (file) => require('fs').existsSync(file) ? require('fs').readFileSync(file, 'utf8') : null;

var cors = (origin="*", method='GET, POST, PUT, DELETE, OPTIONS', headers='Content-Type, Authorization') => ({
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
    <meta charset="UTF-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0">    
    <script src="https://cdn.tailwindcss.com/3.4.3"></script>
    <script src="http://localhost:5050/client.js"></script>
  </head>
  <body class="antialiased">
  </body>
</html>
`;

var browserClient =`
var createScript = (id, content) => {
  let _script = () => Object.assign(document.createElement('script'), {id: id ? id : generateId(), innerHTML:content});
  let findScript = document.querySelector("script[id='"+id+"']");
  if(!findScript) return document.head.appendChild(_script());
  return (findScript.parentNode.removeChild(findScript), document.head.appendChild(_script()))
}
var loadRepl = url => fetch(url, {mode: 'cors'}).then(res => res.text()).then(res => (createScript('_repl', res), setTimeout(()=>loadRepl(url), 200)));
loadRepl('http://localhost:5050/_repl');
console.log('Browser Repl: connected');
`;

var defaultHeaders = (type={'Content-Type': 'text/javascript'}) => Object.assign(cors(), type);

var mainHandler = (req, res) => {
  if(req.pathname === '/client.js') return { status: 200, body: browserClient, headers: defaultHeaders() };
  if(req.pathname === "/") return { status: 200, body: defaultHtml, headers: defaultHeaders({'Content-Type': 'text/html'}) };
  return (bufferContent.push(res), null);
}
// todo: instead of jsut releasing it, we want to set timer and clear timer

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


var server = server || httpServer({ port: parseInt(process.env.PORT) || 5050 , handler: (req, res)=> mainHandler(req, res)});

var replServer = replServer || repl.start({
  prompt: '> ',
  eval: browserEval,
  ignoreUndefined: true
});

console.log(`Browser Repl created at port ${process.env.PORT}, REPL started`);

// simple tmux systems
// tmux new-session -d -s browser-repl 'PORT=5050 node ~/.emacs.d/docs/browser-repl.js'


