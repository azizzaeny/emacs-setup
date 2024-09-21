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
var mainHandler = (req, res) => {
  if(req.pathname === '/client.js') return { status: 200, body: readFile('./browser-client.js'), headers: {'Content-Type': 'text/javascript'}};
  return (bufferContent.push(res), null);
}
var server = httpServer({ port: 5050, handler: (req, res)=> mainHandler(req, res)});
