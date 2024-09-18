var createScript = (id, content) => {
  let _script = () => Object.assign(document.createElement('script'), {id: id ? id : generateId(), innerHTML:content});
  let findScript = document.querySelector(`script[id="${id}"]`);
  if(!findScript) return document.head.appendChild(_script());
  return (findScript.parentNode.removeChild(findScript), document.head.appendChild(_script()))
}
var loadRepl = url => fetch(url, {mode: 'cors'}).then(res => res.text()).then(res => (createScript('foo', res), setTimeout(()=>loadRepl(url), 200)));
loadRepl('http://localhost:5050/_repl');
console.log('Browser Repl: connected');
