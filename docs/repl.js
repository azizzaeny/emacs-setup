const repl = require('repl');
const vm = require('vm');

// Define the function to be called on each complete evaluation
function foo(txt) {
  console.log('Received complete expression:');
  console.log(txt);
}

// Custom eval function
function myEval(cmd, context, filename, callback) {
  // Remove trailing semicolons, but keep newlines
  cmd = cmd.replace(/;+$/, '');

  if (cmd.trim()) {
    try {
      // Try to compile the command
      new vm.Script(cmd);
      // If compilation succeeds, it's a complete expression
      foo(cmd);
      callback(null);
    } catch (e) {
      if (e instanceof SyntaxError) {
        // If it's a syntax error, it might be an incomplete input
        callback(new repl.Recoverable(e));
      } else {
        // For other errors, call foo() with the input and return the error
        foo(cmd);
        callback(e);
      }
    }
  } else {
    // If the command is empty, just return
    callback(null);
  }
}

// Create a custom REPL server
const replServer = repl.start({
  prompt: '> ',
  eval: myEval,
  ignoreUndefined: true
});

console.log('Custom REPL started. Enter complete expressions, and foo(txt) will be called with the full text.');
console.log('Multi-line input is supported. Press Ctrl+C twice to exit.');
