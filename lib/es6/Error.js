


let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error("Panic! " + msg);
}

export {
  $$EvalError,
  $$RangeError,
  $$ReferenceError,
  $$SyntaxError,
  $$TypeError,
  $$URIError,
  panic,
}
/* No side effect */
