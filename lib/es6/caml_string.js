


function get(s, i) {
  if (i >= s.length || i < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "index out of bounds",
          Error: new Error()
        };
  }
  return s.charCodeAt(i);
}

function make(n, ch) {
  return String.fromCharCode(ch).repeat(n);
}

export {
  get ,
  make ,
  
}
/* No side effect */
