


function get(s, i) {
  if (i >= s.length || i < 0) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "index out of bounds"
          }
        });
  }
  return s.codePointAt(i);
}

function make(n, ch) {
  return String.fromCharCode(ch).repeat(n);
}

export {
  get,
  make,
}
/* No side effect */
