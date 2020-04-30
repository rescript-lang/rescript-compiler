


function get(s, i) {
  if (i >= s.length || i < 0) {
    throw {
          ExceptionID: -3,
          _1: "index out of bounds",
          Debug: "Invalid_argument"
        };
  }
  return s.charCodeAt(i);
}

export {
  get ,
  
}
/* No side effect */
