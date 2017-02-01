'use strict';


function bind(x, f) {
  if (x !== null) {
    return f(x);
  }
  else {
    return null;
  }
}

function iter(x, f) {
  if (x !== null) {
    return f(x);
  }
  else {
    return /* () */0;
  }
}

function from_opt(x) {
  if (x) {
    return x[0];
  }
  else {
    return null;
  }
}

export{
  bind     ,
  iter     ,
  from_opt ,
  
}
/* No side effect */
