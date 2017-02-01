'use strict';


function some(x) {
  return /* Some */[x];
}

function is_none(x) {
  if (x) {
    return false;
  }
  else {
    return true;
  }
}

function to_def(x) {
  if (x) {
    return x[0];
  }
  else {
    return undefined;
  }
}

function cons(x, y) {
  return /* :: */[
          x,
          y
        ];
}

function is_list_empty(x) {
  if (x) {
    return false;
  }
  else {
    return true;
  }
}

var none = /* None */0;

export {
  none          ,
  some          ,
  is_none       ,
  to_def        ,
  cons          ,
  is_list_empty ,
  
}
/* No side effect */
