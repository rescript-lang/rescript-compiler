// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function add(x, y) {
  return x + y;
}

function add2(x, y) {
  return x - y;
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      Caml_curry.app1(f, param[1]);
      _param = param[2];
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

exports.add  = add;
exports.add2 = add2;
exports.iter = iter;
/* No side effect */
