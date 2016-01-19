// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_array = require("./caml_array");

function curry(_f, _args) {
  while(/* true */1) {
    var args = _args;
    var f = _f;
    var arity = f.length;
    var len = args.length;
    var d = arity - len;
    if (d) {
      if (d < 0) {
        _args = Caml_array.caml_array_sub(args, arity, -d);
        _f = f.apply(null, Caml_array.caml_array_sub(args, 0, arity));
      }
      else {
        return (function(f,args){
        return function (x) {
          return curry(f, args.concat(/* array */[x]));
        }
        }(f,args));
      }
    }
    else {
      return f.apply(null, args);
    }
  };
}

function curry1(o, x, arity) {
  if (7 < (arity >>> 0)) {
    return function (a) {
      return curry(o, /* array */[
                  x,
                  a
                ]);
    };
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return o(x);
      case 2 : 
          return function (param) {
            return o(x, param);
          };
      case 3 : 
          return function (param, param$1) {
            return o(x, param, param$1);
          };
      case 4 : 
          return function (param, param$1, param$2) {
            return o(x, param, param$1, param$2);
          };
      case 5 : 
          return function (param, param$1, param$2, param$3) {
            return o(x, param, param$1, param$2, param$3);
          };
      case 6 : 
          return function (param, param$1, param$2, param$3, param$4) {
            return o(x, param, param$1, param$2, param$3, param$4);
          };
      case 7 : 
          return function (param, param$1, param$2, param$3, param$4, param$5) {
            return o(x, param, param$1, param$2, param$3, param$4, param$5);
          };
      
    }
  }
}

function app1(o, x) {
  var len = o.length;
  if (len === 1 || len === 0) {
    return o(x);
  }
  else {
    return curry1(o, x, len);
  }
}

function app2(o, x, y) {
  var len = o.length;
  if (len === 2) {
    return o(x, y);
  }
  else {
    return curry(o, /* array */[
                x,
                y
              ]);
  }
}

function app3(o, a0, a1, a2) {
  var len = o.length;
  if (len === 3) {
    return o(a0, a1, a2);
  }
  else {
    return curry(o, /* array */[
                a0,
                a1,
                a2
              ]);
  }
}

function app4(o, a0, a1, a2, a3) {
  var len = o.length;
  if (len === 4) {
    return o(a0, a1, a2)(a3);
  }
  else {
    return curry(o, /* array */[
                a0,
                a1,
                a2,
                a3
              ]);
  }
}

function app5(o, a0, a1, a2, a3, a4) {
  var len = o.length;
  if (len === 4) {
    return o(a0, a1, a2)(a3, a4);
  }
  else {
    return curry(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4
              ]);
  }
}

exports.curry = curry;
exports.curry1 = curry1;
exports.app1 = app1;
exports.app2 = app2;
exports.app3 = app3;
exports.app4 = app4;
exports.app5 = app5;
/* No side effect */
