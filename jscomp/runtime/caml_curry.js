// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_oo    = require("./caml_oo");
var Caml_array = require("./caml_array");

function app(_f, _args) {
  while(true) {
    var args = _args;
    var f = _f;
    var arity = f.length;
    var len = args.length;
    var d = arity - len | 0;
    if (d) {
      if (d < 0) {
        _args = Caml_array.caml_array_sub(args, arity, -d);
        _f = f.apply(null, Caml_array.caml_array_sub(args, 0, arity));
        continue ;
        
      }
      else {
        return (function(f,args){
        return function (x) {
          return app(f, args.concat(/* array */[x]));
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
  if (arity > 7 || arity < 0) {
    return function (a) {
      return app(o, /* array */[
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
    return app(o, /* array */[
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
    return app(o, /* array */[
                a0,
                a1,
                a2
              ]);
  }
}

function app4(o, a0, a1, a2, a3) {
  var len = o.length;
  if (len === 4) {
    return o(a0, a1, a2, a3);
  }
  else {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3
              ]);
  }
}

function app5(o, a0, a1, a2, a3, a4) {
  var len = o.length;
  if (len === 5) {
    return o(a0, a1, a2, a3, a4);
  }
  else {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4
              ]);
  }
}

function app6(o, a0, a1, a2, a3, a4, a5) {
  var len = o.length;
  if (len === 6) {
    return o(a0, a1, a2, a3, a4, a5);
  }
  else {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4,
                a5
              ]);
  }
}

function app7(o, a0, a1, a2, a3, a4, a5, a6) {
  var len = o.length;
  if (len === 7) {
    return o(a0, a1, a2, a3, a4, a5, a6);
  }
  else {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4,
                a5,
                a6
              ]);
  }
}

function app8(o, a0, a1, a2, a3, a4, a5, a6, a7) {
  var len = o.length;
  if (len === 8) {
    return o(a0, a1, a2, a3, a4, a5, a6, a7);
  }
  else {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7
              ]);
  }
}

function js(label, cacheid, obj, args) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app(meth, args);
}

function js1(label, cacheid, obj) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app1(meth, obj);
}

function js2(label, cacheid, obj, a1) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app2(meth, obj, a1);
}

function js3(label, cacheid, obj, a1, a2) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app3(meth, obj, a1, a2);
}

function js4(label, cacheid, obj, a1, a2, a3) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app4(meth, obj, a1, a2, a3);
}

function js5(label, cacheid, obj, a1, a2, a3, a4) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app5(meth, obj, a1, a2, a3, a4);
}

function js6(label, cacheid, obj, a1, a2, a3, a4, a5) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app6(meth, obj, a1, a2, a3, a4, a5);
}

function js7(label, cacheid, obj, a1, a2, a3, a4, a5, a6) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app7(meth, obj, a1, a2, a3, a4, a5, a6);
}

function js8(label, cacheid, obj, a1, a2, a3, a4, a5, a6, a7) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app8(meth, obj, a1, a2, a3, a4, a5, a6, a7);
}

exports.app    = app;
exports.curry1 = curry1;
exports.app1   = app1;
exports.app2   = app2;
exports.app3   = app3;
exports.app4   = app4;
exports.app5   = app5;
exports.app6   = app6;
exports.app7   = app7;
exports.app8   = app8;
exports.js     = js;
exports.js1    = js1;
exports.js2    = js2;
exports.js3    = js3;
exports.js4    = js4;
exports.js5    = js5;
exports.js6    = js6;
exports.js7    = js7;
exports.js8    = js8;
/* No side effect */
