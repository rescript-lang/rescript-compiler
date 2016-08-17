'use strict';

var Caml_oo    = require("./caml_oo");
var Caml_array = require("./caml_array");

function app(_f, _args) {
  while(true) {
    var args = _args;
    var f = _f;
    var arity = f.length;
    var arity$1 = arity ? arity : 1;
    var len = args.length;
    var d = arity$1 - len | 0;
    if (d) {
      if (d < 0) {
        _args = Caml_array.caml_array_sub(args, arity$1, -d);
        _f = f.apply(null, Caml_array.caml_array_sub(args, 0, arity$1));
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

function js(label, cacheid, obj, args) {
  var meth = Caml_oo.caml_get_public_method(obj, label, cacheid);
  return app(meth, args);
}

function curry_1(o, a0, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[a0]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return o(a0);
      case 2 : 
          return function (param) {
            return o(a0, param);
          };
      case 3 : 
          return function (param, param$1) {
            return o(a0, param, param$1);
          };
      case 4 : 
          return function (param, param$1, param$2) {
            return o(a0, param, param$1, param$2);
          };
      case 5 : 
          return function (param, param$1, param$2, param$3) {
            return o(a0, param, param$1, param$2, param$3);
          };
      case 6 : 
          return function (param, param$1, param$2, param$3, param$4) {
            return o(a0, param, param$1, param$2, param$3, param$4);
          };
      case 7 : 
          return function (param, param$1, param$2, param$3, param$4, param$5) {
            return o(a0, param, param$1, param$2, param$3, param$4, param$5);
          };
      
    }
  }
}

function _1(o, a0) {
  var arity = o.length;
  if (arity === 1) {
    return o(a0);
  }
  else {
    return curry_1(o, a0, arity);
  }
}

function js1(label, cacheid, a0) {
  return _1(Caml_oo.caml_get_public_method(a0, label, cacheid), a0);
}

function __1(o) {
  var arity = o.length;
  if (arity === 1) {
    return o;
  }
  else {
    return function (a0) {
      return _1(o, a0);
    };
  }
}

function curry_2(o, a0, a1, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1
              ]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[a1]);
      case 2 : 
          return o(a0, a1);
      case 3 : 
          return function (param) {
            return o(a0, a1, param);
          };
      case 4 : 
          return function (param, param$1) {
            return o(a0, a1, param, param$1);
          };
      case 5 : 
          return function (param, param$1, param$2) {
            return o(a0, a1, param, param$1, param$2);
          };
      case 6 : 
          return function (param, param$1, param$2, param$3) {
            return o(a0, a1, param, param$1, param$2, param$3);
          };
      case 7 : 
          return function (param, param$1, param$2, param$3, param$4) {
            return o(a0, a1, param, param$1, param$2, param$3, param$4);
          };
      
    }
  }
}

function _2(o, a0, a1) {
  var arity = o.length;
  if (arity === 2) {
    return o(a0, a1);
  }
  else {
    return curry_2(o, a0, a1, arity);
  }
}

function js2(label, cacheid, a0, a1) {
  return _2(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1);
}

function __2(o) {
  var arity = o.length;
  if (arity === 2) {
    return o;
  }
  else {
    return function (a0, a1) {
      return _2(o, a0, a1);
    };
  }
}

function curry_3(o, a0, a1, a2, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1,
                a2
              ]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[a2]);
      case 3 : 
          return o(a0, a1, a2);
      case 4 : 
          return function (param) {
            return o(a0, a1, a2, param);
          };
      case 5 : 
          return function (param, param$1) {
            return o(a0, a1, a2, param, param$1);
          };
      case 6 : 
          return function (param, param$1, param$2) {
            return o(a0, a1, a2, param, param$1, param$2);
          };
      case 7 : 
          return function (param, param$1, param$2, param$3) {
            return o(a0, a1, a2, param, param$1, param$2, param$3);
          };
      
    }
  }
}

function _3(o, a0, a1, a2) {
  var arity = o.length;
  if (arity === 3) {
    return o(a0, a1, a2);
  }
  else {
    return curry_3(o, a0, a1, a2, arity);
  }
}

function js3(label, cacheid, a0, a1, a2) {
  return _3(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2);
}

function __3(o) {
  var arity = o.length;
  if (arity === 3) {
    return o;
  }
  else {
    return function (a0, a1, a2) {
      return _3(o, a0, a1, a2);
    };
  }
}

function curry_4(o, a0, a1, a2, a3, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3
              ]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2,
                      a3
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[
                      a2,
                      a3
                    ]);
      case 3 : 
          return app(o(a0, a1, a2), /* array */[a3]);
      case 4 : 
          return o(a0, a1, a2, a3);
      case 5 : 
          return function (param) {
            return o(a0, a1, a2, a3, param);
          };
      case 6 : 
          return function (param, param$1) {
            return o(a0, a1, a2, a3, param, param$1);
          };
      case 7 : 
          return function (param, param$1, param$2) {
            return o(a0, a1, a2, a3, param, param$1, param$2);
          };
      
    }
  }
}

function _4(o, a0, a1, a2, a3) {
  var arity = o.length;
  if (arity === 4) {
    return o(a0, a1, a2, a3);
  }
  else {
    return curry_4(o, a0, a1, a2, a3, arity);
  }
}

function js4(label, cacheid, a0, a1, a2, a3) {
  return _4(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3);
}

function __4(o) {
  var arity = o.length;
  if (arity === 4) {
    return o;
  }
  else {
    return function (a0, a1, a2, a3) {
      return _4(o, a0, a1, a2, a3);
    };
  }
}

function curry_5(o, a0, a1, a2, a3, a4, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4
              ]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2,
                      a3,
                      a4
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[
                      a2,
                      a3,
                      a4
                    ]);
      case 3 : 
          return app(o(a0, a1, a2), /* array */[
                      a3,
                      a4
                    ]);
      case 4 : 
          return app(o(a0, a1, a2, a3), /* array */[a4]);
      case 5 : 
          return o(a0, a1, a2, a3, a4);
      case 6 : 
          return function (param) {
            return o(a0, a1, a2, a3, a4, param);
          };
      case 7 : 
          return function (param, param$1) {
            return o(a0, a1, a2, a3, a4, param, param$1);
          };
      
    }
  }
}

function _5(o, a0, a1, a2, a3, a4) {
  var arity = o.length;
  if (arity === 5) {
    return o(a0, a1, a2, a3, a4);
  }
  else {
    return curry_5(o, a0, a1, a2, a3, a4, arity);
  }
}

function js5(label, cacheid, a0, a1, a2, a3, a4) {
  return _5(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4);
}

function __5(o) {
  var arity = o.length;
  if (arity === 5) {
    return o;
  }
  else {
    return function (a0, a1, a2, a3, a4) {
      return _5(o, a0, a1, a2, a3, a4);
    };
  }
}

function curry_6(o, a0, a1, a2, a3, a4, a5, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1,
                a2,
                a3,
                a4,
                a5
              ]);
  }
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2,
                      a3,
                      a4,
                      a5
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[
                      a2,
                      a3,
                      a4,
                      a5
                    ]);
      case 3 : 
          return app(o(a0, a1, a2), /* array */[
                      a3,
                      a4,
                      a5
                    ]);
      case 4 : 
          return app(o(a0, a1, a2, a3), /* array */[
                      a4,
                      a5
                    ]);
      case 5 : 
          return app(o(a0, a1, a2, a3, a4), /* array */[a5]);
      case 6 : 
          return o(a0, a1, a2, a3, a4, a5);
      case 7 : 
          return function (param) {
            return o(a0, a1, a2, a3, a4, a5, param);
          };
      
    }
  }
}

function _6(o, a0, a1, a2, a3, a4, a5) {
  var arity = o.length;
  if (arity === 6) {
    return o(a0, a1, a2, a3, a4, a5);
  }
  else {
    return curry_6(o, a0, a1, a2, a3, a4, a5, arity);
  }
}

function js6(label, cacheid, a0, a1, a2, a3, a4, a5) {
  return _6(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5);
}

function __6(o) {
  var arity = o.length;
  if (arity === 6) {
    return o;
  }
  else {
    return function (a0, a1, a2, a3, a4, a5) {
      return _6(o, a0, a1, a2, a3, a4, a5);
    };
  }
}

function curry_7(o, a0, a1, a2, a3, a4, a5, a6, arity) {
  if (arity > 7 || arity < 0) {
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
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2,
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[
                      a2,
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 3 : 
          return app(o(a0, a1, a2), /* array */[
                      a3,
                      a4,
                      a5,
                      a6
                    ]);
      case 4 : 
          return app(o(a0, a1, a2, a3), /* array */[
                      a4,
                      a5,
                      a6
                    ]);
      case 5 : 
          return app(o(a0, a1, a2, a3, a4), /* array */[
                      a5,
                      a6
                    ]);
      case 6 : 
          return app(o(a0, a1, a2, a3, a4, a5), /* array */[a6]);
      case 7 : 
          return o(a0, a1, a2, a3, a4, a5, a6);
      
    }
  }
}

function _7(o, a0, a1, a2, a3, a4, a5, a6) {
  var arity = o.length;
  if (arity === 7) {
    return o(a0, a1, a2, a3, a4, a5, a6);
  }
  else {
    return curry_7(o, a0, a1, a2, a3, a4, a5, a6, arity);
  }
}

function js7(label, cacheid, a0, a1, a2, a3, a4, a5, a6) {
  return _7(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5, a6);
}

function __7(o) {
  var arity = o.length;
  if (arity === 7) {
    return o;
  }
  else {
    return function (a0, a1, a2, a3, a4, a5, a6) {
      return _7(o, a0, a1, a2, a3, a4, a5, a6);
    };
  }
}

function curry_8(o, a0, a1, a2, a3, a4, a5, a6, a7, arity) {
  if (arity > 7 || arity < 0) {
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
  else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[
                      a1,
                      a2,
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 2 : 
          return app(o(a0, a1), /* array */[
                      a2,
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 3 : 
          return app(o(a0, a1, a2), /* array */[
                      a3,
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 4 : 
          return app(o(a0, a1, a2, a3), /* array */[
                      a4,
                      a5,
                      a6,
                      a7
                    ]);
      case 5 : 
          return app(o(a0, a1, a2, a3, a4), /* array */[
                      a5,
                      a6,
                      a7
                    ]);
      case 6 : 
          return app(o(a0, a1, a2, a3, a4, a5), /* array */[
                      a6,
                      a7
                    ]);
      case 7 : 
          return app(o(a0, a1, a2, a3, a4, a5, a6), /* array */[a7]);
      
    }
  }
}

function _8(o, a0, a1, a2, a3, a4, a5, a6, a7) {
  var arity = o.length;
  if (arity === 8) {
    return o(a0, a1, a2, a3, a4, a5, a6, a7);
  }
  else {
    return curry_8(o, a0, a1, a2, a3, a4, a5, a6, a7, arity);
  }
}

function js8(label, cacheid, a0, a1, a2, a3, a4, a5, a6, a7) {
  return _8(Caml_oo.caml_get_public_method(a0, label, cacheid), a0, a1, a2, a3, a4, a5, a6, a7);
}

function __8(o) {
  var arity = o.length;
  if (arity === 8) {
    return o;
  }
  else {
    return function (a0, a1, a2, a3, a4, a5, a6, a7) {
      return _8(o, a0, a1, a2, a3, a4, a5, a6, a7);
    };
  }
}

exports.app     = app;
exports.js      = js;
exports.curry_1 = curry_1;
exports._1      = _1;
exports.js1     = js1;
exports.__1     = __1;
exports.curry_2 = curry_2;
exports._2      = _2;
exports.js2     = js2;
exports.__2     = __2;
exports.curry_3 = curry_3;
exports._3      = _3;
exports.js3     = js3;
exports.__3     = __3;
exports.curry_4 = curry_4;
exports._4      = _4;
exports.js4     = js4;
exports.__4     = __4;
exports.curry_5 = curry_5;
exports._5      = _5;
exports.js5     = js5;
exports.__5     = __5;
exports.curry_6 = curry_6;
exports._6      = _6;
exports.js6     = js6;
exports.__6     = __6;
exports.curry_7 = curry_7;
exports._7      = _7;
exports.js7     = js7;
exports.__7     = __7;
exports.curry_8 = curry_8;
exports._8      = _8;
exports.js8     = js8;
exports.__8     = __8;
/* No side effect */
