'use strict';

var Caml_obj = require("./caml_obj.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function init_mod(loc, shape) {
  var undef_module = function (param) {
    throw [
          Caml_builtin_exceptions.undefined_recursive_module,
          loc
        ];
  };
  var loop = function (shape, struct_, idx) {
    if (typeof shape === "number") {
      switch (shape) {
        case 0 : 
        case 1 : 
            struct_[idx] = undef_module;
            return /* () */0;
        case 2 : 
            struct_[idx] = /* tuple */[
              undef_module,
              undef_module,
              undef_module,
              0
            ];
            return /* () */0;
        
      }
    } else if (shape.tag) {
      struct_[idx] = shape[0];
      return /* () */0;
    } else {
      var comps = shape[0];
      var v = /* array */[];
      struct_[idx] = v;
      var len = comps.length;
      for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
        loop(comps[i], v, i);
      }
      return /* () */0;
    }
  };
  var res = /* array */[];
  loop(shape, res, 0);
  return res[0];
}

function update_mod(shape, o, n) {
  var aux = function (shape, o, n, parent, i) {
    if (typeof shape === "number") {
      switch (shape) {
        case 0 : 
            parent[i] = n;
            return /* () */0;
        case 1 : 
        case 2 : 
            return Caml_obj.caml_update_dummy(o, n);
        
      }
    } else if (shape.tag) {
      return /* () */0;
    } else {
      var comps = shape[0];
      for(var i$1 = 0 ,i_finish = comps.length - 1 | 0; i$1 <= i_finish; ++i$1){
        aux(comps[i$1], o[i$1], n[i$1], o, i$1);
      }
      return /* () */0;
    }
  };
  if (typeof shape === "number") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "caml_module.ml",
            95,
            10
          ]
        ];
  } else if (shape.tag) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "caml_module.ml",
            95,
            10
          ]
        ];
  } else {
    var comps = shape[0];
    for(var i = 0 ,i_finish = comps.length - 1 | 0; i <= i_finish; ++i){
      aux(comps[i], o[i], n[i], o, i);
    }
    return /* () */0;
  }
}

exports.init_mod = init_mod;
exports.update_mod = update_mod;
/* No side effect */
