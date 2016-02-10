// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj_runtime        = require("../runtime/caml_obj_runtime");
var Obj                     = require("./obj");
var CamlinternalOO          = require("./camlinternalOO");
var $$Array                 = require("./array");
var Caml_curry              = require("../runtime/caml_curry");

function init_mod(loc, shape) {
  if (typeof shape === "number") {
    switch (shape) {
      case 0 : 
          return function () {
            throw [
                  0,
                  Caml_builtin_exceptions.Undefined_recursive_module,
                  loc
                ];
          };
      case 1 : 
          return [
                  246,
                  function () {
                    throw [
                          0,
                          Caml_builtin_exceptions.Undefined_recursive_module,
                          loc
                        ];
                  }
                ];
      case 2 : 
          return CamlinternalOO.dummy_class(loc);
      
    }
  }
  else if (shape[0]) {
    return shape[1];
  }
  else {
    return $$Array.map(function (param) {
                return init_mod(loc, param);
              }, shape[1]);
  }
}

function overwrite(o, n) {
  if (o.length < n.length) {
    throw [
          0,
          Caml_builtin_exceptions.Assert_failure,
          [
            0,
            "camlinternalMod.ml",
            40,
            2
          ]
        ];
  }
  for(var i = 0 ,i_finish = n.length - 1; i<= i_finish; ++i){
    o[i] = n[i];
  }
  return /* () */0;
}

function update_mod(shape, o, n) {
  if (typeof shape === "number") {
    switch (shape) {
      case 0 : 
          if (Caml_obj_runtime.caml_obj_tag(n) === Obj.closure_tag && n.length <= o.length) {
            overwrite(o, n);
            return Caml_obj_runtime.caml_obj_truncate(o, n.length);
          }
          else {
            return overwrite(o, function (x) {
                        return Caml_curry.app1(n, x);
                      });
          }
      case 1 : 
          if (Caml_obj_runtime.caml_obj_tag(n) === Obj.lazy_tag) {
            o[0] = n[0];
            return /* () */0;
          }
          else if (Caml_obj_runtime.caml_obj_tag(n) === Obj.forward_tag) {
            Caml_obj_runtime.caml_obj_set_tag(o, Obj.forward_tag);
            o[0] = n[0];
            return /* () */0;
          }
          else {
            Caml_obj_runtime.caml_obj_set_tag(o, Obj.forward_tag);
            o[0] = n;
            return /* () */0;
          }
      case 2 : 
          if (!(Caml_obj_runtime.caml_obj_tag(n) === 0 && n.length === 4)) {
            throw [
                  0,
                  Caml_builtin_exceptions.Assert_failure,
                  [
                    0,
                    "camlinternalMod.ml",
                    63,
                    6
                  ]
                ];
          }
          return overwrite(o, n);
      
    }
  }
  else if (shape[0]) {
    return /* () */0;
  }
  else {
    var comps = shape[1];
    if (!(Caml_obj_runtime.caml_obj_tag(n) === 0 && n.length >= comps.length)) {
      throw [
            0,
            Caml_builtin_exceptions.Assert_failure,
            [
              0,
              "camlinternalMod.ml",
              66,
              6
            ]
          ];
    }
    for(var i = 0 ,i_finish = comps.length - 1; i<= i_finish; ++i){
      update_mod(comps[i], o[i], n[i]);
    }
    return /* () */0;
  }
}

exports.init_mod   = init_mod;
exports.update_mod = update_mod;
/* No side effect */
