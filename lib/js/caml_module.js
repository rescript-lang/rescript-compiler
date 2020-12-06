'use strict';

var Caml_obj = require("./caml_obj.js");

function init_mod(loc, shape) {
  var undef_module = function (param) {
    throw {
          RE_EXN_ID: "Undefined_recursive_module",
          _1: loc,
          Error: new Error()
        };
  };
  var loop = function (shape, struct_, idx) {
    if (typeof shape === "number") {
      switch (shape) {
        case /* Function */0 :
            struct_[idx] = undef_module;
            return ;
        case /* Lazy */1 :
            struct_[idx] = {
              LAZY_DONE: true,
              VAL: undef_module
            };
            return ;
        case /* Class */2 :
            struct_[idx] = [
              undef_module,
              undef_module,
              undef_module,
              0
            ];
            return ;
        
      }
    } else {
      if (shape.TAG === /* Module */0) {
        var comps = shape._0;
        var v = {};
        struct_[idx] = v;
        var len = comps.length;
        for(var i = 0; i < len; ++i){
          var match = comps[i];
          loop(match[0], v, match[1]);
        }
        return ;
      }
      struct_[idx] = shape._0;
      return ;
    }
  };
  var res = {};
  var dummy_name = "dummy";
  loop(shape, res, dummy_name);
  return res[dummy_name];
}

function update_mod(shape, o, n) {
  var aux = function (shape, o, n, parent, i) {
    if (typeof shape === "number") {
      switch (shape) {
        case /* Function */0 :
            parent[i] = n;
            return ;
        case /* Lazy */1 :
        case /* Class */2 :
            return Caml_obj.update_dummy(o, n);
        
      }
    } else {
      if (shape.TAG !== /* Module */0) {
        return ;
      }
      var comps = shape._0;
      for(var i$1 = 0 ,i_finish = comps.length; i$1 < i_finish; ++i$1){
        var match = comps[i$1];
        var name = match[1];
        aux(match[0], o[name], n[name], o, name);
      }
      return ;
    }
  };
  if (typeof shape === "number") {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "caml_module.ml",
            107,
            10
          ],
          Error: new Error()
        };
  }
  if (shape.TAG === /* Module */0) {
    var comps = shape._0;
    for(var i = 0 ,i_finish = comps.length; i < i_finish; ++i){
      var match = comps[i];
      var name = match[1];
      aux(match[0], o[name], n[name], o, name);
    }
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "caml_module.ml",
          107,
          10
        ],
        Error: new Error()
      };
}

exports.init_mod = init_mod;
exports.update_mod = update_mod;
/* No side effect */
