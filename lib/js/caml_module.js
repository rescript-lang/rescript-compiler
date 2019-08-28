'use strict';

var Caml_obj = require("./caml_obj.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function set_field (o,n,v){
o[n] = v;
};

function get_field (o,n){
return o[n];
};

function init_mod(loc, shape) {
  var undef_module = function (param) {
    throw [
          Caml_builtin_exceptions.undefined_recursive_module,
          loc
        ];
  };
  var loop = function (shape, struct_, name) {
    if (typeof shape === "number") {
      switch (shape) {
        case 0 : 
        case 1 : 
            return set_field(struct_, name, undef_module);
        case 2 : 
            return set_field(struct_, name, /* tuple */[
                        undef_module,
                        undef_module,
                        undef_module,
                        0
                      ]);
        
      }
    } else if (shape.tag) {
      return set_field(struct_, name, shape[0]);
    } else {
      var comps = shape[0];
      var v = ({});
      set_field(struct_, name, v);
      var len = comps.length;
      for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
        var match = comps[i];
        loop(match[0], v, match[1]);
      }
      return /* () */0;
    }
  };
  var res = ({});
  var dummy_name = "module";
  loop(shape, res, dummy_name);
  return get_field(res, dummy_name);
}

function update_mod(shape, o, n) {
  var aux = function (shape, o, n, parent, name) {
    if (typeof shape === "number") {
      switch (shape) {
        case 0 : 
            return set_field(parent, name, n);
        case 1 : 
        case 2 : 
            return Caml_obj.caml_update_dummy(o, n);
        
      }
    } else if (shape.tag) {
      return /* () */0;
    } else {
      var comps = shape[0];
      for(var i = 0 ,i_finish = comps.length - 1 | 0; i <= i_finish; ++i){
        var match = comps[i];
        var name$1 = match[1];
        aux(match[0], get_field(o, name$1), get_field(n, name$1), o, name$1);
      }
      return /* () */0;
    }
  };
  if (typeof shape === "number") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "caml_module.ml",
            109,
            10
          ]
        ];
  } else if (shape.tag) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "caml_module.ml",
            109,
            10
          ]
        ];
  } else {
    var comps = shape[0];
    for(var i = 0 ,i_finish = comps.length - 1 | 0; i <= i_finish; ++i){
      var match = comps[i];
      var name = match[1];
      aux(match[0], get_field(o, name), get_field(n, name), o, name);
    }
    return /* () */0;
  }
}

exports.init_mod = init_mod;
exports.update_mod = update_mod;
/* No side effect */
