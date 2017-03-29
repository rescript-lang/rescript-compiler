'use strict';

var $$Array    = require("./array.js");
var Block      = require("./block.js");
var Caml_array = require("./caml_array.js");

function int32_to_value(x) {
  return /* Int32 */Block.__(0, [x]);
}

function int64_to_value(x) {
  return /* Int64 */Block.__(1, [x]);
}

function int_to_value(x) {
  return /* Int */Block.__(2, [x]);
}

function nativeint_to_value(x) {
  return /* Nativeint */Block.__(3, [x]);
}

function bool_to_value(x) {
  return /* Bool */Block.__(4, [x]);
}

function float_to_value(x) {
  return /* Float */Block.__(5, [x]);
}

function char_to_value(x) {
  return /* Char */Block.__(6, [x]);
}

function string_to_value(x) {
  return /* String */Block.__(7, [x]);
}

function array_map(f, a) {
  var l = a.length;
  if (l) {
    var r = Caml_array.caml_make_vect(l, f(a[0]));
    for(var i = 1 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      r[i] = f(a[i]);
    }
    return r;
  } else {
    return /* array */[];
  }
}

function array_to_value(k) {
  return function (x) {
    return /* Array */Block.__(10, [array_map(k, x)]);
  };
}

function list_to_value(k) {
  return function (x) {
    return /* Array */Block.__(10, [array_map(k, $$Array.of_list(x))]);
  };
}

function record_to_value(labels, v) {
  return /* Record */Block.__(12, [
            labels,
            v
          ]);
}

function variant_to_value(labels, tag, vs) {
  return /* Variant */Block.__(13, [
            labels,
            tag,
            vs
          ]);
}

function tuple_2_to_value(k0, k1) {
  return function (param) {
    return /* Tuple */Block.__(9, [/* array */[
                k0(param[0]),
                k1(param[1])
              ]]);
  };
}

function tuple_3_to_value(k0, k1, k2) {
  return function (param) {
    return /* Tuple */Block.__(9, [/* array */[
                k0(param[0]),
                k1(param[1]),
                k2(param[2])
              ]]);
  };
}

function tuple_4_to_value(k0, k1, k2, k3) {
  return function (param) {
    return /* Tuple */Block.__(9, [/* array */[
                k0(param[0]),
                k1(param[1]),
                k2(param[2]),
                k3(param[3])
              ]]);
  };
}

function tuple_5_to_value(k0, k1, k2, k3, k4) {
  return function (param) {
    return /* Tuple */Block.__(9, [/* array */[
                k0(param[0]),
                k1(param[1]),
                k2(param[2]),
                k3(param[3]),
                k4(param[4])
              ]]);
  };
}

function tuple_6_to_value(k0, k1, k2, k3, k4, k5) {
  return function (param) {
    return /* Tuple */Block.__(9, [/* array */[
                k0(param[0]),
                k1(param[1]),
                k2(param[2]),
                k3(param[3]),
                k4(param[4]),
                k5(param[5])
              ]]);
  };
}

function option_to_value(k) {
  return function (x) {
    if (x) {
      return /* OptionSome */Block.__(8, [k(x[0])]);
    } else {
      return /* OptionNone */0;
    }
  };
}

function shape_of_record(labels) {
  return labels;
}

function shape_of_variant(constructors, arities) {
  return /* record */[
          /* constructors */constructors,
          /* arities */arities
        ];
}

exports.int32_to_value     = int32_to_value;
exports.int64_to_value     = int64_to_value;
exports.int_to_value       = int_to_value;
exports.nativeint_to_value = nativeint_to_value;
exports.bool_to_value      = bool_to_value;
exports.float_to_value     = float_to_value;
exports.char_to_value      = char_to_value;
exports.string_to_value    = string_to_value;
exports.array_to_value     = array_to_value;
exports.list_to_value      = list_to_value;
exports.option_to_value    = option_to_value;
exports.record_to_value    = record_to_value;
exports.variant_to_value   = variant_to_value;
exports.tuple_2_to_value   = tuple_2_to_value;
exports.tuple_3_to_value   = tuple_3_to_value;
exports.tuple_4_to_value   = tuple_4_to_value;
exports.tuple_5_to_value   = tuple_5_to_value;
exports.tuple_6_to_value   = tuple_6_to_value;
exports.shape_of_variant   = shape_of_variant;
exports.shape_of_record    = shape_of_record;
/* No side effect */
