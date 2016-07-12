'use strict';

var Block  = require("../../lib/js/block");
var Js_dyn = require("../../lib/js/js_dyn");

var all_branches_of_t_000 = /* constructors : array */[
  "Monday",
  "Tuesday",
  "SpecialDay"
];

var all_branches_of_t_001 = /* arities : array */[
  0,
  0,
  1,
  2,
  1
];

var all_branches_of_t = /* record */[
  all_branches_of_t_000,
  all_branches_of_t_001
];

function _t_to_value(value) {
  if (typeof value === "number") {
    if (value) {
      return /* Variant */Block.__(13, [
                all_branches_of_t,
                1,
                /* array */[]
              ]);
    }
    else {
      return /* Variant */Block.__(13, [
                all_branches_of_t,
                0,
                /* array */[]
              ]);
    }
  }
  else {
    switch (value.tag | 0) {
      case 0 : 
          return /* Variant */Block.__(13, [
                    all_branches_of_t,
                    2,
                    /* array */[Js_dyn.int_to_value(value[0])]
                  ]);
      case 1 : 
          return /* Variant */Block.__(13, [
                    all_branches_of_t,
                    3,
                    /* array */[
                      Js_dyn.int_to_value(value[0]),
                      Js_dyn.int_to_value(value[1])
                    ]
                  ]);
      case 2 : 
          return /* Variant */Block.__(13, [
                    all_branches_of_t,
                    4,
                    /* array */[Js_dyn.tuple_2_to_value(Js_dyn.int_to_value, Js_dyn.int_to_value)(value[0])]
                  ]);
      case 3 : 
          return /* Variant */Block.__(13, [
                    all_branches_of_t,
                    5,
                    /* array */[_t_to_value(value[0])]
                  ]);
      
    }
  }
}

var t_to_value = _t_to_value

var shape = /* array */[
  "x",
  "y",
  "z"
];

function u_to_value(value) {
  return /* Record */Block.__(12, [
            shape,
            /* array */[
              Js_dyn.int_to_value(value[/* x */0]),
              t_to_value(value[/* y */1]),
              Js_dyn.string_to_value(value[/* z */2])
            ]
          ]);
}

exports.t_to_value = t_to_value;
exports.u_to_value = u_to_value;
/* t_to_value Not a pure module */
