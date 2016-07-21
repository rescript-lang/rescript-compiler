'use strict';

var Block  = require("../../lib/js/block");
var Js_dyn = require("./js_dyn");

var shape = /* array */[
  "x",
  "y",
  "a",
  "b",
  "c",
  "tuple"
];

function vv_to_value(x) {
  var args = x;
  return /* Record */Block.__(12, [
            shape,
            /* array */[
              Js_dyn.int_to_value(args[/* x */0]),
              Js_dyn.string_to_value(args[/* y */1]),
              Js_dyn.int_to_value(args[/* a */2]),
              Js_dyn.float_to_value(args[/* b */3]),
              Js_dyn.int32_to_value(args[/* c */4]),
              Js_dyn.tuple_6_to_value(Js_dyn.int_to_value, Js_dyn.string_to_value, Js_dyn.list_to_value(Js_dyn.string_to_value), Js_dyn.list_to_value(Js_dyn.float_to_value), Js_dyn.array_to_value(Js_dyn.array_to_value(Js_dyn.string_to_value)), Js_dyn.int_to_value)(args[/* tuple */5])
            ]
          ]);
}

var shape_000 = /* constructors : array */[
  "A",
  "B",
  "C",
  "D",
  "E"
];

var shape_001 = /* arities : array */[
  1,
  2,
  2,
  1,
  1
];

var shape$1 = /* record */[
  shape_000,
  shape_001
];

function uu_to_value(x) {
  var param = x;
  switch (param.tag | 0) {
    case 0 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  0,
                  /* array */[Js_dyn.int_to_value(param[0])]
                ]);
    case 1 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  1,
                  /* array */[
                    Js_dyn.int_to_value(param[0]),
                    Js_dyn.string_to_value(param[1])
                  ]
                ]);
    case 2 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  2,
                  /* array */[
                    vv_to_value(param[0]),
                    Js_dyn.option_to_value(Js_dyn.option_to_value(Js_dyn.int_to_value))(param[1])
                  ]
                ]);
    case 3 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  3,
                  /* array */[Js_dyn.array_to_value(Js_dyn.array_to_value(Js_dyn.int_to_value))(param[0])]
                ]);
    case 4 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  4,
                  /* array */[Js_dyn.tuple_2_to_value(Js_dyn.int_to_value, Js_dyn.int_to_value)(param[0])]
                ]);
    
  }
}

var tt_to_value = Js_dyn.list_to_value(Js_dyn.tuple_2_to_value(Js_dyn.int_to_value, Js_dyn.string_to_value));

var u = Js_dyn.int_to_value(3);

var h = Js_dyn.array_to_value(Js_dyn.list_to_value(Js_dyn.list_to_value(Js_dyn.int_to_value)))(/* array */[/* :: */[
        /* :: */[
          3,
          /* [] */0
        ],
        /* [] */0
      ]]);

var hh = Js_dyn.array_to_value(Js_dyn.list_to_value(Js_dyn.list_to_value(tt_to_value)))(/* array */[/* :: */[
        /* :: */[
          /* :: */[
            /* tuple */[
              3,
              "3"
            ],
            /* [] */0
          ],
          /* [] */0
        ],
        /* [] */0
      ]]);

var t_to_value = Js_dyn.int_to_value;

exports.vv_to_value = vv_to_value;
exports.uu_to_value = uu_to_value;
exports.t_to_value  = t_to_value;
exports.u           = u;
exports.h           = h;
exports.hh          = hh;
/* vv_to_value Not a pure module */
