'use strict';

var Block      = require("../../lib/js/block");
var Bs_dyn     = require("../../lib/js/bs_dyn");
var Bs_dyn_lib = require("../../lib/js/bs_dyn_lib");

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
              /* Int */Block.__(2, [args[/* x */0]]),
              /* String */Block.__(7, [args[/* y */1]]),
              /* Int */Block.__(2, [args[/* a */2]]),
              /* Float */Block.__(5, [args[/* b */3]]),
              /* Int32 */Block.__(0, [args[/* c */4]]),
              Bs_dyn.tuple_6_to_value(Bs_dyn.int_to_value, Bs_dyn.string_to_value, Bs_dyn.list_to_value(Bs_dyn.string_to_value), Bs_dyn.list_to_value(Bs_dyn.float_to_value), Bs_dyn.array_to_value(Bs_dyn.array_to_value(Bs_dyn.string_to_value)), Bs_dyn.int_to_value)(args[/* tuple */5])
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
                  /* array */[/* Int */Block.__(2, [param[0]])]
                ]);
    case 1 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  1,
                  /* array */[
                    /* Int */Block.__(2, [param[0]]),
                    /* String */Block.__(7, [param[1]])
                  ]
                ]);
    case 2 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  2,
                  /* array */[
                    vv_to_value(param[0]),
                    Bs_dyn.option_to_value(Bs_dyn.option_to_value(Bs_dyn.int_to_value))(param[1])
                  ]
                ]);
    case 3 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  3,
                  /* array */[Bs_dyn.array_to_value(Bs_dyn.array_to_value(Bs_dyn.int_to_value))(param[0])]
                ]);
    case 4 : 
        return /* Variant */Block.__(13, [
                  shape$1,
                  4,
                  /* array */[Bs_dyn.tuple_2_to_value(Bs_dyn.int_to_value, Bs_dyn.int_to_value)(param[0])]
                ]);
    
  }
}

var tt_to_value = Bs_dyn.list_to_value(Bs_dyn.tuple_2_to_value(Bs_dyn.int_to_value, Bs_dyn.string_to_value));

var u = Bs_dyn.int_to_value(3);

var h = Bs_dyn.array_to_value(Bs_dyn.list_to_value(Bs_dyn.list_to_value(Bs_dyn.int_to_value)))(/* array */[/* :: */[
        /* :: */[
          3,
          /* [] */0
        ],
        /* [] */0
      ]]);

var hh = Bs_dyn.array_to_value(Bs_dyn.list_to_value(Bs_dyn.list_to_value(tt_to_value)))(/* array */[/* :: */[
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

console.log(Bs_dyn_lib.to_string(hh));

var shape_000$1 = /* constructors : array */[
  "A",
  "B",
  "C"
];

var shape_001$1 = /* arities : int array */[
  0,
  0,
  1
];

var shape$2 = /* record */[
  shape_000$1,
  shape_001$1
];

function enum_to_value_(param) {
  if (typeof param === "number") {
    if (param !== 0) {
      return /* Variant */Block.__(13, [
                shape$2,
                1,
                /* array */[]
              ]);
    } else {
      return /* Variant */Block.__(13, [
                shape$2,
                0,
                /* array */[]
              ]);
    }
  } else {
    return /* Variant */Block.__(13, [
              shape$2,
              2,
              /* array */[Bs_dyn.int_to_value(param[0])]
            ]);
  }
}

console.log(Bs_dyn_lib.to_string(enum_to_value_(/* C */[3])));

var t_to_value = Bs_dyn.int_to_value;

exports.vv_to_value = vv_to_value;
exports.uu_to_value = uu_to_value;
exports.t_to_value  = t_to_value;
exports.u           = u;
exports.h           = h;
exports.hh          = hh;
/* tt_to_value Not a pure module */
