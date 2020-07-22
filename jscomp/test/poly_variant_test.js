'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function hey_string (option){
  switch(option){
  case "on_closed" : 
  case "on_open" : 
  case "in" : return option
  default : throw Error ("impossible")
 }
}
function hey_int (option){
  switch (option){
   case 0 : 
   case 3 : 
   case 4 : 
   case 5:
   case 6 : return option
   default : throw Error("impossible")
  }
 }
;

var uu = [
  hey_string("on_open"),
  hey_string("on_closed"),
  hey_string("in")
];

var vv = [
  hey_int(3),
  hey_int(0),
  hey_int(4)
];

eq("File \"poly_variant_test.ml\", line 58, characters 5-12", vv, [
      3,
      0,
      4
    ]);

eq("File \"poly_variant_test.ml\", line 59, characters 5-12", [
      hey_int(5),
      hey_int(6)
    ], [
      5,
      6
    ]);

eq("File \"poly_variant_test.ml\", line 60, characters 5-12", uu, [
      "on_open",
      "on_closed",
      "in"
    ]);

hey_string("on_closed");

hey_string("in");

function p_is_int_test(x) {
  if (typeof x === "string") {
    return 2;
  } else {
    return 3;
  }
}

eq("File \"poly_variant_test.ml\", line 142, characters 5-12", 2, 2);

eq("File \"poly_variant_test.ml\", line 143, characters 5-12", 3, p_is_int_test({
          HASH: "b",
          VAL: 2
        }));

Mt.from_pair_suites("Poly_variant_test", suites.contents);

/*  Not a pure module */
