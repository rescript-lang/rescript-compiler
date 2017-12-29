'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
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

var uu = /* array */[
  hey_string("on_open"),
  hey_string("on_closed"),
  hey_string("in")
];

var vv = /* int array */[
  hey_int(3),
  hey_int(0),
  hey_int(4)
];

eq("File \"poly_variant_test.ml\", line 58, characters 5-12", vv, /* int array */[
      3,
      0,
      4
    ]);

eq("File \"poly_variant_test.ml\", line 59, characters 5-12", /* tuple */[
      hey_int(5),
      hey_int(6)
    ], /* tuple */[
      5,
      6
    ]);

eq("File \"poly_variant_test.ml\", line 60, characters 5-12", uu, /* array */[
      "on_open",
      "on_closed",
      "in"
    ]);

hey_string("on_closed");

hey_string("in");

function register(readline) {
  readline.on("line", (function (s) {
          console.log(s);
          return /* () */0;
        }));
  readline.on("close", (function () {
          console.log("finished");
          return /* () */0;
        }));
  return /* () */0;
}

function read(name) {
  return Fs.readFileSync(name, "utf8");
}

function read$1(name) {
  return Fs.readFileSync(name, "utf8");
}

function test(readline, x) {
  readline.on((function (action) {
          switch (x) {
            case -944564236 : 
                return action[0];
            case -933029960 : 
                return action[0];
            
          }
        }), (function (action) {
          switch (x) {
            case -944564236 : 
                return action[1];
            case -933029960 : 
                return action[1];
            
          }
        }));
  return /* () */0;
}

Mt.from_pair_suites("poly_variant_test.ml", suites[0]);

function on2(prim, prim$1) {
  prim.on2((function (action) {
          switch (prim$1) {
            case -944564236 : 
                return action[0];
            case -933029960 : 
                return action[0];
            
          }
        }), (function (action) {
          switch (prim$1) {
            case -944564236 : 
                return action[1];
            case -933029960 : 
                return action[1];
            
          }
        }));
  return /* () */0;
}

var readN = read$1;

exports.uu = uu;
exports.vv = vv;
exports.register = register;
exports.test = test;
exports.on2 = on2;
exports.read = read;
exports.readN = readN;
/*  Not a pure module */
