// GENERATED CODE BY BUCKLESCRIPT VERSION 0.5.0 , PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Queue      = require("../queue");
var Block      = require("../block");
var Caml_array = require("../caml_array");
var $$Array    = require("../array");

function to_array(q) {
  var v = Caml_array.caml_make_vect(q[/* length */0], 0);
  Queue.fold(function (i, e) {
        v[i] = e;
        return i + 1 | 0;
      }, 0, q);
  return v;
}

function queue_1(x) {
  var q = /* record */[
    /* length */0,
    /* tail : None */0
  ];
  $$Array.iter(function (x) {
        return Queue.add(x, q);
      }, x);
  return to_array(q);
}

var suites_000 = /* tuple */[
  "simple push",
  function () {
    var x = /* int array */[
      3,
      4,
      5,
      2
    ];
    return /* Eq */Block.__(0, [
              x,
              queue_1(x)
            ]);
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("queue_test.ml", suites);

exports.to_array = to_array;
exports.queue_1  = queue_1;
exports.suites   = suites;
/*  Not a pure module */
