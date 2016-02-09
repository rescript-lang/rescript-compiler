// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Queue      = require("../stdlib/queue");
var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");

function to_array(q) {
  var v = Caml_array.caml_make_vect(q[1], 0);
  Queue.fold(function (i, e) {
        v[i] = e;
        return i + 1;
      }, 0, q);
  return v;
}

function queue_1(x) {
  var q = [
    /* record */0,
    0,
    /* None */0
  ];
  $$Array.iter(function (x) {
        return Queue.add(x, q);
      }, x);
  return to_array(q);
}

var suites_001 = [
  /* tuple */0,
  "simple push",
  function () {
    var x = /* array */[
      3,
      4,
      5,
      2
    ];
    return [
            /* Eq */0,
            x,
            queue_1(x)
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("queue_test.ml", suites);

exports.to_array = to_array;
exports.queue_1  = queue_1;
exports.suites   = suites;
/*  Not a pure module */
