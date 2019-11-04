'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Queue_402 = require("./queue_402.js");
var Caml_array = require("../../lib/js/caml_array.js");

function Test(Queue) {
  var to_array = function (q) {
    var v = Caml_array.caml_make_vect(Curry._1(Queue.length, q), 0);
    Curry._3(Queue.fold, (function (i, e) {
            Caml_array.caml_array_set(v, i, e);
            return i + 1 | 0;
          }), 0, q);
    return v;
  };
  var queue_1 = function (x) {
    var q = Curry._1(Queue.create, /* () */0);
    $$Array.iter((function (x) {
            return Curry._2(Queue.add, x, q);
          }), x);
    return to_array(q);
  };
  return {
          to_array: to_array,
          queue_1: queue_1
        };
}

function to_array(q) {
  var v = Caml_array.caml_make_vect(q[/* length */0], 0);
  Queue.fold((function (i, e) {
          Caml_array.caml_array_set(v, i, e);
          return i + 1 | 0;
        }), 0, q);
  return v;
}

function queue_1(x) {
  var q = [
    /* length */0,
    /* first : Nil */0,
    /* last : Nil */0
  ];
  $$Array.iter((function (x) {
          return Queue.add(x, q);
        }), x);
  return to_array(q);
}

var T1 = {
  to_array: to_array,
  queue_1: queue_1
};

function to_array$1(q) {
  var v = Caml_array.caml_make_vect(q[/* length */0], 0);
  Queue_402.fold((function (i, e) {
          Caml_array.caml_array_set(v, i, e);
          return i + 1 | 0;
        }), 0, q);
  return v;
}

function queue_1$1(x) {
  var q = [
    /* length */0,
    /* tail */undefined
  ];
  $$Array.iter((function (x) {
          return Queue_402.add(x, q);
        }), x);
  return to_array$1(q);
}

var T2 = {
  to_array: to_array$1,
  queue_1: queue_1$1
};

var suites_000 = /* tuple */[
  "File \"queue_test.ml\", line 26, characters 2-9",
  (function (param) {
      var x = /* array */[
        3,
        4,
        5,
        2
      ];
      return /* Eq */Block.__(0, [
                x,
                queue_1(x)
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "File \"queue_test.ml\", line 29, characters 2-9",
    (function (param) {
        var x = /* array */[
          3,
          4,
          5,
          2
        ];
        return /* Eq */Block.__(0, [
                  x,
                  queue_1$1(x)
                ]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Queue_test", suites);

exports.Test = Test;
exports.T1 = T1;
exports.T2 = T2;
exports.suites = suites;
/*  Not a pure module */
