'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_splice_call = require("../../lib/js/caml_splice_call.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var Caml_splice_call$1 = { };

Math.max(1);

function f00(a, b) {
  return a.send(b);
}

var a = /* array */[];

a.push(1, 2, 3, 4);

eq("File \"splice_test.ml\", line 29, characters 5-12", a, /* array */[
      1,
      2,
      3,
      4
    ]);

function dynamic(arr) {
  var a = /* array */[];
  Caml_splice_call.spliceObjApply(a, "push", [
        1,
        arr
      ]);
  return eq("File \"splice_test.ml\", line 34, characters 5-12", a, Caml_array.caml_array_concat(/* :: */[
                  /* array */[1],
                  /* :: */[
                    arr,
                    /* [] */0
                  ]
                ]));
}

dynamic(/* array */[
      2,
      3,
      4
    ]);

dynamic(/* array */[]);

dynamic(/* array */[
      1,
      1,
      3
    ]);

var a$1 = /* array */[];

a$1.push(1, 2, 3, 4);

eq("File \"splice_test.ml\", line 51, characters 7-14", a$1, /* array */[
      1,
      2,
      3,
      4
    ]);

function dynamic$1(arr) {
  var a = /* array */[];
  Caml_splice_call.spliceObjApply(a, "push", [
        1,
        arr
      ]);
  return eq("File \"splice_test.ml\", line 56, characters 7-14", a, Caml_array.caml_array_concat(/* :: */[
                  /* array */[1],
                  /* :: */[
                    arr,
                    /* [] */0
                  ]
                ]));
}

dynamic$1(/* array */[
      2,
      3,
      4
    ]);

dynamic$1(/* array */[]);

dynamic$1(/* array */[
      1,
      1,
      3
    ]);

var Pipe = {
  dynamic: dynamic$1
};

function f1(c) {
  return Caml_splice_call.spliceApply(Math.max, [
              1,
              c
            ]);
}

eq("File \"splice_test.ml\", line 67, characters 6-13", Math.max(1, 2, 3), 3);

eq("File \"splice_test.ml\", line 68, characters 6-13", Math.max(1), 1);

eq("File \"splice_test.ml\", line 69, characters 6-13", Math.max(1, 1, 2, 3, 4, 5, 2, 3), 5);

Mt.from_pair_suites("splice_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Caml_splice_call = Caml_splice_call$1;
exports.f00 = f00;
exports.dynamic = dynamic;
exports.Pipe = Pipe;
exports.f1 = f1;
/*  Not a pure module */
