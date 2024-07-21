// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Int64 = require("../../lib/js/int64.js");
let Random = require("../../lib/js/random.js");
let Mt_global = require("./mt_global.js");
let Caml_array = require("../../lib/js/caml_array.js");

let id = {
  contents: 0
};

let suites = {
  contents: /* [] */0
};

function eq(f) {
  return function (extra, extra$1) {
    return Mt_global.collect_eq(id, suites, f, extra, extra$1);
  };
}

function neq(f) {
  return function (extra, extra$1) {
    return Mt_global.collect_neq(id, suites, f, extra, extra$1);
  };
}

function approx(f) {
  return function (extra, extra$1) {
    return Mt_global.collect_approx(id, suites, f, extra, extra$1);
  };
}

neq("File \"random_test.res\", line 9, characters 2-9")((Random.self_init(), Random.int(10000)), (Random.self_init(), Random.int(1000)));

Random.init(0);

let v = Caml_array.make(10, false);

for(let i = 0; i <= 9; ++i){
  Caml_array.set(v, i, Random.bool());
}

eq("File \"random_test.res\", line 28, characters 12-19")(v, [
  true,
  true,
  true,
  true,
  true,
  false,
  true,
  true,
  true,
  false
]);

let f = Random.int64(Int64.max_int);

let h = Random.int64([
  0,
  3
]);

let vv = Random.bits();

let xx = Random.float(3.0);

let xxx = Random.int32(103);

Mt.from_pair_suites("Random_test", suites.contents);

exports.id = id;
exports.suites = suites;
exports.eq = eq;
exports.neq = neq;
exports.approx = approx;
exports.v = v;
exports.f = f;
exports.h = h;
exports.vv = vv;
exports.xx = xx;
exports.xxx = xxx;
/*  Not a pure module */
