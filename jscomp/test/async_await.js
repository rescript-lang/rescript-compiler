'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

function next(n) {
  return n + 1 | 0;
}

async function useNext(param) {
  return 4;
}

function Make(I) {
  var get = async function (key) {
    return await Curry._1(I.get, key);
  };
  return {
          get: get
        };
}

async function topFoo(param) {
  return 1;
}

var arr = [
  1,
  2,
  3
];

var toplevelAwait = await topFoo(undefined);

var toplevelAwait2 = Caml_array.get(arr, await topFoo(undefined));

exports.next = next;
exports.useNext = useNext;
exports.Make = Make;
exports.topFoo = topFoo;
exports.arr = arr;
exports.toplevelAwait = toplevelAwait;
exports.toplevelAwait2 = toplevelAwait2;
/* toplevelAwait Not a pure module */
