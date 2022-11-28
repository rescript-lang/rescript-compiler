'use strict';

var Curry = require("../../lib/js/curry.js");

async function willBeInlined(param) {
  return 3;
}

var inlined = willBeInlined(undefined);

function wrapSomethingAsync(param) {
  ((async function (param) {
          var test = await Promise.resolve("Test");
          console.log(test);
        })(777));
}

function wrapSomethingAsync2(param) {
  ((async function (param) {
          var test = await Promise.resolve("Test");
          console.log(test);
        })(undefined));
}

async function doSomethingAsync(someAsyncFunction) {
  return await Curry._1(someAsyncFunction, undefined);
}

var broken = doSomethingAsync;

var M = {
  broken: broken
};

async function broken$1(someAsyncFunction) {
  return await Curry._1(someAsyncFunction, undefined);
}

var broken$2 = broken$1;

exports.willBeInlined = willBeInlined;
exports.inlined = inlined;
exports.wrapSomethingAsync = wrapSomethingAsync;
exports.wrapSomethingAsync2 = wrapSomethingAsync2;
exports.M = M;
exports.broken = broken$2;
/* inlined Not a pure module */
