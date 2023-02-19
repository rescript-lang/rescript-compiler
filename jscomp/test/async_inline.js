'use strict';

var Curry = require("../../lib/js/curry.js");
var React = require("react");

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

function nested1(param) {
  return async function (y) {
    return await y;
  };
}

async function nested2(param) {
  return async function (y) {
    return await y;
  };
}

function onSubmit(param) {
  return React.useCallback(async function (_a, b) {
              return await b;
            });
}

exports.willBeInlined = willBeInlined;
exports.inlined = inlined;
exports.wrapSomethingAsync = wrapSomethingAsync;
exports.wrapSomethingAsync2 = wrapSomethingAsync2;
exports.M = M;
exports.broken = broken$2;
exports.nested1 = nested1;
exports.nested2 = nested2;
exports.onSubmit = onSubmit;
/* inlined Not a pure module */
