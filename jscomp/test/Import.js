'use strict';

var Curry = require("../../lib/js/curry.js");

async function eachIntAsync(list, f) {
  return Curry._2(await import("../../lib/js/belt_List.js").then(function (m) {
                  return m.forEach;
                }), list, f);
}

function eachIntLazy(list, f) {
  var obj = import("../../lib/js/belt_List.js").then(function (m) {
        return m.forEach;
      });
  var arg1 = function (each) {
    return Promise.resolve(Curry._2(each, list, f));
  };
  return obj.then(arg1);
}

eachIntLazy({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, (function (n) {
        console.log("lazy", n);
      }));

eachIntAsync({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, (function (n) {
        console.log("async", n);
      }));

var beltAsModule = await import("../../lib/js/belt_List.js");

var M = await import("../../lib/js/belt_List.js");

var each = M.forEach;

exports.eachIntAsync = eachIntAsync;
exports.eachIntLazy = eachIntLazy;
exports.beltAsModule = beltAsModule;
exports.M = M;
exports.each = each;
/*  Not a pure module */
