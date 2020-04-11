'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

function eq(param, param$1) {
  var x = param.contents;
  var y = param$1.contents;
  return x === y;
}

function eq2(x, param) {
  var y = param.contents;
  return Caml_obj.caml_equal(x.contents, y);
}

console.log(eq({
          contents: 1
        }, {
          contents: 2
        }));

var u = {
  hi: (function (param, param$1) {
      var x = param.contents;
      var y = param$1.contents;
      return x === y;
    })
};

var h = u.hi({
      contents: 1
    }, {
      contents: 2
    });

exports.eq = eq;
exports.eq2 = eq2;
exports.u = u;
exports.h = h;
/*  Not a pure module */
