'use strict';

var Curry = require("../../lib/js/curry.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

function raise(e) {
  throw e;
}

var map = Belt_List.mapU;

var List = {
  map: map
};

var Uncurried = {
  raise: raise,
  List: List
};

var E = /* @__PURE__ */Caml_exceptions.create("Uncurried_cast.E");

function testRaise(param) {
  throw {
        RE_EXN_ID: E,
        Error: new Error()
      };
}

var l = map({
      hd: 1,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }, (function (x) {
        return x + 1 | 0;
      }));

var partial_arg = {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
};

function partial(param) {
  return map(partial_arg, param);
}

var ll = Curry._1(partial, (function (x) {
        return x + 1 | 0;
      }));

var StandardNotation = {
  testRaise: testRaise,
  l: l,
  partial: partial,
  ll: ll
};

function testRaise$1() {
  return raise({
              RE_EXN_ID: E
            });
}

var l$1 = map({
      hd: 1,
      tl: {
        hd: 2,
        tl: /* [] */0
      }
    }, (function (x) {
        return x + 1 | 0;
      }));

var partial_arg$1 = {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
};

function partial$1(param) {
  return map(partial_arg$1, param);
}

var ll$1 = partial$1(function (x) {
      return x + 1 | 0;
    });

exports.Uncurried = Uncurried;
exports.E = E;
exports.StandardNotation = StandardNotation;
exports.testRaise = testRaise$1;
exports.l = l$1;
exports.partial = partial$1;
exports.ll = ll$1;
/* l Not a pure module */
