'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var x = List.length({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

var jsMapperConstantArray = [
  [
    -988374136,
    "http"
  ],
  [
    5243943,
    "idb"
  ],
  [
    561436162,
    "leveldb"
  ]
];

function adapterToJs(param) {
  return Js_mapperRt.binarySearch(3, param, jsMapperConstantArray);
}

function adapterFromJs(param) {
  return Js_mapperRt.revSearch(3, jsMapperConstantArray, param);
}

eq("File \"re_first_test.re\", line 18, characters 3-10", adapterToJs(/* idb */5243943), "idb");

Mt.from_pair_suites("Re_first_test", suites.contents);

var u = 3;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.x = x;
exports.u = u;
exports.adapterToJs = adapterToJs;
exports.adapterFromJs = adapterFromJs;
/* x Not a pure module */
