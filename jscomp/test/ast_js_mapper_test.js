'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

function tToJs(param) {
  return {
          xx: param.xx,
          yy: param.yy,
          zz: param.zz
        };
}

function tFromJs(param) {
  return {
          xx: param.xx,
          yy: param.yy,
          zz: param.zz
        };
}

var u = tToJs({
      xx: 3,
      yy: "x",
      zz: [
        1,
        2
      ]
    });

tFromJs(u);

tFromJs({
      xx: 3,
      yy: "2",
      zz: [
        1,
        2
      ],
      cc: 3
    });

function searchForSureExists(xs, k) {
  var _i = 0;
  while(true) {
    var i = _i;
    var match = xs[i];
    if (match[0] === k) {
      return match[1];
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var jsMapperConstantArray = [
  0,
  3,
  4,
  5
];

function aToJs(param) {
  return jsMapperConstantArray[param];
}

function aFromJs(param) {
  return Js_mapperRt.fromIntAssert(4, jsMapperConstantArray, param);
}

var jsMapperConstantArray$1 = [
  [
    21902,
    "b0"
  ],
  [
    21903,
    "b1"
  ],
  [
    21904,
    "b2"
  ],
  [
    21905,
    "b3"
  ]
];

function bToJs(param) {
  return Js_mapperRt.binarySearch(4, param, jsMapperConstantArray$1);
}

function bFromJs(param) {
  return Js_mapperRt.revSearchAssert(4, jsMapperConstantArray$1, param);
}

bToJs(/* b0 */21902);

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.searchForSureExists = searchForSureExists;
exports.aToJs = aToJs;
exports.aFromJs = aFromJs;
exports.bToJs = bToJs;
exports.bFromJs = bFromJs;
/* u Not a pure module */
