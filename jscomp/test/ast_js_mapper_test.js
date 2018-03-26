'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

function tToJs(param) {
  return {
          xx: param[/* xx */0],
          yy: param[/* yy */1],
          zz: param[/* zz */2]
        };
}

function tFromJs(param) {
  return /* record */[
          /* xx */param.xx,
          /* yy */param.yy,
          /* zz */param.zz
        ];
}

var u = tToJs(/* record */[
      /* xx */3,
      /* yy */"x",
      /* zz : tuple */[
        1,
        2
      ]
    ]);

tFromJs(u);

tFromJs({
      xx: 3,
      yy: "2",
      zz: /* tuple */[
        1,
        2
      ],
      cc: 3
    });

function searchForSureExists(xs, k) {
  var _i = 0;
  var xs$1 = xs;
  var k$1 = k;
  while(true) {
    var i = _i;
    var match = xs$1[i];
    if (match[0] === k$1) {
      return match[1];
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

var jsMapperConstantArray = /* int array */[
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

var jsMapperConstantArray$1 = /* array */[
  /* tuple */[
    21902,
    "b0"
  ],
  /* tuple */[
    21903,
    "b1"
  ],
  /* tuple */[
    21904,
    "b2"
  ],
  /* tuple */[
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
