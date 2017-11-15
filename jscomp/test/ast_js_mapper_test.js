'use strict';


function tToJs(record) {
  return {
          xx: record[/* xx */0],
          yy: record[/* yy */1],
          zz: record[/* zz */2]
        };
}

function tFromJs(obj) {
  return /* record */[
          /* xx */obj.xx,
          /* yy */obj.yy,
          /* zz */obj.zz
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

exports.tToJs               = tToJs;
exports.tFromJs             = tFromJs;
exports.searchForSureExists = searchForSureExists;
/* u Not a pure module */
