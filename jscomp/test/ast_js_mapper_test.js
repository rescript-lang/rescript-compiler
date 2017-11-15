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

exports.tToJs   = tToJs;
exports.tFromJs = tFromJs;
/* u Not a pure module */
