'use strict';

var Curry = require("../../lib/js/curry");

function map_uncurry(f, a) {
  if (a) {
    return /* :: */[
            f(a[0]),
            map_uncurry(f, a[1])
          ];
  }
  else {
    return /* [] */0;
  }
}

function map(f, a) {
  return map_uncurry(Curry.__1(f), a);
}

map(function (x) {
      return x + 1 | 0;
    }, /* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

function map2(f, a) {
  if (a) {
    return /* :: */[
            f(a[0]),
            map2(f, a[1])
          ];
  }
  else {
    return /* [] */0;
  }
}

function map2$1(f, a) {
  return map2(Curry.__1(f), a);
}

map2$1(function (x) {
      return x + 1 | 0;
    }, /* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

/*  Not a pure module */
