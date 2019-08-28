'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var MoreLabels = require("../../lib/js/moreLabels.js");

function f(x) {
  return x;
}

var XX = /* module */[
  /* init */$$Array.init,
  /* make_matrix */$$Array.make_matrix,
  /* create_matrix */$$Array.create_matrix,
  /* append */$$Array.append,
  /* concat */$$Array.concat,
  /* sub */$$Array.sub,
  /* copy */$$Array.copy,
  /* fill */$$Array.fill,
  /* blit */$$Array.blit,
  /* to_list */$$Array.to_list,
  /* of_list */$$Array.of_list,
  /* iter */$$Array.iter,
  /* map */$$Array.map,
  /* iteri */$$Array.iteri,
  /* mapi */$$Array.mapi,
  /* fold_left */$$Array.fold_left,
  /* fold_right */$$Array.fold_right,
  /* sort */$$Array.sort,
  /* stable_sort */$$Array.stable_sort,
  /* fast_sort */$$Array.fast_sort,
  /* f */f
];

var u = /* array */[/* String */[
    /* make */$$String.make,
    /* init */$$String.init,
    /* copy */$$String.copy,
    /* sub */$$String.sub,
    /* fill */$$String.fill,
    /* blit */$$String.blit,
    /* concat */$$String.concat,
    /* iter */$$String.iter,
    /* iteri */$$String.iteri,
    /* map */$$String.map,
    /* mapi */$$String.mapi,
    /* trim */$$String.trim,
    /* escaped */$$String.escaped,
    /* index */$$String.index,
    /* rindex */$$String.rindex,
    /* index_from */$$String.index_from,
    /* rindex_from */$$String.rindex_from,
    /* contains */$$String.contains,
    /* contains_from */$$String.contains_from,
    /* rcontains_from */$$String.rcontains_from,
    /* uppercase */$$String.uppercase,
    /* lowercase */$$String.lowercase,
    /* capitalize */$$String.capitalize,
    /* uncapitalize */$$String.uncapitalize,
    /* compare */$$String.compare
  ]];

var ghh = Curry._2(MoreLabels.Hashtbl[/* create */0], undefined, 30);

var hh = 1;

exports.XX = XX;
exports.u = u;
exports.hh = hh;
exports.ghh = ghh;
/* ghh Not a pure module */
