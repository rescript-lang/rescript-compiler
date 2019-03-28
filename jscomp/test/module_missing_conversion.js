'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var MoreLabels = require("../../lib/js/moreLabels.js");

function f(x) {
  return x;
}

var XX = /* module */[
  /* make_float */$$Array.make_float,
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
  /* iteri */$$Array.iteri,
  /* map */$$Array.map,
  /* mapi */$$Array.mapi,
  /* fold_left */$$Array.fold_left,
  /* fold_right */$$Array.fold_right,
  /* iter2 */$$Array.iter2,
  /* map2 */$$Array.map2,
  /* for_all */$$Array.for_all,
  /* exists */$$Array.exists,
  /* mem */$$Array.mem,
  /* memq */$$Array.memq,
  /* sort */$$Array.sort,
  /* stable_sort */$$Array.stable_sort,
  /* fast_sort */$$Array.fast_sort,
  /* Floatarray */$$Array.Floatarray,
  /* f */f
];

var u = /* array */[/* String */[
    $$String.make,
    $$String.init,
    $$String.copy,
    $$String.sub,
    $$String.fill,
    $$String.blit,
    $$String.concat,
    $$String.iter,
    $$String.iteri,
    $$String.map,
    $$String.mapi,
    $$String.trim,
    $$String.escaped,
    $$String.index,
    $$String.index_opt,
    $$String.rindex,
    $$String.rindex_opt,
    $$String.index_from,
    $$String.index_from_opt,
    $$String.rindex_from,
    $$String.rindex_from_opt,
    $$String.contains,
    $$String.contains_from,
    $$String.rcontains_from,
    $$String.uppercase,
    $$String.lowercase,
    $$String.capitalize,
    $$String.uncapitalize,
    $$String.uppercase_ascii,
    $$String.lowercase_ascii,
    $$String.capitalize_ascii,
    $$String.uncapitalize_ascii,
    $$String.compare,
    $$String.equal,
    $$String.split_on_char
  ]];

var ghh = Curry._2(MoreLabels.Hashtbl[/* create */0], undefined, 30);

var hh = 1;

exports.XX = XX;
exports.u = u;
exports.hh = hh;
exports.ghh = ghh;
/* ghh Not a pure module */
