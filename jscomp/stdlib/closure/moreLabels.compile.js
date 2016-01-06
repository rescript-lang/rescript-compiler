// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_primitive.js
var b = 0;
// Input 3./caml_utils.js
// Input 4./caml_string.js
// Input 5./caml_format.js
// Input 6./camlinternalFormatBasics.js
// Input 7./caml_io.js
// Input 8./pervasives.js
++b;
// Input 9./list.js
// Input 10./set.js
// Input 11./array.js
++b;
// Input 12./sys.js
++b;
// Input 13./caml_obj_runtime.js
// Input 14./char.js
// Input 15./bytes.js
// Input 16./marshal.js
// Input 17./obj.js
// Input 18./camlinternalLazy.js
++b;
// Input 19./string.js
// Input 20./caml_sys.js
// Input 21./int64.js
// Input 22./digest.js
// Input 23./int32.js
// Input 24./nativeint.js
// Input 25./random.js
// Input 26./hashtbl.js
var c = [248, "Invalid_argument", -3], d = [248, "Not_found", -6];
function e(a) {
  if (void 0 !== typeof process && process.a && void 0 != process.a[a]) {
    return process.a[a];
  }
  throw d;
}
var f;
try {
  f = e("OCAMLRUNPARAM");
} catch (a) {
  if (a === d) {
    try {
      f = e("CAMLRUNPARAM");
    } catch (l) {
      if (l === d) {
        f = "";
      } else {
        throw l;
      }
    }
  } else {
    throw a;
  }
}
var g;
var h = f, k = h.length, m = 0;
if (0 === k) {
  g = [];
} else {
  for (var n = Array(k);m < k;++m) {
    n[m] = h.charCodeAt(m);
  }
  g = n;
}
var p = g, q = p.length;
if (0 > q) {
  throw [0, c, "String.contains_from / Bytes.contains_from"];
}
try {
  var r = 0;
  for (;;) {
    if (r >= q) {
      throw d;
    }
    if ("R" === String.fromCharCode(p[r])) {
      break;
    }
    r += 1;
  }
} catch (a) {
  if (a !== d) {
    throw a;
  }
}
;
// Input 27./map.js
// Input 28./moreLabels.js

