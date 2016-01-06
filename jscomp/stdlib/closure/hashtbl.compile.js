// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_utils.js
// Input 3./caml_string.js
// Input 4./caml_primitive.js
var b = 0;
// Input 5./caml_format.js
// Input 6./camlinternalFormatBasics.js
// Input 7./caml_io.js
// Input 8./pervasives.js
++b;
// Input 9./array.js
++b;
// Input 10./sys.js
++b;
// Input 11./caml_obj_runtime.js
// Input 12./list.js
// Input 13./char.js
// Input 14./bytes.js
// Input 15./marshal.js
// Input 16./obj.js
// Input 17./camlinternalLazy.js
++b;
// Input 18./string.js
// Input 19./caml_sys.js
// Input 20./int64.js
// Input 21./digest.js
// Input 22./int32.js
// Input 23./nativeint.js
// Input 24./random.js
// Input 25./hashtbl.js
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
