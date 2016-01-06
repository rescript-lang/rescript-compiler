// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_primitive.js
var b = 0;
// Input 3./sys.js
++b;
// Input 4./caml_obj_runtime.js
// Input 5./caml_utils.js
// Input 6./caml_string.js
// Input 7./caml_format.js
// Input 8./camlinternalFormatBasics.js
// Input 9./caml_io.js
// Input 10./pervasives.js
++b;
// Input 11./list.js
// Input 12./char.js
// Input 13./bytes.js
// Input 14./string.js
// Input 15./marshal.js
// Input 16./obj.js
// Input 17./camlinternalLazy.js
++b;
// Input 18./caml_sys.js
// Input 19./array.js
++b;
// Input 20./int64.js
// Input 21./digest.js
// Input 22./int32.js
// Input 23./nativeint.js
// Input 24./random.js
// Input 25./caml_float.js
// Input 26./buffer.js
// Input 27./camlinternalFormat.js
++b;
// Input 28./printf.js
// Input 29./filename.js
var c = [248, "Not_found", -6];
function d(a) {
  if (void 0 === typeof process || !process.a || void 0 == process.a[a]) {
    throw c;
  }
}
try {
  d("TMPDIR");
} catch (a) {
  if (a !== c) {
    throw a;
  }
}
try {
  d("TEMP");
} catch (a) {
  if (a !== c) {
    throw a;
  }
}
switch("Unix") {
  case "Cygwin":
    break;
  case "Unix":
    break;
  case "Win32":
    break;
  default:
    throw [0, [248, "Assert_failure", -10], [0, "filename.ml", 189, 9]];;
}
;
