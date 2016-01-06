// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_primitive.js
var c = 0;
// Input 3./caml_utils.js
// Input 4./caml_string.js
// Input 5./caml_format.js
// Input 6./camlinternalFormatBasics.js
// Input 7./caml_io.js
// Input 8./pervasives.js
++c;
// Input 9./list.js
// Input 10./char.js
// Input 11./bytes.js
// Input 12./string.js
// Input 13./sys.js
++c;
// Input 14./caml_float.js
// Input 15./buffer.js
// Input 16./camlinternalFormat.js
++c;
// Input 17./printf.js
// Input 18./scanf.js
var h = {}, l = [248, "Invalid_argument", -3], m = [248, "End_of_file", -4];
function n(f, a) {
  var b = Array(1024);
  return [0, 0, "\x00", 0, 0, 0, 0, a, [0, b, 0, 1024, b], f];
}
(function(f, a, b) {
  var d = Array(1024), e = 0, g = 0, k = 0;
  return n(a, function() {
    if (e < g) {
      var a = String.fromCharCode(d[e]);
      ++e;
      return a;
    }
    if (k) {
      throw m;
    }
    if (0 > d.length - 1024) {
      throw [0, l, "input"];
    }
    g = h.a(b, d, 0, 1024);
    if (0 === g) {
      return k = 1, f(b);
    }
    e = 1;
    return String.fromCharCode(d[0]);
  });
})(function() {
  throw m;
}, [0, "-", void 0], void 0);
++c;

