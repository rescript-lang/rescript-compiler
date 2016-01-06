// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_utils.js
// Input 3./caml_string.js
// Input 4./caml_primitive.js
// Input 5./caml_format.js
// Input 6./camlinternalFormatBasics.js
// Input 7./caml_io.js
// Input 8./pervasives.js
var a = 0;
++a;
// Input 9./std_exit.js
var b = {};
a: {
  var c = b.b(0);
  for (;;) {
    if (c) {
      try {
        b.a(c[1]);
      } catch (d) {
      }
      c = c[2];
    } else {
      break a;
    }
  }
}
;
