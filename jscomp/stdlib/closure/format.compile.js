// Input 0./caml_exceptions.js
'use strict';
// Input 1./caml_array.js
// Input 2./caml_primitive.js
var m = 0;
// Input 3./caml_utils.js
// Input 4./caml_string.js
// Input 5./caml_format.js
// Input 6./camlinternalFormatBasics.js
// Input 7./caml_io.js
// Input 8./pervasives.js
++m;
// Input 9./list.js
// Input 10./char.js
// Input 11./bytes.js
// Input 12./string.js
// Input 13./sys.js
++m;
// Input 14./caml_float.js
// Input 15./buffer.js
// Input 16./camlinternalFormat.js
++m;
// Input 17./format.js
var n = [248, "Invalid_argument", -3];
function p(a, d, b) {
  for (var c = Array(b), e = 0;e < b;) {
    c[e++] = a[d++];
  }
  return c;
}
var q = [248, "Sys_error", -1], r = [248, "Failure", -2];
function t(a) {
  throw [0, n, a];
}
function u(a, d) {
  var b = 0, c = String.fromCharCode;
  if (0 == b && 4096 >= d && d == a.length) {
    return c.apply(null, a);
  }
  for (var e = "";0 < d;b += 1024, d -= 1024) {
    e += c.apply(null, p(a, b, Math.min(d, 1024)));
  }
  return e;
}
function v(a) {
  if (0 > a) {
    throw [0, n, "String.create"];
  }
  return Array(a);
}
var w = {};
++m;
var x = [0, [0, -1, [0, -1, [0, ""], 0]], 0], y = v(80), z = 80, A = 0;
if (0 < z) {
  for (z += A;A < z;A++) {
    y[A] = 32;
  }
}
var B;
B = u(y, y.length);
function C(a) {
  return "<" + (a + ">");
}
function D(a) {
  return "</" + (a + ">");
}
function E(a) {
  return a;
}
function F(a) {
  return a;
}
function G(a, d, b, c) {
  var e;
  e = [0, 0, 0];
  var f;
  f = [0, -1, [3, 0, 3], 0];
  var g = [0, [0, f, 0]], h = e[1];
  h ? (e[1] = g, h[1][2] = g) : (e[1] = g, e[2] = g);
  return [0, [0, [0, 1, f], x], 0, 0, 0, 0, 78, 10, 68, 78, 0, 1, 1, 1, 1, 2147483647, ".", a, d, b, c, 0, 0, C, D, E, F, e];
}
function H(a, d) {
  var b = G(a, d, function(a) {
    return a;
  }, function(a) {
    return a;
  });
  b[19] = function() {
    return b[17]("\n", 0, 1);
  };
  b[20] = function(a) {
    a: {
      for (;;) {
        if (0 < a) {
          if (80 >= a) {
            a = b[17](B, 0, a);
            break a;
          }
          b[17](B, 0, 80);
          a = a - 80;
        } else {
          a = 0;
          break a;
        }
      }
    }
    return a;
  };
  return b;
}
function I() {
  H(function(a, d, b) {
    if (0 > d || 0 > b || d > a.length - b) {
      a = t("output_substring");
    } else {
      if (!(void 0).b) {
        throw [0, q, "Cannot output to a closed channel"];
      }
      var c;
      "string" === typeof a ? c = a : c = u(a, a.length);
      if (!(0 > c.toString().lastIndexOf("\n"))) {
        if (!(void 0).b) {
          throw [0, q, "Cannot flush a closed channel"];
        }
        if ("" != (void 0).buffer && (void 0).a) {
          switch((void 0).a.length) {
            case 2:
              (void 0).a(void 0, (void 0).buffer);
              break;
            default:
              (void 0).a((void 0).buffer);
          }
        }
      }
      a = 0;
    }
    return a;
  }, function() {
    return w.c(void 0);
  });
}
var J, K = v(512);
J = [0, K, 0, 512, K];
I();
I();
(function(a) {
  return H(function(d, b, c) {
    (0 > b || 0 > c || b + c > d.length) && t("Buffer.add_substring/add_subbytes");
    var e = a[2] + c;
    if (e > a[3]) {
      for (var f = a[3];a[2] + c > f;) {
        f = 2 * f;
      }
      if (34359738359 < f) {
        if (34359738359 >= a[2] + c) {
          f = 34359738359;
        } else {
          throw [0, r, "Buffer.add: cannot grow buffer"];
        }
      }
      var g = v(f), h = a[1], k = a[2];
      if (0 > k || 0 > h.length - k || 0 > g.length - k) {
        t("Bytes.blit");
      } else {
        if (0 !== k) {
          var l, L = Math.min(k, h.length - 0);
          for (l = 0;l < L;l++) {
            g[0 + l] = h[0 + l];
          }
          for (;l < k;l++) {
            g[0 + l] = 0;
          }
        }
      }
      a[1] = g;
      a[3] = f;
    }
    f = a[1];
    g = a[2];
    if (0 > c || 0 > b || b > d.length - c || 0 > g || g > f.length - c) {
      t("String.blit / Bytes.blit_string");
    } else {
      if (0 !== c) {
        k = Math.min(c, d.length - b);
        for (h = 0;h < k;h++) {
          f[g + h] = d.charCodeAt(b + h);
        }
        for (;h < c;h++) {
          f[g + h] = 0;
        }
      }
    }
    return a[2] = e, 0;
  }, function(a) {
    return a;
  });
})(J);

