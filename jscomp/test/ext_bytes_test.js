'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var Char = require("../../lib/js/char.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function escaped(s) {
  var n = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    var match = s[i];
    n = n + (
      match >= 32 ? (
          match > 92 || match < 34 ? (
              match >= 127 ? 4 : 1
            ) : (
              match > 91 || match < 35 ? 2 : 1
            )
        ) : (
          match >= 11 ? (
              match !== 13 ? 4 : 2
            ) : (
              match >= 8 ? 2 : 4
            )
        )
    ) | 0;
  }
  if (n === s.length) {
    return Bytes.copy(s);
  }
  var s$p = Caml_bytes.create(n);
  n = 0;
  for(var i$1 = 0 ,i_finish$1 = s.length; i$1 < i_finish$1; ++i$1){
    var c = s[i$1];
    var exit = 0;
    if (c >= 35) {
      if (c !== 92) {
        if (c >= 127) {
          exit = 1;
        } else {
          s$p[n] = c;
        }
      } else {
        exit = 2;
      }
    } else if (c >= 32) {
      if (c >= 34) {
        exit = 2;
      } else {
        s$p[n] = c;
      }
    } else if (c >= 14) {
      exit = 1;
    } else {
      switch (c) {
        case 8 :
            s$p[n] = /* '\\' */92;
            n = n + 1 | 0;
            s$p[n] = /* 'b' */98;
            break;
        case 9 :
            s$p[n] = /* '\\' */92;
            n = n + 1 | 0;
            s$p[n] = /* 't' */116;
            break;
        case 10 :
            s$p[n] = /* '\\' */92;
            n = n + 1 | 0;
            s$p[n] = /* 'n' */110;
            break;
        case 0 :
        case 1 :
        case 2 :
        case 3 :
        case 4 :
        case 5 :
        case 6 :
        case 7 :
        case 11 :
        case 12 :
            exit = 1;
            break;
        case 13 :
            s$p[n] = /* '\\' */92;
            n = n + 1 | 0;
            s$p[n] = /* 'r' */114;
            break;
        
      }
    }
    switch (exit) {
      case 1 :
          s$p[n] = /* '\\' */92;
          n = n + 1 | 0;
          s$p[n] = 48 + (c / 100 | 0) | 0;
          n = n + 1 | 0;
          s$p[n] = 48 + (c / 10 | 0) % 10 | 0;
          n = n + 1 | 0;
          s$p[n] = 48 + c % 10 | 0;
          break;
      case 2 :
          s$p[n] = /* '\\' */92;
          n = n + 1 | 0;
          s$p[n] = c;
          break;
      
    }
    n = n + 1 | 0;
  }
  return s$p;
}

function starts_with(xs, prefix, p) {
  var H = /* @__PURE__ */Caml_exceptions.create("H");
  var len1 = xs.length;
  var len2 = prefix.length;
  if (len2 > len1) {
    return false;
  }
  try {
    for(var i = 0; i < len2; ++i){
      if (!Curry._2(p, Caml_bytes.get(xs, i), Caml_bytes.get(prefix, i))) {
        throw {
              RE_EXN_ID: H,
              Error: new Error()
            };
      }
      
    }
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === H) {
      return false;
    }
    throw exn;
  }
}

var a = Bytes.init(100, Char.chr);

Bytes.blit(a, 5, a, 10, 10);

eq("File \"ext_bytes_test.res\", line 116, characters 4-11", a, Bytes.of_string("\x00\x01\x02\x03\x04\x05\x06\x07\b\t\x05\x06\x07\b\t\n\x0b\x0c\r\x0e\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abc"));

var a$1 = Bytes.init(100, Char.chr);

Bytes.blit(a$1, 10, a$1, 5, 10);

eq("File \"ext_bytes_test.res\", line 128, characters 4-11", a$1, Bytes.of_string("\x00\x01\x02\x03\x04\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abc"));

var f = Char.chr;

var a$2 = Bytes.unsafe_to_string(Bytes.init(100, f));

var b = Bytes.init(100, (function (i) {
        return /* '\000' */0;
      }));

Bytes.blit_string(a$2, 10, b, 5, 10);

eq("File \"ext_bytes_test.res\", line 141, characters 4-11", b, Bytes.of_string("\x00\x00\x00\x00\x00\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"));

var s = Bytes.init(50000, (function (i) {
        return Char.chr(i % 137);
      }));

var s1 = Bytes.to_string(s);

var s2 = Bytes.of_string(s1);

eq("File \"ext_bytes_test.res\", line 153, characters 5-12", s, s2);

function f$1(a, b) {
  return [
          Caml_bytes.bytes_greaterthan(a, b),
          Caml_bytes.bytes_greaterequal(a, b),
          Caml_bytes.bytes_lessthan(a, b),
          Caml_bytes.bytes_lessequal(a, b),
          Caml_bytes.bytes_equal(a, b)
        ];
}

function f_0(a, b) {
  return [
          Caml.i64_gt(a, b),
          Caml.i64_ge(a, b),
          Caml.i64_lt(a, b),
          Caml.i64_le(a, b),
          Caml.i64_eq(a, b)
        ];
}

Mt.from_pair_suites("Ext_bytes_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.escaped = escaped;
exports.starts_with = starts_with;
exports.f = f$1;
exports.f_0 = f_0;
/* a Not a pure module */
