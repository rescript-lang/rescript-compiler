'use strict';

var Mt = require("./mt.js");
var Char = require("../../lib/js/char.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function escaped(s) {
  var n = /* record */{
    contents: 0
  };
  for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
    var match = s[i];
    var tmp;
    if (match >= 32) {
      var switcher = match - 34 | 0;
      tmp = switcher > 58 || switcher < 0 ? (
          switcher >= 93 ? 4 : 1
        ) : (
          switcher > 57 || switcher < 1 ? 2 : 1
        );
    } else {
      tmp = match >= 11 ? (
          match !== 13 ? 4 : 2
        ) : (
          match >= 8 ? 2 : 4
        );
    }
    n.contents = n.contents + tmp | 0;
  }
  if (n.contents === s.length) {
    return Bytes.copy(s);
  } else {
    var s$prime = Caml_bytes.caml_create_bytes(n.contents);
    n.contents = 0;
    for(var i$1 = 0 ,i_finish$1 = s.length - 1 | 0; i$1 <= i_finish$1; ++i$1){
      var c = s[i$1];
      var exit = 0;
      if (c >= 35) {
        if (c !== 92) {
          if (c >= 127) {
            exit = 1;
          } else {
            s$prime[n.contents] = c;
          }
        } else {
          exit = 2;
        }
      } else if (c >= 32) {
        if (c >= 34) {
          exit = 2;
        } else {
          s$prime[n.contents] = c;
        }
      } else if (c >= 14) {
        exit = 1;
      } else {
        switch (c) {
          case 8 :
              s$prime[n.contents] = /* "\\" */92;
              Pervasives.incr(n);
              s$prime[n.contents] = /* "b" */98;
              break;
          case 9 :
              s$prime[n.contents] = /* "\\" */92;
              Pervasives.incr(n);
              s$prime[n.contents] = /* "t" */116;
              break;
          case 10 :
              s$prime[n.contents] = /* "\\" */92;
              Pervasives.incr(n);
              s$prime[n.contents] = /* "n" */110;
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
              s$prime[n.contents] = /* "\\" */92;
              Pervasives.incr(n);
              s$prime[n.contents] = /* "r" */114;
              break;
          
        }
      }
      switch (exit) {
        case 1 :
            s$prime[n.contents] = /* "\\" */92;
            Pervasives.incr(n);
            s$prime[n.contents] = 48 + (c / 100 | 0) | 0;
            Pervasives.incr(n);
            s$prime[n.contents] = 48 + (c / 10 | 0) % 10 | 0;
            Pervasives.incr(n);
            s$prime[n.contents] = 48 + c % 10 | 0;
            break;
        case 2 :
            s$prime[n.contents] = /* "\\" */92;
            Pervasives.incr(n);
            s$prime[n.contents] = c;
            break;
        
      }
      Pervasives.incr(n);
    }
    return s$prime;
  }
}

function starts_with(xs, prefix, p) {
  var H = Caml_exceptions.create("H");
  var len1 = xs.length;
  var len2 = prefix.length;
  if (len2 > len1) {
    return false;
  } else {
    try {
      for(var i = 0 ,i_finish = len2 - 1 | 0; i <= i_finish; ++i){
        if (!Curry._2(p, Caml_bytes.get(xs, i), Caml_bytes.get(prefix, i))) {
          throw H;
        }
        
      }
      return true;
    }
    catch (exn){
      if (exn === H) {
        return false;
      } else {
        throw exn;
      }
    }
  }
}

var a = Bytes.init(100, Char.chr);

Bytes.blit(a, 5, a, 10, 10);

eq("File \"ext_bytes_test.ml\", line 96, characters 7-14", a, Bytes.of_string("\0\x01\x02\x03\x04\x05\x06\x07\b\t\x05\x06\x07\b\t\n\x0b\f\r\x0e\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abc"));

var a$1 = Bytes.init(100, Char.chr);

Bytes.blit(a$1, 10, a$1, 5, 10);

eq("File \"ext_bytes_test.ml\", line 102, characters 7-14", a$1, Bytes.of_string("\0\x01\x02\x03\x04\n\x0b\f\r\x0e\x0f\x10\x11\x12\x13\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abc"));

var f = Char.chr;

var a$2 = Caml_bytes.bytes_to_string(Bytes.init(100, f));

var b = Bytes.init(100, (function (i) {
        return /* "\000" */0;
      }));

Bytes.blit_string(a$2, 10, b, 5, 10);

eq("File \"ext_bytes_test.ml\", line 109, characters 7-14", b, Bytes.of_string("\0\0\0\0\0\n\x0b\f\r\x0e\x0f\x10\x11\x12\x13\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"));

Mt.from_pair_suites("Ext_bytes_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.escaped = escaped;
exports.starts_with = starts_with;
/* a Not a pure module */
