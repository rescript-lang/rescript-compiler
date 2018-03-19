'use strict';

var Bytes = require("../../lib/js/bytes.js");

function gray_encode(b) {
  return b ^ (b >>> 1);
}

function gray_decode(n) {
  var _p = n;
  var _n = (n >>> 1);
  while(true) {
    var n$1 = _n;
    var p = _p;
    if (n$1 === 0) {
      return p;
    } else {
      _n = (n$1 >>> 1);
      _p = p ^ n$1;
      continue ;
    }
  };
}

function bool_string(len, n) {
  var s = Bytes.make(len, /* "0" */48);
  var _i = len - 1 | 0;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var i = _i;
    if ((n$1 & 1) === 1) {
      s[i] = /* "1" */49;
    }
    if (i <= 0) {
      return s;
    } else {
      _n = (n$1 >>> 1);
      _i = i - 1 | 0;
      continue ;
    }
  };
}

function next_power(v) {
  var v$1 = v - 1 | 0;
  var v$2 = (v$1 >>> 1) | v$1;
  var v$3 = (v$2 >>> 2) | v$2;
  var v$4 = (v$3 >>> 4) | v$3;
  var v$5 = (v$4 >>> 8) | v$4;
  var v$6 = (v$5 >>> 16) | v$5;
  return v$6 + 1 | 0;
}

exports.gray_encode = gray_encode;
exports.gray_decode = gray_decode;
exports.bool_string = bool_string;
exports.next_power = next_power;
/* No side effect */
