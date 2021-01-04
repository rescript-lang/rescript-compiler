'use strict';

var Caml_bytes = require("./caml_bytes.js");

function chr(n) {
  if (n < 0 || n > 255) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Char.chr",
          Error: new Error()
        };
  }
  return n;
}

function escaped(c) {
  var exit = 0;
  if (c >= 40) {
    if (c === 92) {
      return "\\\\";
    }
    exit = c >= 127 ? 1 : 2;
  } else if (c >= 32) {
    if (c >= 39) {
      return "\\'";
    }
    exit = 2;
  } else if (c >= 14) {
    exit = 1;
  } else {
    switch (c) {
      case 8 :
          return "\\b";
      case 9 :
          return "\\t";
      case 10 :
          return "\\n";
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
          return "\\r";
      
    }
  }
  switch (exit) {
    case 1 :
        var s = [
          0,
          0,
          0,
          0
        ];
        s[0] = /* '\\' */92;
        s[1] = 48 + (c / 100 | 0) | 0;
        s[2] = 48 + (c / 10 | 0) % 10 | 0;
        s[3] = 48 + c % 10 | 0;
        return Caml_bytes.bytes_to_string(s);
    case 2 :
        var s$1 = [0];
        s$1[0] = c;
        return Caml_bytes.bytes_to_string(s$1);
    
  }
}

function lowercase(c) {
  if (c >= /* 'A' */65 && c <= /* 'Z' */90 || c >= /* '\192' */192 && c <= /* '\214' */214 || c >= /* '\216' */216 && c <= /* '\222' */222) {
    return c + 32 | 0;
  } else {
    return c;
  }
}

function uppercase(c) {
  if (c >= /* 'a' */97 && c <= /* 'z' */122 || c >= /* '\224' */224 && c <= /* '\246' */246 || c >= /* '\248' */248 && c <= /* '\254' */254) {
    return c - 32 | 0;
  } else {
    return c;
  }
}

function lowercase_ascii(c) {
  if (c >= /* 'A' */65 && c <= /* 'Z' */90) {
    return c + 32 | 0;
  } else {
    return c;
  }
}

function uppercase_ascii(c) {
  if (c >= /* 'a' */97 && c <= /* 'z' */122) {
    return c - 32 | 0;
  } else {
    return c;
  }
}

function compare(c1, c2) {
  return c1 - c2 | 0;
}

function equal(c1, c2) {
  return (c1 - c2 | 0) === 0;
}

exports.chr = chr;
exports.escaped = escaped;
exports.lowercase = lowercase;
exports.uppercase = uppercase;
exports.lowercase_ascii = lowercase_ascii;
exports.uppercase_ascii = uppercase_ascii;
exports.compare = compare;
exports.equal = equal;
/* No side effect */
