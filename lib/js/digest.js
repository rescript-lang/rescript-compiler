'use strict';

var Char = require("./char.js");
var $$String = require("./string.js");
var Caml_md5 = require("./caml_md5.js");
var Pervasives = require("./pervasives.js");
var Caml_string = require("./caml_string.js");
var Caml_missing_polyfill = require("./caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function string(str) {
  return Caml_md5.caml_md5_string(str, 0, str.length);
}

function bytes(b) {
  return string(Caml_string.bytes_to_string(b));
}

function substring(str, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (str.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Digest.substring"
        ];
  } else {
    return Caml_md5.caml_md5_string(str, ofs, len);
  }
}

function subbytes(b, ofs, len) {
  return substring(Caml_string.bytes_to_string(b), ofs, len);
}

function file(filename) {
  Pervasives.open_in_bin(filename);
  var exit = 0;
  var d;
  try {
    d = Caml_missing_polyfill.not_implemented("caml_md5_chan");
    exit = 1;
  }
  catch (e){
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
    throw e;
  }
  if (exit === 1) {
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
    return d;
  }
  
}

var output = Pervasives.output_string;

function input(chan) {
  return Pervasives.really_input_string(chan, 16);
}

function char_hex(n) {
  return n + (
          n < 10 ? /* "0" */48 : 87
        ) | 0;
}

function to_hex(d) {
  var result = Caml_string.caml_create_string(32);
  for(var i = 0; i <= 15; ++i){
    var x = Caml_string.get(d, i);
    result[(i << 1)] = char_hex((x >>> 4));
    result[(i << 1) + 1 | 0] = char_hex(x & 15);
  }
  return Caml_string.bytes_to_string(result);
}

function from_hex(s) {
  if (s.length !== 32) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Digest.from_hex"
        ];
  }
  var digit = function (c) {
    if (c >= 65) {
      if (c >= 97) {
        if (c >= 103) {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Digest.from_hex"
              ];
        } else {
          return (c - /* "a" */97 | 0) + 10 | 0;
        }
      } else if (c >= 71) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Digest.from_hex"
            ];
      } else {
        return (c - /* "A" */65 | 0) + 10 | 0;
      }
    } else if (c > 57 || c < 48) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Digest.from_hex"
          ];
    } else {
      return c - /* "0" */48 | 0;
    }
  };
  var $$byte = function (i) {
    return (digit(Caml_string.get(s, i)) << 4) + digit(Caml_string.get(s, i + 1 | 0)) | 0;
  };
  var result = Caml_string.caml_create_string(16);
  for(var i = 0; i <= 15; ++i){
    result[i] = Char.chr($$byte((i << 1)));
  }
  return Caml_string.bytes_to_string(result);
}

var compare = $$String.compare;

exports.compare = compare;
exports.string = string;
exports.bytes = bytes;
exports.substring = substring;
exports.subbytes = subbytes;
exports.file = file;
exports.output = output;
exports.input = input;
exports.to_hex = to_hex;
exports.from_hex = from_hex;
/* No side effect */
