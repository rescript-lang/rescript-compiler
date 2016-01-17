// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Bytes = require("./bytes");
var Pervasives = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Char = require("./char");
var Caml_primitive = require("../runtime/caml_primitive");
var $$String = require("./string");

function string(str) {
  return Caml_primitive.caml_md5_string(str, 0, str.length);
}

function bytes(b) {
  return string(Bytes.unsafe_to_string(b));
}

function substring(str, ofs, len) {
  return ofs < 0 || len < 0 || ofs > str.length - len ? Pervasives.invalid_arg("Digest.substring") : Caml_primitive.caml_md5_string(str, ofs, len);
}

function subbytes(b, ofs, len) {
  return substring(Bytes.unsafe_to_string(b), ofs, len);
}

function file(filename) {
  var ic = Pervasives.open_in_bin(filename);
  var exit = 0;
  var d;
  try {
    d = Caml_primitive.caml_md5_chan(ic, -1);
    exit = 1;
  }
  catch (e){
    Pervasives.close_in(ic);
    throw e;
  }
  if (exit === 1) {
    Pervasives.close_in(ic);
    return d;
  }
  
}

function output(chan, digest) {
  return Pervasives.output_string(chan, digest);
}

function input(chan) {
  return Pervasives.really_input_string(chan, 16);
}

function char_hex(n) {
  return n + (
          n < 10 ? /* "0" */48 : /* "a" */97 - 10
        );
}

function to_hex(d) {
  var result = new Array(32);
  for(var i = 0; i<= 15; ++i){
    var x = d.charCodeAt(i);
    result[i * 2] = char_hex((x >>> 4));
    result[i * 2 + 1] = char_hex(x & 15);
  }
  return Bytes.unsafe_to_string(result);
}

function from_hex(s) {
  if (s.length !== 32) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
          "Digest.from_hex"
        ];
  }
  var digit = function (c) {
    var exit = 0;
    if (c >= 65) {
      if (c >= 97) {
        if (c >= 103) {
          exit = 1;
        }
        else {
          return c - /* "a" */97 + 10;
        }
      }
      else {
        if (c >= 71) {
          exit = 1;
        }
        else {
          return c - /* "A" */65 + 10;
        }
      }
    }
    else {
      if (9 < (-48 + c >>> 0)) {
        exit = 1;
      }
      else {
        return c - /* "0" */48;
      }
    }
    if (exit === 1) {
      throw [
            0,
            Caml_exceptions.Invalid_argument,
            "Digest.from_hex"
          ];
    }
    
  };
  var $$byte = function (i) {
    return (digit(s.charCodeAt(i)) << 4) + digit(s.charCodeAt(i + 1));
  };
  var result = new Array(16);
  for(var i = 0; i<= 15; ++i){
    result[i] = Char.chr($$byte(2 * i));
  }
  return Bytes.unsafe_to_string(result);
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
