'use strict';

var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function $$finally(v, action, f) {
  var e;
  try {
    e = Curry._1(f, v);
  }
  catch (e$1){
    Curry._1(action, v);
    throw e$1;
  }
  Curry._1(action, v);
  return e;
}

function with_file_as_chan(filename, f) {
  return $$finally(Pervasives.open_out_bin(filename), Pervasives.close_out, f);
}

function is_pos_pow(n) {
  var E = /* @__PURE__ */Caml_exceptions.create("E");
  try {
    var _c = 0;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var c = _c;
      if (n$1 <= 0) {
        return -2;
      }
      if (n$1 === 1) {
        return c;
      }
      if ((n$1 & 1) === 0) {
        _n = (n$1 >> 1);
        _c = c + 1 | 0;
        continue ;
      }
      throw {
            RE_EXN_ID: E,
            Error: new Error()
          };
    };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === E) {
      return -1;
    }
    throw exn;
  }
}

function hash_variant(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    accu = Math.imul(223, accu) + Caml_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

exports.$$finally = $$finally;
exports.with_file_as_chan = with_file_as_chan;
exports.is_pos_pow = is_pos_pow;
exports.hash_variant = hash_variant;
/* No side effect */
