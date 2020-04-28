'use strict';

var Arg = require("../../lib/js/arg.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

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

function with_file_as_pp(filename, f) {
  return $$finally(Pervasives.open_out_bin(filename), Pervasives.close_out, (function (chan) {
                var fmt = Format.formatter_of_out_channel(chan);
                var v = Curry._1(f, fmt);
                Format.pp_print_flush(fmt, undefined);
                return v;
              }));
}

function is_pos_pow(n) {
  var E = Caml_exceptions.create("E");
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
            CamlExt: E
          };
    };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt.CamlId === E.CamlId) {
      return -1;
    }
    throw exn;
  }
}

function failwithf(loc, fmt) {
  return Format.ksprintf((function (s) {
                var s$1 = loc + s;
                throw {
                      CamlExt: Caml_builtin_exceptions.failure,
                      _1: s$1
                    };
              }), fmt);
}

function invalid_argf(fmt) {
  return Format.ksprintf(Pervasives.invalid_arg, fmt);
}

function bad_argf(fmt) {
  return Format.ksprintf((function (x) {
                throw {
                      CamlExt: Arg.Bad,
                      _1: x
                    };
              }), fmt);
}

function hash_variant(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    accu = Caml_int32.imul(223, accu) + Caml_string.get(s, i) | 0;
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
exports.with_file_as_pp = with_file_as_pp;
exports.is_pos_pow = is_pos_pow;
exports.failwithf = failwithf;
exports.invalid_argf = invalid_argf;
exports.bad_argf = bad_argf;
exports.hash_variant = hash_variant;
/* Format Not a pure module */
