// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.1 , PLEASE EDIT WITH CARE
'use strict';

var Pervasives      = require("../pervasives");
var Caml_exceptions = require("../caml_exceptions");
var Arg             = require("../arg");
var Curry           = require("../curry");
var Format          = require("../format");

function $$finally(v, f, action) {
  var exit = 0;
  var e;
  try {
    e = Curry._1(f, v);
    exit = 1;
  }
  catch (e$1){
    Curry._1(action, v);
    throw e$1;
  }
  if (exit === 1) {
    Curry._1(action, v);
    return e;
  }
  
}

function with_file_as_chan(filename, f) {
  var chan = Pervasives.open_out(filename);
  return $$finally(chan, f, Pervasives.close_out);
}

function with_file_as_pp(filename, f) {
  var chan = Pervasives.open_out(filename);
  return $$finally(chan, function (chan) {
              var fmt = Format.formatter_of_out_channel(chan);
              var v = Curry._1(f, fmt);
              Format.pp_print_flush(fmt, /* () */0);
              return v;
            }, Pervasives.close_out);
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
      else if (n$1 === 1) {
        return c;
      }
      else if (n$1 & 1) {
        throw E;
      }
      else {
        _n = (n$1 >> 1);
        _c = c + 1 | 0;
        continue ;
        
      }
    };
  }
  catch (exn){
    if (exn === E) {
      return -1;
    }
    else {
      throw exn;
    }
  }
}

function failwithf(fmt) {
  return Format.ksprintf(Pervasives.failwith, fmt);
}

function invalid_argf(fmt) {
  return Format.ksprintf(Pervasives.invalid_arg, fmt);
}

function bad_argf(fmt) {
  return Format.ksprintf(function (x) {
              throw [
                    Arg.Bad,
                    x
                  ];
            }, fmt);
}

exports.$$finally         = $$finally;
exports.with_file_as_chan = with_file_as_chan;
exports.with_file_as_pp   = with_file_as_pp;
exports.is_pos_pow        = is_pos_pow;
exports.failwithf         = failwithf;
exports.invalid_argf      = invalid_argf;
exports.bad_argf          = bad_argf;
/* Format Not a pure module */
