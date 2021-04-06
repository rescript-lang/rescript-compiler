'use strict';

var Curry = require("./curry.js");
var Pervasives = require("./pervasives.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

var printers = {
  contents: /* [] */0
};

function locfmt(s, linum, start, finish, msg) {
  return "File \"" + s + "\", line " + linum + ", characters " + start + "-" + finish + ": " + msg;
}

var fields = (function(x){
  var s = "" 
  var index = 1
  while ("_"+index in x){
    s += x ["_" + index];
    ++ index
  }
  if(index === 1){
    return s 
  }
  return "(" + s + ")"
});

function to_string(x) {
  var _param = printers.contents;
  while(true) {
    var param = _param;
    if (param) {
      var s;
      try {
        s = Curry._1(param.hd, x);
      }
      catch (exn){
        s = undefined;
      }
      if (s !== undefined) {
        return s;
      }
      _param = param.tl;
      continue ;
    }
    if (x.RE_EXN_ID === "Out_of_memory") {
      return "Out of memory";
    }
    if (x.RE_EXN_ID === "Stack_overflow") {
      return "Stack overflow";
    }
    if (x.RE_EXN_ID === "Match_failure") {
      var match = x._1;
      var $$char = match[2];
      return locfmt(match[0], match[1], $$char, $$char + 5 | 0, "Pattern matching failed");
    }
    if (x.RE_EXN_ID === "Assert_failure") {
      var match$1 = x._1;
      var $$char$1 = match$1[2];
      return locfmt(match$1[0], match$1[1], $$char$1, $$char$1 + 6 | 0, "Assertion failed");
    }
    if (x.RE_EXN_ID === "Undefined_recursive_module") {
      var match$2 = x._1;
      var $$char$2 = match$2[2];
      return locfmt(match$2[0], match$2[1], $$char$2, $$char$2 + 6 | 0, "Undefined recursive module");
    }
    var constructor = Caml_exceptions.caml_exn_slot_name(x);
    return constructor + fields(x);
  };
}

function print(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    console.log("Uncaught exception: " + to_string(x));
    throw x;
  }
}

function $$catch(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    Pervasives.flush(Pervasives.stdout);
    console.log("Uncaught exception: " + to_string(x));
    return Pervasives.exit(2);
  }
}

function register_printer(fn) {
  printers.contents = {
    hd: fn,
    tl: printers.contents
  };
  
}

exports.to_string = to_string;
exports.print = print;
exports.$$catch = $$catch;
exports.register_printer = register_printer;
/* No side effect */
