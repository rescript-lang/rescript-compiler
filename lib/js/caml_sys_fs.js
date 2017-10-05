'use strict';

var Fs                      = require("fs");
var Curry                   = require("./curry.js");
var Js_exn                  = require("./js_exn.js");
var Caml_obj                = require("./caml_obj.js");
var Js_primitive            = require("./js_primitive.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function caml_sys_is_directory(p) {
  try {
    return +Fs.statSync(p).isDirectory();
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      if (Caml_obj.caml_equal(/* Some */["ENOENT"], Js_primitive.undefined_to_opt(exn[1].code))) {
        throw [
              Caml_builtin_exceptions.sys_error,
              p + ": No such file or directory"
            ];
      } else {
        throw exn;
      }
    } else {
      throw exn;
    }
  }
}

function caml_sys_file_exists(p) {
  return +Fs.existsSync(p);
}

function int_of_open_flag(param) {
  switch (param) {
    case 0 : 
        return Fs.constants.O_RDONLY;
    case 1 : 
        return Fs.constants.O_WRONLY;
    case 2 : 
        return Fs.constants.O_APPEND;
    case 3 : 
        return Fs.constants.O_CREAT;
    case 4 : 
        return Fs.constants.O_TRUNC;
    case 5 : 
        return Fs.constants.O_EXCL;
    case 6 : 
    case 7 : 
        return 0;
    case 8 : 
        return Fs.constants.O_NONBLOCK;
    
  }
}

var int_of_open_flags = Curry._1((
  function (int_of_open_flag) {
    return function (flags) {
      var res = 0;
      while (flags instanceof Array) {
        res |= int_of_open_flag(flags[0]);
        flags = flags[1];
      }
      return res;
    };
  }
), int_of_open_flag);

function caml_sys_open(file, flags, mode) {
  return Fs.openSync(file, Curry._1(int_of_open_flags, flags), mode);
}

exports.caml_sys_is_directory = caml_sys_is_directory;
exports.caml_sys_file_exists  = caml_sys_file_exists;
exports.caml_sys_open         = caml_sys_open;
/* int_of_open_flags Not a pure module */
