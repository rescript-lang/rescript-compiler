'use strict';

var Fs                      = require("fs");
var Curry                   = require("./curry.js");
var Js_exn                  = require("./js_exn.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function with_sys_error(f) {
  try {
    return Curry._1(f, /* () */0);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      var match = exn[1].message;
      var message = match !== undefined ? match : "Unknown error";
      throw [
            Caml_builtin_exceptions.sys_error,
            message
          ];
    } else {
      throw exn;
    }
  }
}

function caml_sys_is_directory(p) {
  return with_sys_error((function () {
                return +Fs.statSync(p).isDirectory();
              }));
}

function caml_sys_file_exists(p) {
  return with_sys_error((function () {
                return +Fs.existsSync(p);
              }));
}

function caml_sys_remove(path) {
  return with_sys_error((function () {
                Fs.unlinkSync(path);
                return /* () */0;
              }));
}

function fs_flag_of_open_flag(param) {
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
        return Fs.constants.O_BINARY;
    case 7 : 
        return Fs.constants.O_TEXT;
    case 8 : 
        return Fs.constants.O_NONBLOCK;
    
  }
}

function int_of_open_flag(f) {
  var match = fs_flag_of_open_flag(f);
  if (match !== undefined) {
    return match;
  } else {
    return 0;
  }
}

var go = (
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
  );

var int_of_open_flags = Curry._1(go, int_of_open_flag);

function caml_sys_open(file, flags, mode) {
  return with_sys_error((function () {
                return Fs.openSync(file, Curry._1(int_of_open_flags, flags), mode);
              }));
}

function caml_ml_open_descriptor_out(fd) {
  return /* record */[
          /* fd : Some */[fd],
          /* buffer */"",
          /* output */(function (_, s) {
              return with_sys_error((function () {
                            var to_write = s.length;
                            var written = 0;
                            var rest = s;
                            while(written < to_write) {
                              rest = s.slice(written);
                              written = written + Fs.writeSync(fd, rest, "binary") | 0;
                            };
                            return /* () */0;
                          }));
            })
        ];
}

function caml_ml_close_channel(oc) {
  return with_sys_error((function () {
                var match = oc[/* fd */0];
                if (match) {
                  Fs.closeSync(match[0]);
                  oc[/* fd */0] = /* None */0;
                  return /* () */0;
                } else {
                  return /* () */0;
                }
              }));
}

exports.caml_sys_is_directory       = caml_sys_is_directory;
exports.caml_sys_file_exists        = caml_sys_file_exists;
exports.caml_sys_open               = caml_sys_open;
exports.caml_sys_remove             = caml_sys_remove;
exports.caml_ml_open_descriptor_out = caml_ml_open_descriptor_out;
exports.caml_ml_close_channel       = caml_ml_close_channel;
/* go Not a pure module */
