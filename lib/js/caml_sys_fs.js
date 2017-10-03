'use strict';

var Fs                      = require("fs");
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

exports.caml_sys_is_directory = caml_sys_is_directory;
exports.caml_sys_file_exists  = caml_sys_file_exists;
/* fs Not a pure module */
