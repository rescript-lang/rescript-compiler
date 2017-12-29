'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function caml_sys_getenv(s) {
  var match = typeof (process) === "undefined" ? undefined : (process);
  if (match !== undefined) {
    var match$1 = match.env[s];
    if (match$1 !== undefined) {
      return match$1;
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function caml_sys_time() {
  var match = typeof (process) === "undefined" ? undefined : (process);
  if (match !== undefined) {
    return match.uptime();
  } else {
    return -1;
  }
}

function caml_sys_random_seed() {
  return /* array */[((Date.now() | 0) ^ 4294967295) * Math.random() | 0];
}

function caml_sys_system_command() {
  return 127;
}

function caml_sys_getcwd() {
  var match = typeof (process) === "undefined" ? undefined : (process);
  if (match !== undefined) {
    return match.cwd();
  } else {
    return "/";
  }
}

function caml_sys_get_argv() {
  var match = typeof (process) === "undefined" ? undefined : (process);
  if (match !== undefined) {
    if (match.argv == null) {
      return /* tuple */[
              "",
              /* array */[""]
            ];
    } else {
      return /* tuple */[
              match.argv[0],
              match.argv
            ];
    }
  } else {
    return /* tuple */[
            "",
            /* array */[""]
          ];
  }
}

function caml_sys_exit(exit_code) {
  var match = typeof (process) === "undefined" ? undefined : (process);
  if (match !== undefined) {
    return match.exit(exit_code);
  } else {
    return /* () */0;
  }
}

function caml_sys_is_directory() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_sys_is_directory not implemented"
      ];
}

function caml_sys_file_exists() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_sys_file_exists not implemented"
      ];
}

exports.caml_sys_getenv = caml_sys_getenv;
exports.caml_sys_time = caml_sys_time;
exports.caml_sys_random_seed = caml_sys_random_seed;
exports.caml_sys_system_command = caml_sys_system_command;
exports.caml_sys_getcwd = caml_sys_getcwd;
exports.caml_sys_get_argv = caml_sys_get_argv;
exports.caml_sys_exit = caml_sys_exit;
exports.caml_sys_is_directory = caml_sys_is_directory;
exports.caml_sys_file_exists = caml_sys_file_exists;
/* No side effect */
