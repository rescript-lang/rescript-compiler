'use strict';


function sys_getenv(s) {
  if (typeof process === "undefined" || process.env === undefined) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  var x = process.env[s];
  if (x !== undefined) {
    return x;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var os_type = (function(_){
  if(typeof process !== 'undefined' && process.platform === 'win32'){
        return "Win32"    
  }
  else {
    return "Unix"
  }
});

function sys_time(param) {
  if (typeof process === "undefined" || process.uptime === undefined) {
    return -1;
  } else {
    return process.uptime();
  }
}

var sys_getcwd = (function(param){
    if (typeof process === "undefined" || process.cwd === undefined){
      return "/"  
    }
    return process.cwd()
  });

function sys_get_argv(param) {
  if (typeof process === "undefined") {
    return [
            "",
            [""]
          ];
  }
  var argv = process.argv;
  if (argv == null) {
    return [
            "",
            [""]
          ];
  } else {
    return [
            argv[0],
            argv
          ];
  }
}

function sys_exit(exit_code) {
  if (typeof process !== "undefined") {
    return process.exit(exit_code);
  }
  
}

function sys_is_directory(_s) {
  throw {
        RE_EXN_ID: "Failure",
        _1: "sys_is_directory not implemented",
        Error: new Error()
      };
}

function sys_file_exists(_s) {
  throw {
        RE_EXN_ID: "Failure",
        _1: "sys_file_exists not implemented",
        Error: new Error()
      };
}

exports.sys_getenv = sys_getenv;
exports.sys_time = sys_time;
exports.os_type = os_type;
exports.sys_getcwd = sys_getcwd;
exports.sys_get_argv = sys_get_argv;
exports.sys_exit = sys_exit;
exports.sys_is_directory = sys_is_directory;
exports.sys_file_exists = sys_file_exists;
/* No side effect */
