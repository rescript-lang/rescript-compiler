'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function caml_raise_not_found() {
  throw Caml_builtin_exceptions.not_found;
}


function $$caml_sys_getenv(n) {
    //nodejs env
    if (typeof process !== 'undefined'
        && process.env
        && process.env[n] != undefined){
        return process.env[n]
    }
    else{ 
     caml_raise_not_found()
    };
  }

;


function $$date(){
  return (+new Date())
};


;

var caml_initial_time = $$date() * 0.001;

function caml_sys_time() {
  return $$date() * 0.001 - caml_initial_time;
}

function caml_sys_random_seed() {
  return /* array */[(($$date() | 0) ^ 4294967295) * Math.random() | 0];
}

function caml_sys_system_command() {
  return 127;
}

function caml_sys_getcwd() {
  return "/";
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

function caml_sys_getenv(prim) {
  return $$caml_sys_getenv(prim);
}

exports.caml_raise_not_found    = caml_raise_not_found;
exports.caml_sys_getenv         = caml_sys_getenv;
exports.caml_sys_time           = caml_sys_time;
exports.caml_sys_random_seed    = caml_sys_random_seed;
exports.caml_sys_system_command = caml_sys_system_command;
exports.caml_sys_getcwd         = caml_sys_getcwd;
exports.caml_sys_is_directory   = caml_sys_is_directory;
exports.caml_sys_file_exists    = caml_sys_file_exists;
/*  Not a pure module */
