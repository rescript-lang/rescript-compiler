// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.2 , PLEASE EDIT WITH CARE
'use strict';

var Caml_sys = require("../caml_sys");

function is_directory_no_exn(f) {
  try {
    return Caml_sys.caml_sys_is_directory(f);
  }
  catch (exn){
    return /* false */0;
  }
}

exports.is_directory_no_exn = is_directory_no_exn;
/* No side effect */
