// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["../runtime/caml_obj_runtime","./obj"],
  function(Caml_obj_runtime,Obj){
    'use strict';
    function register(_, _$1) {
      return /* () */0;
    }
    
    function register_exception(_, exn) {
      Caml_obj_runtime.caml_obj_tag(exn) === Obj.object_tag ? exn : exn[0];
      return /* () */0;
    }
    return {
      register : register, 
      register_exception : register_exception
    }
  })
/* No side effect */
