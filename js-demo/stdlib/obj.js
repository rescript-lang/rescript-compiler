// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["../runtime/caml_obj_runtime","../runtime/caml_exceptions","./pervasives","./marshal","../runtime/caml_primitive"],
  function(Caml_obj_runtime,Caml_exceptions,Pervasives,Marshal,Caml_primitive){
    'use strict';
    function double_field(x, i) {
      return x[i];
    }
    
    function set_double_field(x, i, v) {
      x[i] = v;
      return /* () */0;
    }
    
    function marshal(obj) {
      return Caml_primitive.caml_output_value_to_string(obj, /* [] */0);
    }
    
    function unmarshal(str, pos) {
      return [
              /* tuple */0,
              Marshal.from_bytes(str, pos),
              pos + Marshal.total_size(str, pos)
            ];
    }
    
    var object_tag = 248;
    
    var string_tag = 252;
    
    var custom_tag = 255;
    
    function extension_slot(x) {
      var slot = Caml_obj_runtime.caml_obj_is_block(x) && Caml_obj_runtime.caml_obj_tag(x) !== object_tag && x.length >= 1 ? x[0] : x;
      var name;
      if (Caml_obj_runtime.caml_obj_is_block(slot) && Caml_obj_runtime.caml_obj_tag(slot) === object_tag) {
        name = slot[0];
      }
      else {
        throw Caml_exceptions.Not_found;
      }
      if (Caml_obj_runtime.caml_obj_tag(name) === string_tag) {
        return slot;
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    }
    
    function extension_name(x) {
      try {
        var slot = extension_slot(x);
        return slot[0];
      }
      catch (exn){
        if (exn === Caml_exceptions.Not_found) {
          return Pervasives.invalid_arg("Obj.extension_name");
        }
        else {
          throw exn;
        }
      }
    }
    
    function extension_id(x) {
      try {
        var slot = extension_slot(x);
        return slot[1];
      }
      catch (exn){
        if (exn === Caml_exceptions.Not_found) {
          return Pervasives.invalid_arg("Obj.extension_id");
        }
        else {
          throw exn;
        }
      }
    }
    
    function extension_slot$1(x) {
      try {
        return extension_slot(x);
      }
      catch (exn){
        if (exn === Caml_exceptions.Not_found) {
          return Pervasives.invalid_arg("Obj.extension_slot");
        }
        else {
          throw exn;
        }
      }
    }
    
    var first_non_constant_constructor_tag = 0;
    
    var last_non_constant_constructor_tag = 245;
    
    var lazy_tag = 246;
    
    var closure_tag = 247;
    
    var infix_tag = 249;
    
    var forward_tag = 250;
    
    var no_scan_tag = 251;
    
    var abstract_tag = 251;
    
    var double_tag = 253;
    
    var double_array_tag = 254;
    
    var final_tag = custom_tag;
    
    var int_tag = 1000;
    
    var out_of_heap_tag = 1001;
    
    var unaligned_tag = 1002;
    return {
      double_field : double_field, 
      set_double_field : set_double_field, 
      first_non_constant_constructor_tag : first_non_constant_constructor_tag, 
      last_non_constant_constructor_tag : last_non_constant_constructor_tag, 
      lazy_tag : lazy_tag, 
      closure_tag : closure_tag, 
      object_tag : object_tag, 
      infix_tag : infix_tag, 
      forward_tag : forward_tag, 
      no_scan_tag : no_scan_tag, 
      abstract_tag : abstract_tag, 
      string_tag : string_tag, 
      double_tag : double_tag, 
      double_array_tag : double_array_tag, 
      custom_tag : custom_tag, 
      final_tag : final_tag, 
      int_tag : int_tag, 
      out_of_heap_tag : out_of_heap_tag, 
      unaligned_tag : unaligned_tag, 
      extension_name : extension_name, 
      extension_id : extension_id, 
      extension_slot : extension_slot$1, 
      marshal : marshal, 
      unmarshal : unmarshal
    }
  })
/* No side effect */
