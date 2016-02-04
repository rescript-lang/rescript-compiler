// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Caml_exceptions  = require("../runtime/caml_exceptions");
var Pervasives       = require("./pervasives");
var Marshal          = require("./marshal");
var Caml_primitive   = require("../runtime/caml_primitive");

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

exports.double_field                       = double_field;
exports.set_double_field                   = set_double_field;
exports.first_non_constant_constructor_tag = first_non_constant_constructor_tag;
exports.last_non_constant_constructor_tag  = last_non_constant_constructor_tag;
exports.lazy_tag                           = lazy_tag;
exports.closure_tag                        = closure_tag;
exports.object_tag                         = object_tag;
exports.infix_tag                          = infix_tag;
exports.forward_tag                        = forward_tag;
exports.no_scan_tag                        = no_scan_tag;
exports.abstract_tag                       = abstract_tag;
exports.string_tag                         = string_tag;
exports.double_tag                         = double_tag;
exports.double_array_tag                   = double_array_tag;
exports.custom_tag                         = custom_tag;
exports.final_tag                          = final_tag;
exports.int_tag                            = int_tag;
exports.out_of_heap_tag                    = out_of_heap_tag;
exports.unaligned_tag                      = unaligned_tag;
exports.extension_name                     = extension_name;
exports.extension_id                       = extension_id;
exports.extension_slot                     = extension_slot$1;
exports.marshal                            = marshal;
exports.unmarshal                          = unmarshal;
/* No side effect */
