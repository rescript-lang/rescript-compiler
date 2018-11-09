'use strict';

var Marshal = require("./marshal.js");
var Caml_array = require("./caml_array.js");
var Caml_missing_polyfill = require("./caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function is_block(a) {
  return typeof a !== "number";
}

var double_field = Caml_array.caml_array_get;

var set_double_field = Caml_array.caml_array_set;

function marshal(obj) {
  return Caml_missing_polyfill.not_implemented("caml_output_value_to_string");
}

function unmarshal(str, pos) {
  return /* tuple */[
          Marshal.from_bytes(str, pos),
          pos + Marshal.total_size(str, pos) | 0
        ];
}

function extension_slot(x) {
  var slot = typeof x !== "number" && (x.tag | 0) !== 248 && x.length >= 1 ? x[0] : x;
  var name;
  if (typeof slot !== "number" && slot.tag === 248) {
    name = slot[0];
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
  if (name.tag === 252) {
    return slot;
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function extension_name(x) {
  try {
    var slot = extension_slot(x);
    return slot[0];
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Obj.extension_name"
          ];
    } else {
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
    if (exn === Caml_builtin_exceptions.not_found) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Obj.extension_id"
          ];
    } else {
      throw exn;
    }
  }
}

function extension_slot$1(x) {
  try {
    return extension_slot(x);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Obj.extension_slot"
          ];
    } else {
      throw exn;
    }
  }
}

var first_non_constant_constructor_tag = 0;

var last_non_constant_constructor_tag = 245;

var lazy_tag = 246;

var closure_tag = 247;

var object_tag = 248;

var infix_tag = 249;

var forward_tag = 250;

var no_scan_tag = 251;

var abstract_tag = 251;

var string_tag = 252;

var double_tag = 253;

var double_array_tag = 254;

var custom_tag = 255;

var final_tag = 255;

var int_tag = 1000;

var out_of_heap_tag = 1001;

var unaligned_tag = 1002;

exports.is_block = is_block;
exports.double_field = double_field;
exports.set_double_field = set_double_field;
exports.first_non_constant_constructor_tag = first_non_constant_constructor_tag;
exports.last_non_constant_constructor_tag = last_non_constant_constructor_tag;
exports.lazy_tag = lazy_tag;
exports.closure_tag = closure_tag;
exports.object_tag = object_tag;
exports.infix_tag = infix_tag;
exports.forward_tag = forward_tag;
exports.no_scan_tag = no_scan_tag;
exports.abstract_tag = abstract_tag;
exports.string_tag = string_tag;
exports.double_tag = double_tag;
exports.double_array_tag = double_array_tag;
exports.custom_tag = custom_tag;
exports.final_tag = final_tag;
exports.int_tag = int_tag;
exports.out_of_heap_tag = out_of_heap_tag;
exports.unaligned_tag = unaligned_tag;
exports.extension_name = extension_name;
exports.extension_id = extension_id;
exports.extension_slot = extension_slot$1;
exports.marshal = marshal;
exports.unmarshal = unmarshal;
/* No side effect */
