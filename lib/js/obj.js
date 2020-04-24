'use strict';

var Block = require("./block.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function is_block(a) {
  return typeof a !== "number";
}

var double_field = Caml_array.caml_array_get;

var set_double_field = Caml_array.caml_array_set;

function extension_constructor(x) {
  var slot = typeof x !== "number" && (x.tag | 0) !== 248 && x.length >= 1 ? x[0] : x;
  var name;
  if (typeof slot !== "number" && slot.tag === 248) {
    name = slot[0];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Obj.extension_constructor"
        ];
  }
  if (name.tag === 252) {
    return slot;
  }
  throw [
        Caml_builtin_exceptions.invalid_argument,
        "Obj.extension_constructor"
      ];
}

function extension_name(slot) {
  return slot[0];
}

function extension_id(slot) {
  return slot[1];
}

function length(x) {
  return x.length - 2 | 0;
}

function new_object_tag_block(size) {
  return Caml_obj.caml_obj_block(248, size);
}

function new_lazy_tag_block() {
  return /* obj_block */Block.__(246, [0]);
}

var first_non_constant_constructor_tag = 0;

var last_non_constant_constructor_tag = 245;

var object_tag = 248;

var infix_tag = 249;

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

function Ephemeron_create(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_create")(prim);
}

function Ephemeron_get_key(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_get_key")(prim, prim$1);
}

function Ephemeron_get_key_copy(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_get_key_copy")(prim, prim$1);
}

function Ephemeron_set_key(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("caml_ephe_set_key")(prim, prim$1, prim$2);
}

function Ephemeron_unset_key(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_unset_key")(prim, prim$1);
}

function Ephemeron_check_key(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_check_key")(prim, prim$1);
}

function Ephemeron_blit_key(prim, prim$1, prim$2, prim$3, prim$4) {
  return Caml_external_polyfill.resolve("caml_ephe_blit_key")(prim, prim$1, prim$2, prim$3, prim$4);
}

function Ephemeron_get_data(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_get_data")(prim);
}

function Ephemeron_get_data_copy(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_get_data_copy")(prim);
}

function Ephemeron_set_data(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_set_data")(prim, prim$1);
}

function Ephemeron_unset_data(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_unset_data")(prim);
}

function Ephemeron_check_data(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_check_data")(prim);
}

function Ephemeron_blit_data(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_blit_data")(prim, prim$1);
}

var Ephemeron = {
  create: Ephemeron_create,
  length: length,
  get_key: Ephemeron_get_key,
  get_key_copy: Ephemeron_get_key_copy,
  set_key: Ephemeron_set_key,
  unset_key: Ephemeron_unset_key,
  check_key: Ephemeron_check_key,
  blit_key: Ephemeron_blit_key,
  get_data: Ephemeron_get_data,
  get_data_copy: Ephemeron_get_data_copy,
  set_data: Ephemeron_set_data,
  unset_data: Ephemeron_unset_data,
  check_data: Ephemeron_check_data,
  blit_data: Ephemeron_blit_data
};

exports.is_block = is_block;
exports.double_field = double_field;
exports.set_double_field = set_double_field;
exports.first_non_constant_constructor_tag = first_non_constant_constructor_tag;
exports.last_non_constant_constructor_tag = last_non_constant_constructor_tag;
exports.object_tag = object_tag;
exports.infix_tag = infix_tag;
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
exports.extension_constructor = extension_constructor;
exports.extension_name = extension_name;
exports.extension_id = extension_id;
exports.Ephemeron = Ephemeron;
exports.new_object_tag_block = new_object_tag_block;
exports.new_lazy_tag_block = new_lazy_tag_block;
/* No side effect */
