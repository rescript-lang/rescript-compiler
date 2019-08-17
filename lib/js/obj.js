'use strict';

var Marshal = require("./marshal.js");
var Caml_array = require("./caml_array.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function is_block(a) {
  return typeof a !== "number";
}

var double_field = Caml_array.caml_array_get;

var set_double_field = Caml_array.caml_array_set;

function marshal(obj) {
  return Caml_external_polyfill.resolve("caml_output_value_to_string")(obj, /* [] */0);
}

function unmarshal(str, pos) {
  return /* tuple */[
          Marshal.from_bytes(str, pos),
          pos + Marshal.total_size(str, pos) | 0
        ];
}

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
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Obj.extension_constructor"
        ];
  }
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

function Ephemeron_000(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_create")(prim);
}

function Ephemeron_002(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_get_key")(prim, prim$1);
}

function Ephemeron_003(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_get_key_copy")(prim, prim$1);
}

function Ephemeron_004(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("caml_ephe_set_key")(prim, prim$1, prim$2);
}

function Ephemeron_005(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_unset_key")(prim, prim$1);
}

function Ephemeron_006(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_check_key")(prim, prim$1);
}

function Ephemeron_007(prim, prim$1, prim$2, prim$3, prim$4) {
  return Caml_external_polyfill.resolve("caml_ephe_blit_key")(prim, prim$1, prim$2, prim$3, prim$4);
}

function Ephemeron_008(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_get_data")(prim);
}

function Ephemeron_009(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_get_data_copy")(prim);
}

function Ephemeron_010(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_set_data")(prim, prim$1);
}

function Ephemeron_011(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_unset_data")(prim);
}

function Ephemeron_012(prim) {
  return Caml_external_polyfill.resolve("caml_ephe_check_data")(prim);
}

function Ephemeron_013(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ephe_blit_data")(prim, prim$1);
}

var Ephemeron = [
  Ephemeron_000,
  length,
  Ephemeron_002,
  Ephemeron_003,
  Ephemeron_004,
  Ephemeron_005,
  Ephemeron_006,
  Ephemeron_007,
  Ephemeron_008,
  Ephemeron_009,
  Ephemeron_010,
  Ephemeron_011,
  Ephemeron_012,
  Ephemeron_013
];

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
exports.extension_constructor = extension_constructor;
exports.extension_name = extension_name;
exports.extension_id = extension_id;
exports.marshal = marshal;
exports.unmarshal = unmarshal;
exports.Ephemeron = Ephemeron;
/* No side effect */
