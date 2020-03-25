

import * as Marshal from "./marshal.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_external_polyfill from "./caml_external_polyfill.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

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
  if (!(typeof slot !== "number" && slot.tag === 248)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Obj.extension_constructor"
        ];
  }
  name = slot[0];
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

export {
  is_block ,
  double_field ,
  set_double_field ,
  first_non_constant_constructor_tag ,
  last_non_constant_constructor_tag ,
  lazy_tag ,
  closure_tag ,
  object_tag ,
  infix_tag ,
  forward_tag ,
  no_scan_tag ,
  abstract_tag ,
  string_tag ,
  double_tag ,
  double_array_tag ,
  custom_tag ,
  final_tag ,
  int_tag ,
  out_of_heap_tag ,
  unaligned_tag ,
  extension_constructor ,
  extension_name ,
  extension_id ,
  marshal ,
  unmarshal ,
  Ephemeron ,
  
}
/* No side effect */
