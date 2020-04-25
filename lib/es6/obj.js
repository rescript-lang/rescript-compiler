

import * as Caml_obj from "./caml_obj.js";
import * as Caml_external_polyfill from "./caml_external_polyfill.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

function is_block(a) {
  return typeof a !== "number";
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
  if (typeof name === "string") {
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
  extension_constructor ,
  extension_name ,
  extension_id ,
  Ephemeron ,
  new_object_tag_block ,
  
}
/* No side effect */
