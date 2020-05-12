

import * as Caml_external_polyfill from "./caml_external_polyfill.js";

function is_block(a) {
  return typeof a !== "number";
}

function length(x) {
  return x.length - 2 | 0;
}

var lazy_tag = 246;

var object_tag = 248;

var forward_tag = 250;

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
  lazy_tag ,
  object_tag ,
  forward_tag ,
  Ephemeron ,
  
}
/* No side effect */
