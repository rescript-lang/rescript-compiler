'use strict';


var id = {
  contents: 0
};

function caml_set_oo_id(b) {
  b[1] = id.contents;
  id.contents = id.contents + 1;
  return b;
}

function create(str) {
  id.contents = id.contents + 1;
  return {
          CamlId: id.contents,
          name: str
        };
}

var caml_is_extension = (function (e){
  if(e == null || e.CamlExt == null) {
    return false 
  }
  return typeof e.CamlExt.CamlId === "number"
});

function caml_exn_slot_id(x) {
  return x.CamlExt.CamlId;
}

function caml_exn_slot_name(x) {
  return x.CamlExt.name;
}

exports.caml_set_oo_id = caml_set_oo_id;
exports.create = create;
exports.caml_is_extension = caml_is_extension;
exports.caml_exn_slot_id = caml_exn_slot_id;
exports.caml_exn_slot_name = caml_exn_slot_name;
/* No side effect */
