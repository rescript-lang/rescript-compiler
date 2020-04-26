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
  var v_001 = id.contents;
  var v = /* tuple */[
    str,
    v_001
  ];
  v.tag = 248;
  return v;
}

function caml_is_extension(e) {
  if (e === undefined) {
    return false;
  }
  if (e.tag === 248) {
    return true;
  }
  var slot = e[0];
  if (slot !== undefined) {
    return slot.tag === 248;
  } else {
    return false;
  }
}

exports.caml_set_oo_id = caml_set_oo_id;
exports.create = create;
exports.caml_is_extension = caml_is_extension;
/* No side effect */
