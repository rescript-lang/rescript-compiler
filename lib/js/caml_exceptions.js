'use strict';


let idMap = {};

function create(str) {
  let v = idMap[str];
  if (v !== undefined) {
    let id = v + 1 | 0;
    idMap[str] = id;
    return str + ("/" + id);
  }
  idMap[str] = 1;
  return str;
}

function is_extension(e) {
  if (e == null) {
    return false;
  } else {
    return typeof e.RE_EXN_ID === "string";
  }
}

function exn_slot_name(x) {
  return x.RE_EXN_ID;
}

exports.create = create;
exports.is_extension = is_extension;
exports.exn_slot_name = exn_slot_name;
/* No side effect */
