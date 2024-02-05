'use strict';


var idMap = new Map();

function create(str) {
  var v = idMap.get(str);
  var id;
  if (v !== undefined) {
    var id$1 = v + 1 | 0;
    idMap.set(str, id$1);
    id = id$1;
  } else {
    idMap.set(str, 1);
    id = 1;
  }
  return str + ("/" + id);
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
/* idMap Not a pure module */
