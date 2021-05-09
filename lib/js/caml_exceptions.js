'use strict';


var id = {
  contents: 0
};

function create(str) {
  id.contents = id.contents + 1 | 0;
  return str + ("/" + id.contents);
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

exports.id = id;
exports.create = create;
exports.is_extension = is_extension;
exports.exn_slot_name = exn_slot_name;
/* No side effect */
