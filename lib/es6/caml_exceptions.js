


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

function is_extension(any) {
  if (any) {
    return typeof any.RE_EXN_ID === "string";
  } else {
    return false;
  }
}

function exn_slot_name(x) {
  return x.RE_EXN_ID;
}

export {
  create,
  is_extension,
  exn_slot_name,
}
/* No side effect */
