


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

export {
  id ,
  create ,
  is_extension ,
  exn_slot_name ,
  
}
/* No side effect */
