


var id = {
  contents: 0
};

function caml_set_oo_id(b) {
  b[1] = id.contents;
  id.contents = id.contents + 1;
  return b;
}

function caml_fresh_oo_id(param) {
  id.contents = id.contents + 1;
  return id.contents;
}

function create(str) {
  var v_001 = caml_fresh_oo_id(undefined);
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

export {
  caml_set_oo_id ,
  caml_fresh_oo_id ,
  create ,
  caml_is_extension ,
  
}
/* No side effect */
