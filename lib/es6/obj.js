


function repr(prim) {
  return prim;
}

function obj(prim) {
  return prim;
}

function magic(prim) {
  return prim;
}

function tag(prim) {
  return prim.TAG;
}

function size(prim) {
  return prim.length | 0;
}

function field(prim0, prim1) {
  return prim0[prim1];
}

function set_field(prim0, prim1, prim2) {
  prim0[prim1] = prim2;
}

function dup(prim) {
  return {...prim};
}

function update_dummy(prim0, prim1) {
  Object.assign(prim0, prim1);
}

export {
  repr,
  obj,
  magic,
  tag,
  size,
  field,
  set_field,
  dup,
  update_dummy,
}
/* No side effect */
