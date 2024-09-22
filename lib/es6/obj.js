


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

function dup(prim) {
  return {...prim};
}

export {
  repr,
  obj,
  magic,
  tag,
  size,
  field,
  dup,
}
/* No side effect */
