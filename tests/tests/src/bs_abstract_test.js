// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


let v = {
  hd: 3,
  tl: null
};

v.tl = v;

let f = {
  k: (x, y) => x === y,
  y: "x"
};

function uf(u) {
  return u.y0(1);
}

function uf1(u) {
  return extra => u.y1(1, extra);
}

function uf2(u) {
  return u.y1(1, 2);
}

function uff(f) {
  return f.yyyy(1);
}

function uff2(f) {
  return f.yyyy1(1, 2);
}

function uff3(f) {
  let x = f.yyyy2;
  if (x !== undefined) {
    return x(0);
  } else {
    return 0;
  }
}

function fx(v) {
  return v.x;
}

exports.f = f;
exports.uf = uf;
exports.uf1 = uf1;
exports.uf2 = uf2;
exports.uff = uff;
exports.uff2 = uff2;
exports.uff3 = uff3;
exports.fx = fx;
/*  Not a pure module */
