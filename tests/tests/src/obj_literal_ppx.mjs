// Generated by ReScript, PLEASE EDIT WITH CARE


let a = {
  x: 3,
  y: [1]
};

let b = {
  x: 3,
  y: [1],
  z: 3,
  u: (x, y) => x + y | 0
};

function f(obj) {
  return obj.x + obj.y.length | 0;
}

let u = f(a);

let v = f(b);

export {
  a,
  b,
  f,
  u,
  v,
}
/* u Not a pure module */
