// Generated by ReScript, PLEASE EDIT WITH CARE


function u(f, a, b) {
  console.log(f(a, b));
  console.log(f(a, b));
}

function u2(f, a, b) {
  console.log(f(a, b));
  console.log(f(a, b));
}

function f(x, y) {
  return x + y | 0;
}

function add(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function h(u) {
  let m = u.hi;
  return m(1, 2);
}

let nested = {
  x: {
    y: 3
  }
};

export {
  u,
  u2,
  f,
  add,
  h,
  nested,
}
/* No side effect */
