// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function O(X) {
  let cow = x => X.foo(x);
  let sheep = x => 1 + X.foo(x) | 0;
  return {
    cow: cow,
    sheep: sheep
  };
}

function F(X, Y) {
  let cow = x => Y.foo(X.foo(x));
  let sheep = x => 1 + Y.foo(X.foo(x)) | 0;
  return {
    cow: cow,
    sheep: sheep
  };
}

function F1(X, Y) {
  let sheep = x => 1 + Y.foo(X.foo(x)) | 0;
  return {
    sheep: sheep
  };
}

function F2(X, Y) {
  let sheep = x => 1 + Y.foo(X.foo(x)) | 0;
  return {
    sheep: sheep
  };
}

let M = {
  F: ((funarg, funarg$1) => {
    let sheep = x => 1 + funarg$1.foo(funarg.foo(x)) | 0;
    return {
      sheep: sheep
    };
  })
};

exports.O = O;
exports.F = F;
exports.F1 = F1;
exports.F2 = F2;
exports.M = M;
/* No side effect */
