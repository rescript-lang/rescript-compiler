'use strict';


function v(displayName, _) {
  var $js = {
    test: 3,
    config: 3,
    hi: "ghos"
  };
  if (displayName) {
    $js.displayName = displayName[0];
  }
  return $js;
}

var v2 = {
  test: 3,
  config: 3,
  hi: "ghos"
};

var v3 = {
  displayName: "display",
  test: 3,
  config: 3,
  hi: "ghos"
};

function u(x) {
  return x;
}

function ff(x) {
  return x;
}

function fff(x) {
  return x;
}

function f(x) {
  return x;
}

exports.v   = v;
exports.v2  = v2;
exports.v3  = v3;
exports.u   = u;
exports.ff  = ff;
exports.fff = fff;
exports.f   = f;
/* No side effect */
