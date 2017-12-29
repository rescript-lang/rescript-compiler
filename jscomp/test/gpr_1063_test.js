'use strict';


function bla(foo, bar, baz) {
  return foo["##"](bar, baz);
}

function bla2(foo, _, _$1) {
  return foo.bar.baz;
}

function bla3(foo, bar, baz) {
  return foo["##"](bar, baz);
}

function bla4(foo, x, y) {
  return foo.method1(x, y);
}

function bla5(foo, x, y) {
  return foo.method1(x, y);
}

exports.bla = bla;
exports.bla2 = bla2;
exports.bla3 = bla3;
exports.bla4 = bla4;
exports.bla5 = bla5;
/* No side effect */
