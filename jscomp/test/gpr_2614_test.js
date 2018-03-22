'use strict';

var Js_primitive = require("../../lib/js/js_primitive.js");

var v = {
  "Content-Type": 3,
  l: 2,
  open: 2
};

v["Content-Type"];

var b = v.l;

var c = v.open;

function ff() {
  v["Content-Type"] = 3;
  v.l = 2;
  return /* () */0;
}

var partial_arg = /* Some */["x"];

function h0() {
  var tmp = {
    hi: 2
  };
  if (partial_arg) {
    tmp["lo-x"] = partial_arg[0];
  }
  return tmp;
}

var h1 = {
  "lo-x": "x",
  hi: 2
};

var h2 = {
  hi: 2
};

function hh(x) {
  x["lo-x"] = "3";
  return Js_primitive.undefined_to_opt(x["lo-x"]);
}

function hh2(x) {
  var match = x["lo-x"];
  if (match !== undefined) {
    return 1;
  } else {
    return 0;
  }
}

var u = {
  "xx-yy": 3
};

var match = u["xx-yy"];

var v$1 = match !== undefined ? match : 0;

exports.b = b;
exports.c = c;
exports.ff = ff;
exports.h0 = h0;
exports.h1 = h1;
exports.h2 = h2;
exports.hh = hh;
exports.hh2 = hh2;
exports.u = u;
exports.v = v$1;
/*  Not a pure module */
