'use strict';

var Marshal_bin = require("./marshal_bin");

function marshal(prim) {
  return Marshal_bin.marshal(prim);
}

function unmarshal(prim) {
  return Marshal_bin.unmarshal(prim);
}

function caml_read_file_content(prim) {
  return Marshal_bin.caml_read_file_content(prim);
}

var s = Marshal_bin.caml_read_file_content("./aaa.marshal");

var v = Marshal_bin.unmarshal(s);

function sum(l) {
  if (l === 0) {
    return 0;
  } else {
    return l[1] + sum(l[2]) | 0;
  }
}

console.log("v", v);

console.log("sum:", sum(v));

exports.marshal = marshal;
exports.unmarshal = unmarshal;
exports.caml_read_file_content = caml_read_file_content;
exports.s = s;
exports.v = v;
exports.sum = sum;
/* s Not a pure module */
