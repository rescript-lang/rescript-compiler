// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Curry = require("../../lib/js/curry.js");

function map(f, x) {
  if (!x) {
    return /* [] */0;
  }
  let r = Curry._1(f, x.hd);
  return {
    hd: r,
    tl: map(f, x.tl)
  };
}

exports.map = map;
/* No side effect */
