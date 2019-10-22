'use strict';

var List = require("./list.js");
var Caml_exceptions = require("./caml_exceptions.js");

var Empty = Caml_exceptions.create("Stack.Empty");

function create(param) {
  return /* record */[
          /* c : [] */0,
          /* len */0
        ];
}

function clear(s) {
  s[/* c */0] = /* [] */0;
  s[/* len */1] = 0;
  return /* () */0;
}

function copy(s) {
  return /* record */[
          /* c */s[/* c */0],
          /* len */s[/* len */1]
        ];
}

function push(x, s) {
  s[/* c */0] = /* :: */[
    x,
    s[/* c */0]
  ];
  s[/* len */1] = s[/* len */1] + 1 | 0;
  return /* () */0;
}

function pop(s) {
  var match = s[/* c */0];
  if (match) {
    s[/* c */0] = match[1];
    s[/* len */1] = s[/* len */1] - 1 | 0;
    return match[0];
  } else {
    throw Empty;
  }
}

function top(s) {
  var match = s[/* c */0];
  if (match) {
    return match[0];
  } else {
    throw Empty;
  }
}

function is_empty(s) {
  return s[/* c */0] === /* [] */0;
}

function length(s) {
  return s[/* len */1];
}

function iter(f, s) {
  return List.iter(f, s[/* c */0]);
}

function fold(f, acc, s) {
  return List.fold_left(f, acc, s[/* c */0]);
}

exports.Empty = Empty;
exports.create = create;
exports.push = push;
exports.pop = pop;
exports.top = top;
exports.clear = clear;
exports.copy = copy;
exports.is_empty = is_empty;
exports.length = length;
exports.iter = iter;
exports.fold = fold;
/* No side effect */
