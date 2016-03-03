// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var List                    = require("./list");

var Empty = {
  0: "Stack.Empty",
  1: Caml_builtin_exceptions.get_id(),
  length: 2,
  tag: 248
};

function create() {
  return /* record */[/* [] */0];
}

function clear(s) {
  s[0] = /* [] */0;
  return /* () */0;
}

function copy(s) {
  return /* record */[s[0]];
}

function push(x, s) {
  s[0] = /* :: */[
    x,
    s[0]
  ];
  return /* () */0;
}

function pop(s) {
  var match = s[0];
  if (match) {
    s[0] = match[1];
    return match[0];
  }
  else {
    throw Empty;
  }
}

function top(s) {
  var match = s[0];
  if (match) {
    return match[0];
  }
  else {
    throw Empty;
  }
}

function is_empty(s) {
  return +(s[0] === /* [] */0);
}

function length(s) {
  return List.length(s[0]);
}

function iter(f, s) {
  return List.iter(f, s[0]);
}

exports.Empty    = Empty;
exports.create   = create;
exports.push     = push;
exports.pop      = pop;
exports.top      = top;
exports.clear    = clear;
exports.copy     = copy;
exports.is_empty = is_empty;
exports.length   = length;
exports.iter     = iter;
/* No side effect */
