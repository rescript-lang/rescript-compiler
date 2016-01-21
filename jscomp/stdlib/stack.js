// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_exceptions = require("../runtime/caml_exceptions");
var List            = require("./list");

var Empty = [
  248,
  "Stack.Empty",
  ++ Caml_exceptions.caml_oo_last_id
];

function create() {
  return [
          /* record */0,
          /* [] */0
        ];
}

function clear(s) {
  s[1] = /* [] */0;
  return /* () */0;
}

function copy(s) {
  return [
          /* record */0,
          s[1]
        ];
}

function push(x, s) {
  s[1] = [
    /* :: */0,
    x,
    s[1]
  ];
  return /* () */0;
}

function pop(s) {
  var match = s[1];
  if (match) {
    s[1] = match[2];
    return match[1];
  }
  else {
    throw Empty;
  }
}

function top(s) {
  var match = s[1];
  if (match) {
    return match[1];
  }
  else {
    throw Empty;
  }
}

function is_empty(s) {
  return +(s[1] === /* [] */0);
}

function length(s) {
  return List.length(s[1]);
}

function iter(f, s) {
  return List.iter(f, s[1]);
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
