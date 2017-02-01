'use strict';

import * as List            from "./list";
import * as Caml_exceptions from "./caml_exceptions";

var Empty = Caml_exceptions.create("Stack.Empty");

function create() {
  return /* record */[/* c : [] */0];
}

function clear(s) {
  s[/* c */0] = /* [] */0;
  return /* () */0;
}

function copy(s) {
  return /* record */[/* c */s[/* c */0]];
}

function push(x, s) {
  s[/* c */0] = /* :: */[
    x,
    s[/* c */0]
  ];
  return /* () */0;
}

function pop(s) {
  var match = s[/* c */0];
  if (match) {
    s[/* c */0] = match[1];
    return match[0];
  }
  else {
    throw Empty;
  }
}

function top(s) {
  var match = s[/* c */0];
  if (match) {
    return match[0];
  }
  else {
    throw Empty;
  }
}

function is_empty(s) {
  return +(s[/* c */0] === /* [] */0);
}

function length(s) {
  return List.length(s[/* c */0]);
}

function iter(f, s) {
  return List.iter(f, s[/* c */0]);
}

export {
  Empty    ,
  create   ,
  push     ,
  pop      ,
  top      ,
  clear    ,
  copy     ,
  is_empty ,
  length   ,
  iter     ,
  
}
/* No side effect */
