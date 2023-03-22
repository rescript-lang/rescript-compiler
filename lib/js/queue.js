'use strict';

var Curry = require("./curry.js");
var Caml_exceptions = require("./caml_exceptions.js");

var Empty = /* @__PURE__ */Caml_exceptions.create("Queue.Empty");

function create(param) {
  return {
          length: 0,
          first: "Nil",
          last: "Nil"
        };
}

function clear(q) {
  q.length = 0;
  q.first = "Nil";
  q.last = "Nil";
}

function add(x, q) {
  var cell = {
    TAG: "Cons",
    content: x,
    next: "Nil"
  };
  var last = q.last;
  if (typeof last !== "object") {
    q.length = 1;
    q.first = cell;
    q.last = cell;
    return ;
  }
  q.length = q.length + 1 | 0;
  last.next = cell;
  q.last = cell;
}

function peek(q) {
  var match = q.first;
  if (typeof match === "object") {
    return match.content;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function take(q) {
  var match = q.first;
  if (typeof match !== "object") {
    throw {
          RE_EXN_ID: Empty,
          Error: new Error()
        };
  }
  var content = match.content;
  var next = match.next;
  if (typeof next !== "object") {
    clear(q);
    return content;
  }
  q.length = q.length - 1 | 0;
  q.first = next;
  return content;
}

function copy(q) {
  var q_res = {
    length: q.length,
    first: "Nil",
    last: "Nil"
  };
  var _prev = "Nil";
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (typeof cell !== "object") {
      q_res.last = prev;
      return q_res;
    }
    var next = cell.next;
    var res = {
      TAG: "Cons",
      content: cell.content,
      next: "Nil"
    };
    if (typeof prev !== "object") {
      q_res.first = res;
    } else {
      prev.next = res;
    }
    _cell = next;
    _prev = res;
    continue ;
  };
}

function is_empty(q) {
  return q.length === 0;
}

function length(q) {
  return q.length;
}

function iter(f, q) {
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    if (typeof cell !== "object") {
      return ;
    }
    var next = cell.next;
    Curry._1(f, cell.content);
    _cell = next;
    continue ;
  };
}

function fold(f, accu, q) {
  var _accu = accu;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var accu$1 = _accu;
    if (typeof cell !== "object") {
      return accu$1;
    }
    var next = cell.next;
    var accu$2 = Curry._2(f, accu$1, cell.content);
    _cell = next;
    _accu = accu$2;
    continue ;
  };
}

function transfer(q1, q2) {
  if (q1.length <= 0) {
    return ;
  }
  var last = q2.last;
  if (typeof last !== "object") {
    q2.length = q1.length;
    q2.first = q1.first;
    q2.last = q1.last;
    return clear(q1);
  }
  q2.length = q2.length + q1.length | 0;
  last.next = q1.first;
  q2.last = q1.last;
  clear(q1);
}

var push = add;

var pop = take;

var top = peek;

exports.Empty = Empty;
exports.create = create;
exports.add = add;
exports.push = push;
exports.take = take;
exports.pop = pop;
exports.peek = peek;
exports.top = top;
exports.clear = clear;
exports.copy = copy;
exports.is_empty = is_empty;
exports.length = length;
exports.iter = iter;
exports.fold = fold;
exports.transfer = transfer;
/* No side effect */
