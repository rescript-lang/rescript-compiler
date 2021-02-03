'use strict';

var Curry = require("./curry.js");
var Caml_exceptions = require("./caml_exceptions.js");

var Empty = /* @__PURE__ */Caml_exceptions.create("Queue.Empty");

function create(param) {
  return {
          length: 0,
          first: /* Nil */0,
          last: /* Nil */0
        };
}

function clear(q) {
  q.length = 0;
  q.first = /* Nil */0;
  q.last = /* Nil */0;
  
}

function add(x, q) {
  var cell = /* Cons */{
    content: x,
    next: /* Nil */0
  };
  var last = q.last;
  if (last) {
    q.length = q.length + 1 | 0;
    last.next = cell;
    q.last = cell;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
  }
  
}

function peek(q) {
  var match = q.first;
  if (match) {
    return match.content;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function take(q) {
  var match = q.first;
  if (match) {
    var content = match.content;
    var next = match.next;
    if (next) {
      q.length = q.length - 1 | 0;
      q.first = next;
      return content;
    } else {
      clear(q);
      return content;
    }
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function copy(q) {
  var q_res = {
    length: q.length,
    first: /* Nil */0,
    last: /* Nil */0
  };
  var _prev = /* Nil */0;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell) {
      var next = cell.next;
      var res = /* Cons */{
        content: cell.content,
        next: /* Nil */0
      };
      if (prev) {
        prev.next = res;
      } else {
        q_res.first = res;
      }
      _cell = next;
      _prev = res;
      continue ;
    }
    q_res.last = prev;
    return q_res;
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
    if (!cell) {
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
    if (!cell) {
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
  if (last) {
    q2.length = q2.length + q1.length | 0;
    last.next = q1.first;
    q2.last = q1.last;
    return clear(q1);
  } else {
    q2.length = q1.length;
    q2.first = q1.first;
    q2.last = q1.last;
    return clear(q1);
  }
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
