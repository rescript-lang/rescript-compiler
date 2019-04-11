'use strict';

var Curry = require("./curry.js");
var Caml_exceptions = require("./caml_exceptions.js");

var Empty = Caml_exceptions.create("Queue.Empty");

function create(param) {
  return /* record */[
          /* length */0,
          /* first : Nil */0,
          /* last : Nil */0
        ];
}

function clear(q) {
  q[/* length */0] = 0;
  q[/* first */1] = /* Nil */0;
  q[/* last */2] = /* Nil */0;
  return /* () */0;
}

function add(x, q) {
  var cell = /* Cons */[
    /* content */x,
    /* next : Nil */0
  ];
  var match = q[/* last */2];
  if (match) {
    q[/* length */0] = q[/* length */0] + 1 | 0;
    match[/* next */1] = cell;
    q[/* last */2] = cell;
    return /* () */0;
  } else {
    q[/* length */0] = 1;
    q[/* first */1] = cell;
    q[/* last */2] = cell;
    return /* () */0;
  }
}

function peek(q) {
  var match = q[/* first */1];
  if (match) {
    return match[/* content */0];
  } else {
    throw Empty;
  }
}

function take(q) {
  var match = q[/* first */1];
  if (match) {
    var content = match[/* content */0];
    var next = match[/* next */1];
    if (next) {
      q[/* length */0] = q[/* length */0] - 1 | 0;
      q[/* first */1] = next;
      return content;
    } else {
      clear(q);
      return content;
    }
  } else {
    throw Empty;
  }
}

function copy(q) {
  var q_res = /* record */[
    /* length */q[/* length */0],
    /* first : Nil */0,
    /* last : Nil */0
  ];
  var _prev = /* Nil */0;
  var _cell = q[/* first */1];
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell) {
      var next = cell[/* next */1];
      var res = /* Cons */[
        /* content */cell[/* content */0],
        /* next : Nil */0
      ];
      if (prev) {
        prev[/* next */1] = res;
      } else {
        q_res[/* first */1] = res;
      }
      _cell = next;
      _prev = res;
      continue ;
    } else {
      q_res[/* last */2] = prev;
      return q_res;
    }
  };
}

function is_empty(q) {
  return q[/* length */0] === 0;
}

function length(q) {
  return q[/* length */0];
}

function iter(f, q) {
  var f$1 = f;
  var _cell = q[/* first */1];
  while(true) {
    var cell = _cell;
    if (cell) {
      var next = cell[/* next */1];
      Curry._1(f$1, cell[/* content */0]);
      _cell = next;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fold(f, accu, q) {
  var f$1 = f;
  var _accu = accu;
  var _cell = q[/* first */1];
  while(true) {
    var cell = _cell;
    var accu$1 = _accu;
    if (cell) {
      var next = cell[/* next */1];
      var accu$2 = Curry._2(f$1, accu$1, cell[/* content */0]);
      _cell = next;
      _accu = accu$2;
      continue ;
    } else {
      return accu$1;
    }
  };
}

function transfer(q1, q2) {
  if (q1[/* length */0] > 0) {
    var match = q2[/* last */2];
    if (match) {
      q2[/* length */0] = q2[/* length */0] + q1[/* length */0] | 0;
      match[/* next */1] = q1[/* first */1];
      q2[/* last */2] = q1[/* last */2];
      return clear(q1);
    } else {
      q2[/* length */0] = q1[/* length */0];
      q2[/* first */1] = q1[/* first */1];
      q2[/* last */2] = q1[/* last */2];
      return clear(q1);
    }
  } else {
    return 0;
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
