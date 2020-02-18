

import * as Curry from "./curry.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var Empty = Caml_exceptions.create("Queue.Empty");

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
  return /* () */0;
}

function add(x, q) {
  var cell = /* Cons */[
    /* content */x,
    /* next : Nil */0
  ];
  var match = q.last;
  if (match) {
    q.length = q.length + 1 | 0;
    match[/* next */1] = cell;
    q.last = cell;
    return /* () */0;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
    return /* () */0;
  }
}

function peek(q) {
  var match = q.first;
  if (match) {
    return match[/* content */0];
  } else {
    throw Empty;
  }
}

function take(q) {
  var match = q.first;
  if (match) {
    var content = match[/* content */0];
    var next = match[/* next */1];
    if (next) {
      q.length = q.length - 1 | 0;
      q.first = next;
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
      var next = cell[/* next */1];
      var res = /* Cons */[
        /* content */cell[/* content */0],
        /* next : Nil */0
      ];
      if (prev) {
        prev[/* next */1] = res;
      } else {
        q_res.first = res;
      }
      _cell = next;
      _prev = res;
      continue ;
    } else {
      q_res.last = prev;
      return q_res;
    }
  };
}

function is_empty(q) {
  return q.length === 0;
}

function length(q) {
  return q.length;
}

function iter(f, q) {
  var f$1 = f;
  var _cell = q.first;
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
  var _cell = q.first;
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
  if (q1.length > 0) {
    var match = q2.last;
    if (match) {
      q2.length = q2.length + q1.length | 0;
      match[/* next */1] = q1.first;
      q2.last = q1.last;
      return clear(q1);
    } else {
      q2.length = q1.length;
      q2.first = q1.first;
      q2.last = q1.last;
      return clear(q1);
    }
  } else {
    return 0;
  }
}

var push = add;

var pop = take;

var top = peek;

export {
  Empty ,
  create ,
  add ,
  push ,
  take ,
  pop ,
  peek ,
  top ,
  clear ,
  copy ,
  is_empty ,
  length ,
  iter ,
  fold ,
  transfer ,
  
}
/* No side effect */
