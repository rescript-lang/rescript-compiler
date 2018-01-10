'use strict';


function create() {
  return /* record */[
          /* length */0,
          /* tail : None */0
        ];
}

function push(x, q) {
  if (q[/* length */0] === 0) {
    var cell = [];
    cell[0] = x;
    cell[1] = cell;
    q[/* length */0] = 1;
    q[/* tail */1] = cell;
    return /* () */0;
  } else {
    var tail = q[/* tail */1];
    var head = tail[/* next */1];
    var cell$1 = /* record */[
      /* content */x,
      /* next */head
    ];
    q[/* length */0] = q[/* length */0] + 1 | 0;
    tail[/* next */1] = cell$1;
    q[/* tail */1] = cell$1;
    return /* () */0;
  }
}

function unsafe_pop(q) {
  q[/* length */0] = q[/* length */0] - 1 | 0;
  var tail = q[/* tail */1];
  var head = tail[/* next */1];
  if (head === tail) {
    q[/* tail */1] = /* None */0;
  } else {
    tail[/* next */1] = head[/* next */1];
  }
  return head[/* content */0];
}

function is_empty(q) {
  return q[/* length */0] === 0;
}

exports.create = create;
exports.push = push;
exports.unsafe_pop = unsafe_pop;
exports.is_empty = is_empty;
/* No side effect */
