'use strict';


var $$null = null;

function create() {
  return /* record */[
          /* length */0,
          /* first */$$null,
          /* last */$$null
        ];
}

function clear(q) {
  q[/* length */0] = 0;
  q[/* first */1] = $$null;
  q[/* last */2] = $$null;
  return /* () */0;
}

function add(x, q) {
  var cell = {
    content: x,
    next: $$null
  };
  var match = q[/* last */2];
  if (match !== null) {
    q[/* length */0] = q[/* length */0] + 1 | 0;
    match.next = cell;
    q[/* last */2] = cell;
    return /* () */0;
  } else {
    q[/* length */0] = 1;
    q[/* first */1] = cell;
    q[/* last */2] = cell;
    return /* () */0;
  }
}

function peekOpt(q) {
  var match = q[/* first */1];
  if (match !== null) {
    return /* Some */[match.content];
  } else {
    return /* None */0;
  }
}

function popOpt(q) {
  var match = q[/* first */1];
  if (match !== null) {
    var next = match.next;
    if (next === null) {
      clear(q);
      return /* Some */[match.content];
    } else {
      q[/* length */0] = q[/* length */0] - 1 | 0;
      q[/* first */1] = next;
      return /* Some */[match.content];
    }
  } else {
    return /* None */0;
  }
}

function copy(q) {
  var q_res = /* record */[
    /* length */q[/* length */0],
    /* first */$$null,
    /* last */$$null
  ];
  var _prev = $$null;
  var _cell = q[/* first */1];
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== null) {
      var content = cell.content;
      var res = {
        content: content,
        next: $$null
      };
      if (prev !== null) {
        prev.next = res;
      } else {
        q_res[/* first */1] = res;
      }
      _cell = cell.next;
      _prev = res;
      continue ;
      
    } else {
      q_res[/* last */2] = prev;
      return q_res;
    }
  };
}

function isEmpty(q) {
  return +(q[/* length */0] === 0);
}

function length(q) {
  return q[/* length */0];
}

function iter(f, q) {
  var f$1 = f;
  var _cell = q[/* first */1];
  while(true) {
    var cell = _cell;
    if (cell !== null) {
      f$1(cell.content);
      _cell = cell.next;
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
    if (cell !== null) {
      var accu$2 = f$1(accu$1, cell.content);
      _cell = cell.next;
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
    if (match !== null) {
      q2[/* length */0] = q2[/* length */0] + q1[/* length */0] | 0;
      match.next = q1[/* first */1];
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

exports.create   = create;
exports.add      = add;
exports.push     = push;
exports.popOpt   = popOpt;
exports.peekOpt  = peekOpt;
exports.clear    = clear;
exports.copy     = copy;
exports.isEmpty  = isEmpty;
exports.length   = length;
exports.iter     = iter;
exports.fold     = fold;
exports.transfer = transfer;
/* null Not a pure module */
