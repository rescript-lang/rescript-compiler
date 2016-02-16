// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Caml_curry              = require("../runtime/caml_curry");

var Empty = {
  0: "Queue.Empty",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

function create() {
  return /* record */[
          0,
          /* None */0
        ];
}

function clear(q) {
  q[0] = 0;
  q[1] = /* None */0;
  return /* () */0;
}

function add(x, q) {
  if (q[0]) {
    var tail = q[1];
    var head = tail[1];
    var cell = /* record */[
      x,
      head
    ];
    ++ q[0];
    tail[1] = cell;
    q[1] = cell;
    return /* () */0;
  }
  else {
    var cell$1 = {
      
    };
    Caml_obj.caml_update_dummy(cell$1, /* record */[
          x,
          cell$1
        ]);
    q[0] = 1;
    q[1] = cell$1;
    return /* () */0;
  }
}

function peek(q) {
  if (q[0]) {
    return q[1][1][0];
  }
  else {
    throw Empty;
  }
}

function take(q) {
  if (!q[0]) {
    throw Empty;
  }
  -- q[0];
  var tail = q[1];
  var head = tail[1];
  if (head === tail) {
    q[1] = /* None */0;
  }
  else {
    tail[1] = head[1];
  }
  return head[0];
}

function copy(q) {
  if (q[0]) {
    var tail = q[1];
    var tail$prime = {
      
    };
    Caml_obj.caml_update_dummy(tail$prime, /* record */[
          tail[0],
          tail$prime
        ]);
    var copy$1 = function (_prev, _cell) {
      while(true) {
        var cell = _cell;
        var prev = _prev;
        if (cell !== tail) {
          var res = /* record */[
            cell[0],
            tail$prime
          ];
          prev[1] = res;
          _cell = cell[1];
          _prev = res;
          continue ;
          
        }
        else {
          return 0;
        }
      };
    };
    copy$1(tail$prime, tail[1]);
    return /* record */[
            q[0],
            tail$prime
          ];
  }
  else {
    return /* record */[
            0,
            /* None */0
          ];
  }
}

function is_empty(q) {
  return +(q[0] === 0);
}

function length(q) {
  return q[0];
}

function iter(f, q) {
  if (q[0] > 0) {
    var tail = q[1];
    var _cell = tail[1];
    while(true) {
      var cell = _cell;
      Caml_curry.app1(f, cell[0]);
      if (cell !== tail) {
        _cell = cell[1];
        continue ;
        
      }
      else {
        return 0;
      }
    };
  }
  else {
    return 0;
  }
}

function fold(f, accu, q) {
  if (q[0]) {
    var tail = q[1];
    var _accu = accu;
    var _cell = tail[1];
    while(true) {
      var cell = _cell;
      var accu$1 = _accu;
      var accu$2 = Caml_curry.app2(f, accu$1, cell[0]);
      if (cell === tail) {
        return accu$2;
      }
      else {
        _cell = cell[1];
        _accu = accu$2;
        continue ;
        
      }
    };
  }
  else {
    return accu;
  }
}

function transfer(q1, q2) {
  var length1 = q1[0];
  if (length1 > 0) {
    var tail1 = q1[1];
    clear(q1);
    if (q2[0] > 0) {
      var tail2 = q2[1];
      var head1 = tail1[1];
      var head2 = tail2[1];
      tail1[1] = head2;
      tail2[1] = head1;
    }
    q2[0] += length1;
    q2[1] = tail1;
    return /* () */0;
  }
  else {
    return 0;
  }
}

var push = add;

var pop = take;

var top = peek;

exports.Empty    = Empty;
exports.create   = create;
exports.add      = add;
exports.push     = push;
exports.take     = take;
exports.pop      = pop;
exports.peek     = peek;
exports.top      = top;
exports.clear    = clear;
exports.copy     = copy;
exports.is_empty = is_empty;
exports.length   = length;
exports.iter     = iter;
exports.fold     = fold;
exports.transfer = transfer;
/* No side effect */
