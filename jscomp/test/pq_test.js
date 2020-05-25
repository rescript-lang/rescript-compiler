'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

function insert(queue, prio, elt) {
  if (!queue) {
    return /* Node */{
            _0: prio,
            _1: elt,
            _2: /* Empty */0,
            _3: /* Empty */0
          };
  }
  var right = queue._3;
  var left = queue._2;
  var e = queue._1;
  var p = queue._0;
  if (prio <= p) {
    return /* Node */{
            _0: prio,
            _1: elt,
            _2: insert(right, p, e),
            _3: left
          };
  } else {
    return /* Node */{
            _0: p,
            _1: e,
            _2: insert(right, prio, elt),
            _3: left
          };
  }
}

var Queue_is_empty = Caml_exceptions.create("Pq_test.PrioQueue.Queue_is_empty");

function remove_top(param) {
  if (param) {
    var left = param._2;
    if (!param._3) {
      return left;
    }
    if (!left) {
      return param._3;
    }
    var right = param._3;
    var rprio = right._0;
    var lprio = left._0;
    if (lprio <= rprio) {
      return /* Node */{
              _0: lprio,
              _1: left._1,
              _2: remove_top(left),
              _3: right
            };
    } else {
      return /* Node */{
              _0: rprio,
              _1: right._1,
              _2: left,
              _3: remove_top(right)
            };
    }
  }
  throw {
        RE_EXN_ID: Queue_is_empty,
        Error: new Error()
      };
}

function extract(queue) {
  if (queue) {
    return /* tuple */[
            queue._0,
            queue._1,
            remove_top(queue)
          ];
  }
  throw {
        RE_EXN_ID: Queue_is_empty,
        Error: new Error()
      };
}

var PrioQueue = {
  empty: /* Empty */0,
  insert: insert,
  Queue_is_empty: Queue_is_empty,
  remove_top: remove_top,
  extract: extract
};

exports.PrioQueue = PrioQueue;
/* No side effect */
