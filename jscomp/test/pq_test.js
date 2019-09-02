'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

function insert(queue, prio, elt) {
  if (queue !== "Empty") {
    var right = queue.Arg3;
    var left = queue.Arg2;
    var e = queue.Arg1;
    var p = queue.Arg0;
    if (prio <= p) {
      return /* constructor */{
              tag: "Node",
              Arg0: prio,
              Arg1: elt,
              Arg2: insert(right, p, e),
              Arg3: left
            };
    } else {
      return /* constructor */{
              tag: "Node",
              Arg0: p,
              Arg1: e,
              Arg2: insert(right, prio, elt),
              Arg3: left
            };
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: prio,
            Arg1: elt,
            Arg2: "Empty",
            Arg3: "Empty"
          };
  }
}

var Queue_is_empty = Caml_exceptions.create("Pq_test.PrioQueue.Queue_is_empty");

function remove_top(param) {
  if (param !== "Empty") {
    var left = param.Arg2;
    if (param.Arg3 !== "Empty") {
      if (left !== "Empty") {
        var right = param.Arg3;
        var rprio = right.Arg0;
        var lprio = left.Arg0;
        if (lprio <= rprio) {
          return /* constructor */{
                  tag: "Node",
                  Arg0: lprio,
                  Arg1: left.Arg1,
                  Arg2: remove_top(left),
                  Arg3: right
                };
        } else {
          return /* constructor */{
                  tag: "Node",
                  Arg0: rprio,
                  Arg1: right.Arg1,
                  Arg2: left,
                  Arg3: remove_top(right)
                };
        }
      } else {
        return param.Arg3;
      }
    } else {
      return left;
    }
  } else {
    throw Queue_is_empty;
  }
}

function extract(queue) {
  if (queue !== "Empty") {
    return /* tuple */[
            queue.Arg0,
            queue.Arg1,
            remove_top(queue)
          ];
  } else {
    throw Queue_is_empty;
  }
}

var PrioQueue = {
  empty: "Empty",
  insert: insert,
  Queue_is_empty: Queue_is_empty,
  remove_top: remove_top,
  extract: extract
};

exports.PrioQueue = PrioQueue;
/* No side effect */
