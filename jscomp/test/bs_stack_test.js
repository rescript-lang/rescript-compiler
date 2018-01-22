'use strict';

var Js_null = require("../../lib/js/js_null.js");
var Bs_Queue = require("../../lib/js/bs_Queue.js");
var Bs_Stack = require("../../lib/js/bs_Stack.js");

function inOrder(v) {
  var current = v;
  var s = {
    root: null
  };
  var q = Bs_Queue.create(/* () */0);
  while(current !== null) {
    var v$1 = current;
    Bs_Stack.push(s, v$1);
    current = v$1.left;
  };
  while(s.root !== null) {
    current = Bs_Stack.popNull(s);
    var v$2 = current;
    Bs_Queue.addDone(q, v$2.value);
    current = v$2.right;
    while(current !== null) {
      var v$3 = current;
      Bs_Stack.push(s, v$3);
      current = v$3.left;
    };
  };
  return Bs_Queue.toArray(q);
}

function inOrder3(v) {
  var current = v;
  var s = {
    root: null
  };
  var q = Bs_Queue.create(/* () */0);
  while(current !== null) {
    var v$1 = current;
    Bs_Stack.push(s, v$1);
    current = v$1.left;
  };
  Bs_Stack.dynamicPopIter(s, (function (popped) {
          Bs_Queue.addDone(q, popped.value);
          var current = popped.right;
          while(current !== null) {
            var v = current;
            Bs_Stack.push(s, v);
            current = v.left;
          };
          return /* () */0;
        }));
  return Bs_Queue.toArray(q);
}

function inOrder2(v) {
  var todo = /* true */1;
  var cursor = v;
  var s = {
    root: null
  };
  var q = Bs_Queue.create(/* () */0);
  while(todo) {
    if (cursor !== null) {
      var v$1 = cursor;
      Bs_Stack.push(s, v$1);
      cursor = v$1.left;
    } else if (s.root !== null) {
      cursor = Bs_Stack.popNull(s);
      var current = cursor;
      Bs_Queue.addDone(q, current.value);
      cursor = current.right;
    } else {
      todo = /* false */0;
    }
  };
  return /* () */0;
}

function n(l, r, a) {
  return {
          value: a,
          left: Js_null.fromOption(l),
          right: Js_null.fromOption(r)
        };
}

var test1 = n(/* Some */[n(/* Some */[n(/* None */0, /* None */0, 4)], /* Some */[n(/* None */0, /* None */0, 5)], 2)], /* Some */[n(/* None */0, /* None */0, 3)], 1);

function pushAllLeft(st1, s1) {
  var current = st1;
  while(current !== null) {
    var v = current;
    Bs_Stack.push(s1, v);
    current = v.left;
  };
  return /* () */0;
}

var test2 = n(/* Some */[n(/* Some */[n(/* Some */[n(/* Some */[n(/* None */0, /* None */0, 4)], /* None */0, 2)], /* None */0, 5)], /* None */0, 1)], /* None */0, 3);

var test3 = n(/* Some */[n(/* Some */[n(/* Some */[n(/* None */0, /* None */0, 4)], /* None */0, 2)], /* None */0, 5)], /* Some */[n(/* None */0, /* None */0, 3)], 1);

console.log(inOrder(test1));

console.log(inOrder3(test1));

var S = 0;

var Q = 0;

exports.S = S;
exports.Q = Q;
exports.inOrder = inOrder;
exports.inOrder3 = inOrder3;
exports.inOrder2 = inOrder2;
exports.n = n;
exports.test1 = test1;
exports.pushAllLeft = pushAllLeft;
exports.test2 = test2;
exports.test3 = test3;
/* test1 Not a pure module */
