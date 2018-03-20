'use strict';

var Curry = require("./curry.js");

function make() {
  return {
          root: null
        };
}

function clear(s) {
  s.root = null;
  return /* () */0;
}

function copy(s) {
  return {
          root: s.root
        };
}

function push(s, x) {
  s.root = {
    head: x,
    tail: s.root
  };
  return /* () */0;
}

function topUndefined(s) {
  var match = s.root;
  if (match !== null) {
    return match.head;
  } else {
    return undefined;
  }
}

function top(s) {
  var match = s.root;
  if (match !== null) {
    return /* Some */[match.head];
  } else {
    return /* None */0;
  }
}

function isEmpty(s) {
  return s.root === null;
}

function popUndefined(s) {
  var match = s.root;
  if (match !== null) {
    s.root = match.tail;
    return match.head;
  } else {
    return undefined;
  }
}

function pop(s) {
  var match = s.root;
  if (match !== null) {
    s.root = match.tail;
    return /* Some */[match.head];
  } else {
    return /* None */0;
  }
}

function size(s) {
  var match = s.root;
  if (match !== null) {
    var _x = match;
    var _acc = 0;
    while(true) {
      var acc = _acc;
      var x = _x;
      var match$1 = x.tail;
      if (match$1 !== null) {
        _acc = acc + 1 | 0;
        _x = match$1;
        continue ;
      } else {
        return acc + 1 | 0;
      }
    };
  } else {
    return 0;
  }
}

function forEachU(s, f) {
  var _s = s.root;
  var f$1 = f;
  while(true) {
    var s$1 = _s;
    if (s$1 !== null) {
      f$1(s$1.head);
      _s = s$1.tail;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEach(s, f) {
  return forEachU(s, Curry.__1(f));
}

function dynamicPopIterU(s, f) {
  var cursor = s.root;
  while(cursor !== null) {
    var v = cursor;
    s.root = v.tail;
    f(v.head);
    cursor = s.root;
  };
  return /* () */0;
}

function dynamicPopIter(s, f) {
  return dynamicPopIterU(s, Curry.__1(f));
}

exports.make = make;
exports.clear = clear;
exports.copy = copy;
exports.push = push;
exports.popUndefined = popUndefined;
exports.pop = pop;
exports.topUndefined = topUndefined;
exports.top = top;
exports.isEmpty = isEmpty;
exports.size = size;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.dynamicPopIterU = dynamicPopIterU;
exports.dynamicPopIter = dynamicPopIter;
/* No side effect */
