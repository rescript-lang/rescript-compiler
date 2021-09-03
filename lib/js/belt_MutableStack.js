'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");

function make(param) {
  return {
          root: undefined
        };
}

function clear(s) {
  s.root = undefined;
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
}

function topUndefined(s) {
  var x = s.root;
  if (x !== undefined) {
    return x.head;
  }
  
}

function top(s) {
  var x = s.root;
  if (x !== undefined) {
    return Caml_option.some(x.head);
  }
  
}

function isEmpty(s) {
  return s.root === undefined;
}

function popUndefined(s) {
  var x = s.root;
  if (x !== undefined) {
    s.root = x.tail;
    return x.head;
  }
  
}

function pop(s) {
  var x = s.root;
  if (x !== undefined) {
    s.root = x.tail;
    return Caml_option.some(x.head);
  }
  
}

function size(s) {
  var x = s.root;
  if (x !== undefined) {
    var _x = x;
    var _acc = 0;
    while(true) {
      var acc = _acc;
      var x$1 = _x;
      var x$2 = x$1.tail;
      if (x$2 === undefined) {
        return acc + 1 | 0;
      }
      _acc = acc + 1 | 0;
      _x = x$2;
      continue ;
    };
  } else {
    return 0;
  }
}

function iterAux(_s, f) {
  while(true) {
    var s = _s;
    if (s === undefined) {
      return ;
    }
    f(s.head);
    _s = s.tail;
    continue ;
  };
}

function forEachU(s, f) {
  iterAux(s.root, f);
}

function forEach(s, f) {
  forEachU(s, (function (x) {
          Curry._1(f, x);
        }));
}

function dynamicPopIterU(s, f) {
  while(true) {
    var match = s.root;
    if (match === undefined) {
      return ;
    }
    s.root = match.tail;
    f(match.head);
    continue ;
  };
}

function dynamicPopIter(s, f) {
  dynamicPopIterU(s, (function (x) {
          Curry._1(f, x);
        }));
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
