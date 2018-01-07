'use strict';


function create() {
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

function topNull(s) {
  var match = s.root;
  if (match !== null) {
    return match.head;
  } else {
    return null;
  }
}

function topOpt(s) {
  var match = s.root;
  if (match !== null) {
    return /* Some */[match.head];
  } else {
    return /* None */0;
  }
}

function isEmpty(s) {
  return +(s.root === null);
}

function popNull(s) {
  var match = s.root;
  if (match !== null) {
    s.root = match.tail;
    return match.head;
  } else {
    return null;
  }
}

function popOpt(s) {
  var match = s.root;
  if (match !== null) {
    s.root = match.tail;
    return /* Some */[match.head];
  } else {
    return /* None */0;
  }
}

function length(s) {
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

function iter(s, f) {
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

function dynamicPopIter(s, f) {
  var cursor = s.root;
  while(cursor !== null) {
    var v = cursor;
    s.root = v.tail;
    f(v.head);
    cursor = s.root;
  };
  return /* () */0;
}

exports.create = create;
exports.clear = clear;
exports.copy = copy;
exports.push = push;
exports.popNull = popNull;
exports.popOpt = popOpt;
exports.topNull = topNull;
exports.topOpt = topOpt;
exports.isEmpty = isEmpty;
exports.length = length;
exports.iter = iter;
exports.dynamicPopIter = dynamicPopIter;
/* No side effect */
