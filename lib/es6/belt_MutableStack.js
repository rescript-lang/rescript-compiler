

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

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
  var match = s.root;
  if (match !== undefined) {
    return match.head;
  }
  
}

function top(s) {
  var match = s.root;
  if (match !== undefined) {
    return Caml_option.some(match.head);
  }
  
}

function isEmpty(s) {
  return s.root === undefined;
}

function popUndefined(s) {
  var match = s.root;
  if (match === undefined) {
    return ;
  }
  var x = match;
  s.root = x.tail;
  return x.head;
}

function pop(s) {
  var match = s.root;
  if (match === undefined) {
    return ;
  }
  var x = match;
  s.root = x.tail;
  return Caml_option.some(x.head);
}

function size(s) {
  var match = s.root;
  if (match !== undefined) {
    var _x = match;
    var _acc = 0;
    while(true) {
      var acc = _acc;
      var x = _x;
      var match$1 = x.tail;
      if (match$1 === undefined) {
        return acc + 1 | 0;
      }
      _acc = acc + 1 | 0;
      _x = match$1;
      continue ;
    };
  } else {
    return 0;
  }
}

function forEachU(s, f) {
  var _s = s.root;
  while(true) {
    var s$1 = _s;
    if (s$1 === undefined) {
      return ;
    }
    var x = s$1;
    f(x.head);
    _s = x.tail;
    continue ;
  };
}

function forEach(s, f) {
  return forEachU(s, Curry.__1(f));
}

function dynamicPopIterU(s, f) {
  var cursor = s.root;
  while(cursor !== undefined) {
    var v = cursor;
    s.root = v.tail;
    f(v.head);
    cursor = s.root;
  };
  
}

function dynamicPopIter(s, f) {
  return dynamicPopIterU(s, Curry.__1(f));
}

export {
  make ,
  clear ,
  copy ,
  push ,
  popUndefined ,
  pop ,
  topUndefined ,
  top ,
  isEmpty ,
  size ,
  forEachU ,
  forEach ,
  dynamicPopIterU ,
  dynamicPopIter ,
  
}
/* No side effect */
