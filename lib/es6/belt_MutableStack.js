

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

function forEachU(s, f) {
  var _s = s.root;
  while(true) {
    var s$1 = _s;
    if (s$1 === undefined) {
      return ;
    }
    f(s$1.head);
    _s = s$1.tail;
    continue ;
  };
}

function forEach(s, f) {
  return forEachU(s, Curry.__1(f));
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
