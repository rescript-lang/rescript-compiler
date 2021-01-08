

import * as Curry from "./curry.mjs";
import * as Caml_option from "./caml_option.mjs";

function make(param) {
  return {
          length: 0,
          first: undefined,
          last: undefined
        };
}

function clear(q) {
  q.length = 0;
  q.first = undefined;
  q.last = undefined;
  
}

function add(q, x) {
  var cell = {
    content: x,
    next: undefined
  };
  var last = q.last;
  if (last !== undefined) {
    q.length = q.length + 1 | 0;
    last.next = cell;
    q.last = cell;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
  }
  
}

function peek(q) {
  var v = q.first;
  if (v !== undefined) {
    return Caml_option.some(v.content);
  }
  
}

function peekUndefined(q) {
  var v = q.first;
  if (v !== undefined) {
    return v.content;
  }
  
}

function peekExn(q) {
  var v = q.first;
  if (v !== undefined) {
    return v.content;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function pop(q) {
  var x = q.first;
  if (x === undefined) {
    return ;
  }
  var next = x.next;
  if (next === undefined) {
    clear(q);
    return Caml_option.some(x.content);
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return Caml_option.some(x.content);
  }
}

function popExn(q) {
  var x = q.first;
  if (x !== undefined) {
    var next = x.next;
    if (next === undefined) {
      clear(q);
      return x.content;
    } else {
      q.length = q.length - 1 | 0;
      q.first = next;
      return x.content;
    }
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function popUndefined(q) {
  var x = q.first;
  if (x === undefined) {
    return ;
  }
  var next = x.next;
  if (next === undefined) {
    clear(q);
    return x.content;
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return x.content;
  }
}

function copy(q) {
  var qRes = {
    length: q.length,
    first: undefined,
    last: undefined
  };
  var _prev;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== undefined) {
      var content = cell.content;
      var res = {
        content: content,
        next: undefined
      };
      if (prev !== undefined) {
        prev.next = res;
      } else {
        qRes.first = res;
      }
      _cell = cell.next;
      _prev = res;
      continue ;
    }
    qRes.last = prev;
    return qRes;
  };
}

function mapU(q, f) {
  var qRes = {
    length: q.length,
    first: undefined,
    last: undefined
  };
  var _prev;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== undefined) {
      var content = f(cell.content);
      var res = {
        content: content,
        next: undefined
      };
      if (prev !== undefined) {
        prev.next = res;
      } else {
        qRes.first = res;
      }
      _cell = cell.next;
      _prev = res;
      continue ;
    }
    qRes.last = prev;
    return qRes;
  };
}

function map(q, f) {
  return mapU(q, Curry.__1(f));
}

function isEmpty(q) {
  return q.length === 0;
}

function size(q) {
  return q.length;
}

function forEachU(q, f) {
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    if (cell === undefined) {
      return ;
    }
    f(cell.content);
    _cell = cell.next;
    continue ;
  };
}

function forEach(q, f) {
  return forEachU(q, Curry.__1(f));
}

function reduceU(q, accu, f) {
  var _accu = accu;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var accu$1 = _accu;
    if (cell === undefined) {
      return accu$1;
    }
    var accu$2 = f(accu$1, cell.content);
    _cell = cell.next;
    _accu = accu$2;
    continue ;
  };
}

function reduce(q, accu, f) {
  return reduceU(q, accu, Curry.__2(f));
}

function transfer(q1, q2) {
  if (q1.length <= 0) {
    return ;
  }
  var l = q2.last;
  if (l !== undefined) {
    q2.length = q2.length + q1.length | 0;
    l.next = q1.first;
    q2.last = q1.last;
    return clear(q1);
  } else {
    q2.length = q1.length;
    q2.first = q1.first;
    q2.last = q1.last;
    return clear(q1);
  }
}

function fillAux(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    if (cell === undefined) {
      return ;
    }
    arr[i] = cell.content;
    _cell = cell.next;
    _i = i + 1 | 0;
    continue ;
  };
}

function toArray(x) {
  var v = new Array(x.length);
  fillAux(0, v, x.first);
  return v;
}

function fromArray(arr) {
  var q = {
    length: 0,
    first: undefined,
    last: undefined
  };
  for(var i = 0 ,i_finish = arr.length; i < i_finish; ++i){
    add(q, arr[i]);
  }
  return q;
}

export {
  make ,
  clear ,
  isEmpty ,
  fromArray ,
  add ,
  peek ,
  peekUndefined ,
  peekExn ,
  pop ,
  popUndefined ,
  popExn ,
  copy ,
  size ,
  mapU ,
  map ,
  forEachU ,
  forEach ,
  reduceU ,
  reduce ,
  transfer ,
  toArray ,
  
}
/* No side effect */
