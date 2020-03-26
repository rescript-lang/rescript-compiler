

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";

var $$null = null;

function make(param) {
  return {
          length: 0,
          first: $$null,
          last: $$null
        };
}

function clear(q) {
  q.length = 0;
  q.first = $$null;
  q.last = $$null;
  
}

function add(q, x) {
  var cell = {
    content: x,
    next: $$null
  };
  var match = q.last;
  if (match !== null) {
    q.length = q.length + 1 | 0;
    match.next = cell;
    q.last = cell;
    return ;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
    return ;
  }
}

function peek(q) {
  var match = q.first;
  if (match !== null) {
    return Caml_option.some(match.content);
  }
  
}

function peekUndefined(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  }
  
}

function peekExn(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  }
  throw new Error("Belt.Queue.Empty");
}

function pop(q) {
  var match = q.first;
  if (match === null) {
    return ;
  }
  var next = match.next;
  if (next === null) {
    clear(q);
    return Caml_option.some(match.content);
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return Caml_option.some(match.content);
  }
}

function popExn(q) {
  var match = q.first;
  if (match !== null) {
    var next = match.next;
    if (next === null) {
      clear(q);
      return match.content;
    } else {
      q.length = q.length - 1 | 0;
      q.first = next;
      return match.content;
    }
  }
  throw new Error("Empty");
}

function popUndefined(q) {
  var match = q.first;
  if (match === null) {
    return ;
  }
  var next = match.next;
  if (next === null) {
    clear(q);
    return match.content;
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return match.content;
  }
}

function copy(q) {
  var qRes = {
    length: q.length,
    first: $$null,
    last: $$null
  };
  var _prev = $$null;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== null) {
      var content = cell.content;
      var res = {
        content: content,
        next: $$null
      };
      if (prev !== null) {
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
    first: $$null,
    last: $$null
  };
  var _prev = $$null;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== null) {
      var content = f(cell.content);
      var res = {
        content: content,
        next: $$null
      };
      if (prev !== null) {
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
    if (cell === null) {
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
    if (cell === null) {
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
  var match = q2.last;
  if (match !== null) {
    q2.length = q2.length + q1.length | 0;
    match.next = q1.first;
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
    if (cell === null) {
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
  var q = make(void 0);
  for(var i = 0 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
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
