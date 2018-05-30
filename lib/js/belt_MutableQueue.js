'use strict';

var Curry = require("./curry.js");

var $$null = null;

function make() {
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
  return /* () */0;
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
    return /* () */0;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
    return /* () */0;
  }
}

function peek(q) {
  var match = q.first;
  if (match !== null) {
    return /* Some */[match.content];
  } else {
    return /* None */0;
  }
}

function peekUndefined(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  } else {
    return undefined;
  }
}

function peekExn(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  } else {
    throw new Error("Belt.Queue.Empty");
  }
}

function pop(q) {
  var match = q.first;
  if (match !== null) {
    var next = match.next;
    if (next === null) {
      clear(q);
      return /* Some */[match.content];
    } else {
      q.length = q.length - 1 | 0;
      q.first = next;
      return /* Some */[match.content];
    }
  } else {
    return /* None */0;
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
  } else {
    throw new Error("Empty");
  }
}

function popUndefined(q) {
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
  } else {
    return undefined;
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
    } else {
      qRes.last = prev;
      return qRes;
    }
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
  var f$1 = f;
  while(true) {
    var cell = _cell;
    var prev = _prev;
    if (cell !== null) {
      var content = f$1(cell.content);
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
    } else {
      qRes.last = prev;
      return qRes;
    }
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
  var f$1 = f;
  while(true) {
    var cell = _cell;
    if (cell !== null) {
      f$1(cell.content);
      _cell = cell.next;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEach(q, f) {
  return forEachU(q, Curry.__1(f));
}

function reduceU(q, accu, f) {
  var f$1 = f;
  var _accu = accu;
  var _cell = q.first;
  while(true) {
    var cell = _cell;
    var accu$1 = _accu;
    if (cell !== null) {
      var accu$2 = f$1(accu$1, cell.content);
      _cell = cell.next;
      _accu = accu$2;
      continue ;
    } else {
      return accu$1;
    }
  };
}

function reduce(q, accu, f) {
  return reduceU(q, accu, Curry.__2(f));
}

function transfer(q1, q2) {
  if (q1.length > 0) {
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
  } else {
    return 0;
  }
}

function fillAux(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    if (cell !== null) {
      arr[i] = cell.content;
      _cell = cell.next;
      _i = i + 1 | 0;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function toArray(x) {
  var v = new Array(x.length);
  fillAux(0, v, x.first);
  return v;
}

function fromArray(arr) {
  var q = make(/* () */0);
  for(var i = 0 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
    add(q, arr[i]);
  }
  return q;
}

exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.fromArray = fromArray;
exports.add = add;
exports.peek = peek;
exports.peekUndefined = peekUndefined;
exports.peekExn = peekExn;
exports.pop = pop;
exports.popUndefined = popUndefined;
exports.popExn = popExn;
exports.copy = copy;
exports.size = size;
exports.mapU = mapU;
exports.map = map;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.transfer = transfer;
exports.toArray = toArray;
/* No side effect */
