'use strict';


var $$null = null;

function create() {
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

function push(x, q) {
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

function peekOpt(q) {
  var match = q.first;
  if (match !== null) {
    return /* Some */[match.content];
  } else {
    return /* None */0;
  }
}

function peekNull(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  } else {
    return $$null;
  }
}

function peekAssert(q) {
  var match = q.first;
  if (match !== null) {
    return match.content;
  } else {
    throw new Error("Bs.Queue.Empty");
  }
}

function popOpt(q) {
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

function popAssert(q) {
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
    throw new Error("Bs.Queue.Empty");
  }
}

function popNull(q) {
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
    return $$null;
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

function isEmpty(q) {
  return +(q.length === 0);
}

function length(q) {
  return q.length;
}

function iter(f, q) {
  var f$1 = f;
  var _cell = q.first;
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

function fold(f, accu, q) {
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

exports.clear      = clear;
exports.create     = create;
exports.push       = push;
exports.peekOpt    = peekOpt;
exports.peekNull   = peekNull;
exports.peekAssert = peekAssert;
exports.popOpt     = popOpt;
exports.popNull    = popNull;
exports.popAssert  = popAssert;
exports.copy       = copy;
exports.isEmpty    = isEmpty;
exports.length     = length;
exports.iter       = iter;
exports.fold       = fold;
exports.transfer   = transfer;
exports.toArray    = toArray;
/* null Not a pure module */
