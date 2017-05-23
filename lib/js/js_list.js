'use strict';


function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (param) {
      _param = param[1];
      _len = len + 1 | 0;
      continue ;
      
    } else {
      return len;
    }
  };
}

function cons(x, xs) {
  return /* :: */[
          x,
          xs
        ];
}

function isEmpty(x) {
  return +(x === /* [] */0);
}

function hd(param) {
  if (param) {
    return /* Some */[param[0]];
  } else {
    return /* None */0;
  }
}

function tl(param) {
  if (param) {
    return /* Some */[param[1]];
  } else {
    return /* None */0;
  }
}

function nth(l, n) {
  if (n < 0) {
    return /* None */0;
  } else {
    var _l = l;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l$1 = _l;
      if (l$1) {
        if (n$1) {
          _n = n$1 - 1 | 0;
          _l = l$1[1];
          continue ;
          
        } else {
          return /* Some */[l$1[0]];
        }
      } else {
        return /* None */0;
      }
    };
  }
}

function revAppend(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      _l2 = /* :: */[
        l1[0],
        l2
      ];
      _l1 = l1[1];
      continue ;
      
    } else {
      return l2;
    }
  };
}

function rev(l) {
  return revAppend(l, /* [] */0);
}

function mapRevAux(f, _acc, _ls) {
  while(true) {
    var ls = _ls;
    var acc = _acc;
    if (ls) {
      _ls = ls[1];
      _acc = /* :: */[
        f(ls[0]),
        acc
      ];
      continue ;
      
    } else {
      return acc;
    }
  };
}

function mapRev(f, ls) {
  return mapRevAux(f, /* [] */0, ls);
}

function map(f, ls) {
  return revAppend(mapRevAux(f, /* [] */0, ls), /* [] */0);
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      f(param[0]);
      _param = param[1];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function iteri(f, l) {
  var _i = 0;
  var f$1 = f;
  var _param = l;
  while(true) {
    var param = _param;
    var i = _i;
    if (param) {
      f$1(i, param[0]);
      _param = param[1];
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function foldLeft(f, _accu, _l) {
  while(true) {
    var l = _l;
    var accu = _accu;
    if (l) {
      _l = l[1];
      _accu = f(accu, l[0]);
      continue ;
      
    } else {
      return accu;
    }
  };
}

function foldRight(f, l, accu) {
  if (l) {
    return f(l[0], foldRight(f, l[1], accu));
  } else {
    return accu;
  }
}

exports.length    = length;
exports.cons      = cons;
exports.isEmpty   = isEmpty;
exports.hd        = hd;
exports.tl        = tl;
exports.nth       = nth;
exports.revAppend = revAppend;
exports.rev       = rev;
exports.mapRev    = mapRev;
exports.map       = map;
exports.iter      = iter;
exports.iteri     = iteri;
exports.foldLeft  = foldLeft;
exports.foldRight = foldRight;
/* No side effect */
