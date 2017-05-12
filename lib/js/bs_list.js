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
/* No side effect */
