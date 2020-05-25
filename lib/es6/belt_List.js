

import * as Curry from "./curry.js";
import * as Belt_Array from "./belt_Array.js";
import * as Caml_option from "./caml_option.js";
import * as Belt_SortArray from "./belt_SortArray.js";

var m = (function(xs,ys){
      xs._1 = ys 
});

function head(x) {
  if (x) {
    return Caml_option.some(x._0);
  }
  
}

function headExn(x) {
  if (x) {
    return x._0;
  }
  throw new Error("headExn");
}

function tail(x) {
  if (x) {
    return x._1;
  }
  
}

function tailExn(x) {
  if (x) {
    return x._1;
  }
  throw new Error("tailExn");
}

function add(xs, x) {
  return /* :: */{
          _0: x,
          _1: xs
        };
}

function get(x, n) {
  if (n < 0) {
    return ;
  } else {
    var _x = x;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var x$1 = _x;
      if (!x$1) {
        return ;
      }
      if (n$1 === 0) {
        return Caml_option.some(x$1._0);
      }
      _n = n$1 - 1 | 0;
      _x = x$1._1;
      continue ;
    };
  }
}

function getExn(x, n) {
  if (n < 0) {
    throw new Error("getExn");
  }
  var _x = x;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var x$1 = _x;
    if (x$1) {
      if (n$1 === 0) {
        return x$1._0;
      }
      _n = n$1 - 1 | 0;
      _x = x$1._1;
      continue ;
    }
    throw new Error("getExn");
  };
}

function partitionAux(p, _cell, _precX, _precY) {
  while(true) {
    var precY = _precY;
    var precX = _precX;
    var cell = _cell;
    if (!cell) {
      return ;
    }
    var t = cell._1;
    var h = cell._0;
    var next = /* :: */{
      _0: h,
      _1: /* [] */0
    };
    if (p(h)) {
      m(precX, next);
      _precX = next;
      _cell = t;
      continue ;
    }
    m(precY, next);
    _precY = next;
    _cell = t;
    continue ;
  };
}

function splitAux(_cell, _precX, _precY) {
  while(true) {
    var precY = _precY;
    var precX = _precX;
    var cell = _cell;
    if (!cell) {
      return ;
    }
    var match = cell._0;
    var nextA = /* :: */{
      _0: match[0],
      _1: /* [] */0
    };
    var nextB = /* :: */{
      _0: match[1],
      _1: /* [] */0
    };
    m(precX, nextA);
    m(precY, nextB);
    _precY = nextB;
    _precX = nextA;
    _cell = cell._1;
    continue ;
  };
}

function copyAuxCont(_cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return prec;
    }
    var next = /* :: */{
      _0: cellX._0,
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellX = cellX._1;
    continue ;
  };
}

function copyAuxWitFilter(f, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    var t = cellX._1;
    var h = cellX._0;
    if (f(h)) {
      var next = /* :: */{
        _0: h,
        _1: /* [] */0
      };
      m(prec, next);
      _prec = next;
      _cellX = t;
      continue ;
    }
    _cellX = t;
    continue ;
  };
}

function copyAuxWithFilterIndex(f, _cellX, _prec, _i) {
  while(true) {
    var i = _i;
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    var t = cellX._1;
    var h = cellX._0;
    if (f(h, i)) {
      var next = /* :: */{
        _0: h,
        _1: /* [] */0
      };
      m(prec, next);
      _i = i + 1 | 0;
      _prec = next;
      _cellX = t;
      continue ;
    }
    _i = i + 1 | 0;
    _cellX = t;
    continue ;
  };
}

function copyAuxWitFilterMap(f, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    var t = cellX._1;
    var h = f(cellX._0);
    if (h !== undefined) {
      var next = /* :: */{
        _0: Caml_option.valFromOption(h),
        _1: /* [] */0
      };
      m(prec, next);
      _prec = next;
      _cellX = t;
      continue ;
    }
    _cellX = t;
    continue ;
  };
}

function removeAssocAuxWithMap(_cellX, x, _prec, f) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return false;
    }
    var t = cellX._1;
    var h = cellX._0;
    if (f(h[0], x)) {
      m(prec, t);
      return true;
    }
    var next = /* :: */{
      _0: h,
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellX = t;
    continue ;
  };
}

function setAssocAuxWithMap(_cellX, x, k, _prec, eq) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return false;
    }
    var t = cellX._1;
    var h = cellX._0;
    if (eq(h[0], x)) {
      m(prec, /* :: */{
            _0: [
              x,
              k
            ],
            _1: t
          });
      return true;
    }
    var next = /* :: */{
      _0: h,
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellX = t;
    continue ;
  };
}

function copyAuxWithMap(_cellX, _prec, f) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    var next = /* :: */{
      _0: f(cellX._0),
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellX = cellX._1;
    continue ;
  };
}

function zipAux(_cellX, _cellY, _prec) {
  while(true) {
    var prec = _prec;
    var cellY = _cellY;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    if (!cellY) {
      return ;
    }
    var next = /* :: */{
      _0: [
        cellX._0,
        cellY._0
      ],
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellY = cellY._1;
    _cellX = cellX._1;
    continue ;
  };
}

function copyAuxWithMap2(f, _cellX, _cellY, _prec) {
  while(true) {
    var prec = _prec;
    var cellY = _cellY;
    var cellX = _cellX;
    if (!cellX) {
      return ;
    }
    if (!cellY) {
      return ;
    }
    var next = /* :: */{
      _0: f(cellX._0, cellY._0),
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellY = cellY._1;
    _cellX = cellX._1;
    continue ;
  };
}

function copyAuxWithMapI(f, _i, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    var i = _i;
    if (!cellX) {
      return ;
    }
    var next = /* :: */{
      _0: f(i, cellX._0),
      _1: /* [] */0
    };
    m(prec, next);
    _prec = next;
    _cellX = cellX._1;
    _i = i + 1 | 0;
    continue ;
  };
}

function takeAux(_n, _cell, _prec) {
  while(true) {
    var prec = _prec;
    var cell = _cell;
    var n = _n;
    if (n === 0) {
      return true;
    }
    if (!cell) {
      return false;
    }
    var cell$1 = /* :: */{
      _0: cell._0,
      _1: /* [] */0
    };
    m(prec, cell$1);
    _prec = cell$1;
    _cell = cell._1;
    _n = n - 1 | 0;
    continue ;
  };
}

function splitAtAux(_n, _cell, _prec) {
  while(true) {
    var prec = _prec;
    var cell = _cell;
    var n = _n;
    if (n === 0) {
      return cell;
    }
    if (!cell) {
      return ;
    }
    var cell$1 = /* :: */{
      _0: cell._0,
      _1: /* [] */0
    };
    m(prec, cell$1);
    _prec = cell$1;
    _cell = cell._1;
    _n = n - 1 | 0;
    continue ;
  };
}

function take(lst, n) {
  if (n < 0) {
    return ;
  }
  if (n === 0) {
    return /* [] */0;
  }
  if (!lst) {
    return ;
  }
  var cell = /* :: */{
    _0: lst._0,
    _1: /* [] */0
  };
  var has = takeAux(n - 1 | 0, lst._1, cell);
  if (has) {
    return cell;
  }
  
}

function drop(lst, n) {
  if (n < 0) {
    return ;
  } else {
    var _l = lst;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l = _l;
      if (n$1 === 0) {
        return l;
      }
      if (!l) {
        return ;
      }
      _n = n$1 - 1 | 0;
      _l = l._1;
      continue ;
    };
  }
}

function splitAt(lst, n) {
  if (n < 0) {
    return ;
  }
  if (n === 0) {
    return [
            /* [] */0,
            lst
          ];
  }
  if (!lst) {
    return ;
  }
  var cell = /* :: */{
    _0: lst._0,
    _1: /* [] */0
  };
  var rest = splitAtAux(n - 1 | 0, lst._1, cell);
  if (rest !== undefined) {
    return [
            cell,
            rest
          ];
  }
  
}

function concat(xs, ys) {
  if (!xs) {
    return ys;
  }
  var cell = /* :: */{
    _0: xs._0,
    _1: /* [] */0
  };
  m(copyAuxCont(xs._1, cell), ys);
  return cell;
}

function mapU(xs, f) {
  if (!xs) {
    return /* [] */0;
  }
  var cell = /* :: */{
    _0: f(xs._0),
    _1: /* [] */0
  };
  copyAuxWithMap(xs._1, cell, f);
  return cell;
}

function map(xs, f) {
  return mapU(xs, Curry.__1(f));
}

function zipByU(l1, l2, f) {
  if (!l1) {
    return /* [] */0;
  }
  if (!l2) {
    return /* [] */0;
  }
  var cell = /* :: */{
    _0: f(l1._0, l2._0),
    _1: /* [] */0
  };
  copyAuxWithMap2(f, l1._1, l2._1, cell);
  return cell;
}

function zipBy(l1, l2, f) {
  return zipByU(l1, l2, Curry.__2(f));
}

function mapWithIndexU(xs, f) {
  if (!xs) {
    return /* [] */0;
  }
  var cell = /* :: */{
    _0: f(0, xs._0),
    _1: /* [] */0
  };
  copyAuxWithMapI(f, 1, xs._1, cell);
  return cell;
}

function mapWithIndex(xs, f) {
  return mapWithIndexU(xs, Curry.__2(f));
}

function makeByU(n, f) {
  if (n <= 0) {
    return /* [] */0;
  }
  var headX = /* :: */{
    _0: f(0),
    _1: /* [] */0
  };
  var cur = headX;
  var i = 1;
  while(i < n) {
    var v = /* :: */{
      _0: f(i),
      _1: /* [] */0
    };
    m(cur, v);
    cur = v;
    i = i + 1 | 0;
  };
  return headX;
}

function makeBy(n, f) {
  return makeByU(n, Curry.__1(f));
}

function make(n, v) {
  if (n <= 0) {
    return /* [] */0;
  }
  var headX = /* :: */{
    _0: v,
    _1: /* [] */0
  };
  var cur = headX;
  var i = 1;
  while(i < n) {
    var v$1 = /* :: */{
      _0: v,
      _1: /* [] */0
    };
    m(cur, v$1);
    cur = v$1;
    i = i + 1 | 0;
  };
  return headX;
}

function length(xs) {
  var _x = xs;
  var _acc = 0;
  while(true) {
    var acc = _acc;
    var x = _x;
    if (!x) {
      return acc;
    }
    _acc = acc + 1 | 0;
    _x = x._1;
    continue ;
  };
}

function fillAux(arr, _i, _x) {
  while(true) {
    var x = _x;
    var i = _i;
    if (!x) {
      return ;
    }
    arr[i] = x._0;
    _x = x._1;
    _i = i + 1 | 0;
    continue ;
  };
}

function fromArray(a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    }
    _res = /* :: */{
      _0: a[i],
      _1: res
    };
    _i = i - 1 | 0;
    continue ;
  };
}

function toArray(x) {
  var len = length(x);
  var arr = new Array(len);
  fillAux(arr, 0, x);
  return arr;
}

function shuffle(xs) {
  var v = toArray(xs);
  Belt_Array.shuffleInPlace(v);
  return fromArray(v);
}

function reverseConcat(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return l2;
    }
    _l2 = /* :: */{
      _0: l1._0,
      _1: l2
    };
    _l1 = l1._1;
    continue ;
  };
}

function reverse(l) {
  return reverseConcat(l, /* [] */0);
}

function flattenAux(_prec, _xs) {
  while(true) {
    var xs = _xs;
    var prec = _prec;
    if (!xs) {
      return m(prec, /* [] */0);
    }
    _xs = xs._1;
    _prec = copyAuxCont(xs._0, prec);
    continue ;
  };
}

function flatten(_xs) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return /* [] */0;
    }
    var match = xs._0;
    if (match) {
      var cell = /* :: */{
        _0: match._0,
        _1: /* [] */0
      };
      flattenAux(copyAuxCont(match._1, cell), xs._1);
      return cell;
    }
    _xs = xs._1;
    continue ;
  };
}

function concatMany(xs) {
  var len = xs.length;
  if (len === 1) {
    return xs[0];
  }
  if (len === 0) {
    return /* [] */0;
  }
  var len$1 = xs.length;
  var v = xs[len$1 - 1 | 0];
  for(var i = len$1 - 2 | 0; i >= 0; --i){
    v = concat(xs[i], v);
  }
  return v;
}

function mapReverseU(l, f) {
  var _accu = /* [] */0;
  var _xs = l;
  while(true) {
    var xs = _xs;
    var accu = _accu;
    if (!xs) {
      return accu;
    }
    _xs = xs._1;
    _accu = /* :: */{
      _0: f(xs._0),
      _1: accu
    };
    continue ;
  };
}

function mapReverse(l, f) {
  return mapReverseU(l, Curry.__1(f));
}

function forEachU(_xs, f) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return ;
    }
    f(xs._0);
    _xs = xs._1;
    continue ;
  };
}

function forEach(xs, f) {
  return forEachU(xs, Curry.__1(f));
}

function forEachWithIndexU(l, f) {
  var _xs = l;
  var _i = 0;
  while(true) {
    var i = _i;
    var xs = _xs;
    if (!xs) {
      return ;
    }
    f(i, xs._0);
    _i = i + 1 | 0;
    _xs = xs._1;
    continue ;
  };
}

function forEachWithIndex(l, f) {
  return forEachWithIndexU(l, Curry.__2(f));
}

function reduceU(_l, _accu, f) {
  while(true) {
    var accu = _accu;
    var l = _l;
    if (!l) {
      return accu;
    }
    _accu = f(accu, l._0);
    _l = l._1;
    continue ;
  };
}

function reduce(l, accu, f) {
  return reduceU(l, accu, Curry.__2(f));
}

function reduceReverseUnsafeU(l, accu, f) {
  if (l) {
    return f(reduceReverseUnsafeU(l._1, accu, f), l._0);
  } else {
    return accu;
  }
}

function reduceReverseU(l, acc, f) {
  var len = length(l);
  if (len < 1000) {
    return reduceReverseUnsafeU(l, acc, f);
  } else {
    return Belt_Array.reduceReverseU(toArray(l), acc, f);
  }
}

function reduceReverse(l, accu, f) {
  return reduceReverseU(l, accu, Curry.__2(f));
}

function reduceWithIndexU(l, acc, f) {
  var _l = l;
  var _acc = acc;
  var _i = 0;
  while(true) {
    var i = _i;
    var acc$1 = _acc;
    var l$1 = _l;
    if (!l$1) {
      return acc$1;
    }
    _i = i + 1 | 0;
    _acc = f(acc$1, l$1._0, i);
    _l = l$1._1;
    continue ;
  };
}

function reduceWithIndex(l, acc, f) {
  return reduceWithIndexU(l, acc, Curry.__3(f));
}

function mapReverse2U(l1, l2, f) {
  var _l1 = l1;
  var _l2 = l2;
  var _accu = /* [] */0;
  while(true) {
    var accu = _accu;
    var l2$1 = _l2;
    var l1$1 = _l1;
    if (!l1$1) {
      return accu;
    }
    if (!l2$1) {
      return accu;
    }
    _accu = /* :: */{
      _0: f(l1$1._0, l2$1._0),
      _1: accu
    };
    _l2 = l2$1._1;
    _l1 = l1$1._1;
    continue ;
  };
}

function mapReverse2(l1, l2, f) {
  return mapReverse2U(l1, l2, Curry.__2(f));
}

function forEach2U(_l1, _l2, f) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return ;
    }
    if (!l2) {
      return ;
    }
    f(l1._0, l2._0);
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function forEach2(l1, l2, f) {
  return forEach2U(l1, l2, Curry.__2(f));
}

function reduce2U(_l1, _l2, _accu, f) {
  while(true) {
    var accu = _accu;
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return accu;
    }
    if (!l2) {
      return accu;
    }
    _accu = f(accu, l1._0, l2._0);
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function reduce2(l1, l2, acc, f) {
  return reduce2U(l1, l2, acc, Curry.__3(f));
}

function reduceReverse2UnsafeU(l1, l2, accu, f) {
  if (l1 && l2) {
    return f(reduceReverse2UnsafeU(l1._1, l2._1, accu, f), l1._0, l2._0);
  } else {
    return accu;
  }
}

function reduceReverse2U(l1, l2, acc, f) {
  var len = length(l1);
  if (len < 1000) {
    return reduceReverse2UnsafeU(l1, l2, acc, f);
  } else {
    return Belt_Array.reduceReverse2U(toArray(l1), toArray(l2), acc, f);
  }
}

function reduceReverse2(l1, l2, acc, f) {
  return reduceReverse2U(l1, l2, acc, Curry.__3(f));
}

function everyU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return true;
    }
    if (!p(xs._0)) {
      return false;
    }
    _xs = xs._1;
    continue ;
  };
}

function every(xs, p) {
  return everyU(xs, Curry.__1(p));
}

function someU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return false;
    }
    if (p(xs._0)) {
      return true;
    }
    _xs = xs._1;
    continue ;
  };
}

function some(xs, p) {
  return someU(xs, Curry.__1(p));
}

function every2U(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return true;
    }
    if (!l2) {
      return true;
    }
    if (!p(l1._0, l2._0)) {
      return false;
    }
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function every2(l1, l2, p) {
  return every2U(l1, l2, Curry.__2(p));
}

function cmpByLength(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!l2) {
      return 1;
    }
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function cmpU(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!l2) {
      return 1;
    }
    var c = p(l1._0, l2._0);
    if (c !== 0) {
      return c;
    }
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function cmp(l1, l2, f) {
  return cmpU(l1, l2, Curry.__2(f));
}

function eqU(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return false;
      } else {
        return true;
      }
    }
    if (!l2) {
      return false;
    }
    if (!p(l1._0, l2._0)) {
      return false;
    }
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function eq(l1, l2, f) {
  return eqU(l1, l2, Curry.__2(f));
}

function some2U(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return false;
    }
    if (!l2) {
      return false;
    }
    if (p(l1._0, l2._0)) {
      return true;
    }
    _l2 = l2._1;
    _l1 = l1._1;
    continue ;
  };
}

function some2(l1, l2, p) {
  return some2U(l1, l2, Curry.__2(p));
}

function hasU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return false;
    }
    if (eq(xs._0, x)) {
      return true;
    }
    _xs = xs._1;
    continue ;
  };
}

function has(xs, x, eq) {
  return hasU(xs, x, Curry.__2(eq));
}

function getAssocU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return ;
    }
    var match = xs._0;
    if (eq(match[0], x)) {
      return Caml_option.some(match[1]);
    }
    _xs = xs._1;
    continue ;
  };
}

function getAssoc(xs, x, eq) {
  return getAssocU(xs, x, Curry.__2(eq));
}

function hasAssocU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return false;
    }
    if (eq(xs._0[0], x)) {
      return true;
    }
    _xs = xs._1;
    continue ;
  };
}

function hasAssoc(xs, x, eq) {
  return hasAssocU(xs, x, Curry.__2(eq));
}

function removeAssocU(xs, x, eq) {
  if (!xs) {
    return /* [] */0;
  }
  var l = xs._1;
  var pair = xs._0;
  if (eq(pair[0], x)) {
    return l;
  }
  var cell = /* :: */{
    _0: pair,
    _1: /* [] */0
  };
  var removed = removeAssocAuxWithMap(l, x, cell, eq);
  if (removed) {
    return cell;
  } else {
    return xs;
  }
}

function removeAssoc(xs, x, eq) {
  return removeAssocU(xs, x, Curry.__2(eq));
}

function setAssocU(xs, x, k, eq) {
  if (!xs) {
    return /* :: */{
            _0: [
              x,
              k
            ],
            _1: /* [] */0
          };
  }
  var l = xs._1;
  var pair = xs._0;
  if (eq(pair[0], x)) {
    return /* :: */{
            _0: [
              x,
              k
            ],
            _1: l
          };
  }
  var cell = /* :: */{
    _0: pair,
    _1: /* [] */0
  };
  var replaced = setAssocAuxWithMap(l, x, k, cell, eq);
  if (replaced) {
    return cell;
  } else {
    return /* :: */{
            _0: [
              x,
              k
            ],
            _1: xs
          };
  }
}

function setAssoc(xs, x, k, eq) {
  return setAssocU(xs, x, k, Curry.__2(eq));
}

function sortU(xs, cmp) {
  var arr = toArray(xs);
  Belt_SortArray.stableSortInPlaceByU(arr, cmp);
  return fromArray(arr);
}

function sort(xs, cmp) {
  return sortU(xs, Curry.__2(cmp));
}

function getByU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return ;
    }
    var x = xs._0;
    if (p(x)) {
      return Caml_option.some(x);
    }
    _xs = xs._1;
    continue ;
  };
}

function getBy(xs, p) {
  return getByU(xs, Curry.__1(p));
}

function keepU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return /* [] */0;
    }
    var t = xs._1;
    var h = xs._0;
    if (p(h)) {
      var cell = /* :: */{
        _0: h,
        _1: /* [] */0
      };
      copyAuxWitFilter(p, t, cell);
      return cell;
    }
    _xs = t;
    continue ;
  };
}

function keep(xs, p) {
  return keepU(xs, Curry.__1(p));
}

function keepWithIndexU(xs, p) {
  var _xs = xs;
  var _i = 0;
  while(true) {
    var i = _i;
    var xs$1 = _xs;
    if (!xs$1) {
      return /* [] */0;
    }
    var t = xs$1._1;
    var h = xs$1._0;
    if (p(h, i)) {
      var cell = /* :: */{
        _0: h,
        _1: /* [] */0
      };
      copyAuxWithFilterIndex(p, t, cell, i + 1 | 0);
      return cell;
    }
    _i = i + 1 | 0;
    _xs = t;
    continue ;
  };
}

function keepWithIndex(xs, p) {
  return keepWithIndexU(xs, Curry.__2(p));
}

function keepMapU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (!xs) {
      return /* [] */0;
    }
    var t = xs._1;
    var h = p(xs._0);
    if (h !== undefined) {
      var cell = /* :: */{
        _0: Caml_option.valFromOption(h),
        _1: /* [] */0
      };
      copyAuxWitFilterMap(p, t, cell);
      return cell;
    }
    _xs = t;
    continue ;
  };
}

function keepMap(xs, p) {
  return keepMapU(xs, Curry.__1(p));
}

function partitionU(l, p) {
  if (!l) {
    return [
            /* [] */0,
            /* [] */0
          ];
  }
  var h = l._0;
  var nextX = /* :: */{
    _0: h,
    _1: /* [] */0
  };
  var nextY = /* :: */{
    _0: h,
    _1: /* [] */0
  };
  var b = p(h);
  partitionAux(p, l._1, nextX, nextY);
  if (b) {
    return [
            nextX,
            nextY._1
          ];
  } else {
    return [
            nextX._1,
            nextY
          ];
  }
}

function partition(l, p) {
  return partitionU(l, Curry.__1(p));
}

function unzip(xs) {
  if (!xs) {
    return [
            /* [] */0,
            /* [] */0
          ];
  }
  var match = xs._0;
  var cellX = /* :: */{
    _0: match[0],
    _1: /* [] */0
  };
  var cellY = /* :: */{
    _0: match[1],
    _1: /* [] */0
  };
  splitAux(xs._1, cellX, cellY);
  return [
          cellX,
          cellY
        ];
}

function zip(l1, l2) {
  if (!l1) {
    return /* [] */0;
  }
  if (!l2) {
    return /* [] */0;
  }
  var cell = /* :: */{
    _0: [
      l1._0,
      l2._0
    ],
    _1: /* [] */0
  };
  zipAux(l1._1, l2._1, cell);
  return cell;
}

var size = length;

var filter = keep;

var filterWithIndex = keepWithIndex;

export {
  length ,
  size ,
  head ,
  headExn ,
  tail ,
  tailExn ,
  add ,
  get ,
  getExn ,
  make ,
  makeByU ,
  makeBy ,
  shuffle ,
  drop ,
  take ,
  splitAt ,
  concat ,
  concatMany ,
  reverseConcat ,
  flatten ,
  mapU ,
  map ,
  zip ,
  zipByU ,
  zipBy ,
  mapWithIndexU ,
  mapWithIndex ,
  fromArray ,
  toArray ,
  reverse ,
  mapReverseU ,
  mapReverse ,
  forEachU ,
  forEach ,
  forEachWithIndexU ,
  forEachWithIndex ,
  reduceU ,
  reduce ,
  reduceWithIndexU ,
  reduceWithIndex ,
  reduceReverseU ,
  reduceReverse ,
  mapReverse2U ,
  mapReverse2 ,
  forEach2U ,
  forEach2 ,
  reduce2U ,
  reduce2 ,
  reduceReverse2U ,
  reduceReverse2 ,
  everyU ,
  every ,
  someU ,
  some ,
  every2U ,
  every2 ,
  some2U ,
  some2 ,
  cmpByLength ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  hasU ,
  has ,
  getByU ,
  getBy ,
  keepU ,
  keep ,
  filter ,
  keepWithIndexU ,
  keepWithIndex ,
  filterWithIndex ,
  keepMapU ,
  keepMap ,
  partitionU ,
  partition ,
  unzip ,
  getAssocU ,
  getAssoc ,
  hasAssocU ,
  hasAssoc ,
  removeAssocU ,
  removeAssoc ,
  setAssocU ,
  setAssoc ,
  sortU ,
  sort ,
  
}
/* No side effect */
