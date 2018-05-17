'use strict';

var Curry = require("./curry.js");
var Belt_Array = require("./belt_Array.js");
var Belt_SortArray = require("./belt_SortArray.js");

function head(x) {
  if (x) {
    return /* Some */[x[0]];
  } else {
    return /* None */0;
  }
}

function headExn(x) {
  if (x) {
    return x[0];
  } else {
    throw new Error("headExn");
  }
}

function tail(x) {
  if (x) {
    return /* Some */[x[1]];
  } else {
    return /* None */0;
  }
}

function tailExn(x) {
  if (x) {
    return x[1];
  } else {
    throw new Error("tailExn");
  }
}

function add(xs, x) {
  return /* :: */[
          x,
          xs
        ];
}

function get(x, n) {
  if (n < 0) {
    return /* None */0;
  } else {
    var _x = x;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var x$1 = _x;
      if (x$1) {
        if (n$1 === 0) {
          return /* Some */[x$1[0]];
        } else {
          _n = n$1 - 1 | 0;
          _x = x$1[1];
          continue ;
        }
      } else {
        return /* None */0;
      }
    };
  }
}

function getExn(x, n) {
  if (n < 0) {
    throw new Error("getExn");
  } else {
    var _x = x;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var x$1 = _x;
      if (x$1) {
        if (n$1 === 0) {
          return x$1[0];
        } else {
          _n = n$1 - 1 | 0;
          _x = x$1[1];
          continue ;
        }
      } else {
        throw new Error("getExn");
      }
    };
  }
}

function partitionAux(p, _cell, _precX, _precY) {
  while(true) {
    var precY = _precY;
    var precX = _precX;
    var cell = _cell;
    if (cell) {
      var t = cell[1];
      var h = cell[0];
      var next = /* :: */[
        h,
        /* [] */0
      ];
      if (p(h)) {
        precX[1] = next;
        _precX = next;
        _cell = t;
        continue ;
      } else {
        precY[1] = next;
        _precY = next;
        _cell = t;
        continue ;
      }
    } else {
      return /* () */0;
    }
  };
}

function splitAux(_cell, _precX, _precY) {
  while(true) {
    var precY = _precY;
    var precX = _precX;
    var cell = _cell;
    if (cell) {
      var match = cell[0];
      var nextA = /* :: */[
        match[0],
        /* [] */0
      ];
      var nextB = /* :: */[
        match[1],
        /* [] */0
      ];
      precX[1] = nextA;
      precY[1] = nextB;
      _precY = nextB;
      _precX = nextA;
      _cell = cell[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function copyAuxCont(_cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var next = /* :: */[
        cellX[0],
        /* [] */0
      ];
      prec[1] = next;
      _prec = next;
      _cellX = cellX[1];
      continue ;
    } else {
      return prec;
    }
  };
}

function copyAuxWitFilter(f, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var h = cellX[0];
      if (f(h)) {
        var next = /* :: */[
          h,
          /* [] */0
        ];
        prec[1] = next;
        _prec = next;
        _cellX = t;
        continue ;
      } else {
        _cellX = t;
        continue ;
      }
    } else {
      return /* () */0;
    }
  };
}

function copyAuxWitFilterMap(f, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var match = f(cellX[0]);
      if (match) {
        var next = /* :: */[
          match[0],
          /* [] */0
        ];
        prec[1] = next;
        _prec = next;
        _cellX = t;
        continue ;
      } else {
        _cellX = t;
        continue ;
      }
    } else {
      return /* () */0;
    }
  };
}

function removeAssocAuxWithMap(_cellX, x, _prec, f) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var h = cellX[0];
      if (f(h[0], x)) {
        prec[1] = t;
        return true;
      } else {
        var next = /* :: */[
          h,
          /* [] */0
        ];
        prec[1] = next;
        _prec = next;
        _cellX = t;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function setAssocAuxWithMap(_cellX, x, k, _prec, eq) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var h = cellX[0];
      if (eq(h[0], x)) {
        prec[1] = /* :: */[
          /* tuple */[
            x,
            k
          ],
          t
        ];
        return true;
      } else {
        var next = /* :: */[
          h,
          /* [] */0
        ];
        prec[1] = next;
        _prec = next;
        _cellX = t;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function copyAuxWithMap(_cellX, _prec, f) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var next = /* :: */[
        f(cellX[0]),
        /* [] */0
      ];
      prec[1] = next;
      _prec = next;
      _cellX = cellX[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function zipAux(_cellX, _cellY, _prec) {
  while(true) {
    var prec = _prec;
    var cellY = _cellY;
    var cellX = _cellX;
    if (cellX && cellY) {
      var next = /* :: */[
        /* tuple */[
          cellX[0],
          cellY[0]
        ],
        /* [] */0
      ];
      prec[1] = next;
      _prec = next;
      _cellY = cellY[1];
      _cellX = cellX[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function copyAuxWithMap2(f, _cellX, _cellY, _prec) {
  while(true) {
    var prec = _prec;
    var cellY = _cellY;
    var cellX = _cellX;
    if (cellX && cellY) {
      var next = /* :: */[
        f(cellX[0], cellY[0]),
        /* [] */0
      ];
      prec[1] = next;
      _prec = next;
      _cellY = cellY[1];
      _cellX = cellX[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function copyAuxWithMapI(f, _i, _cellX, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    var i = _i;
    if (cellX) {
      var next = /* :: */[
        f(i, cellX[0]),
        /* [] */0
      ];
      prec[1] = next;
      _prec = next;
      _cellX = cellX[1];
      _i = i + 1 | 0;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function takeAux(_n, _cell, _prec) {
  while(true) {
    var prec = _prec;
    var cell = _cell;
    var n = _n;
    if (n === 0) {
      return true;
    } else if (cell) {
      var cell$1 = /* :: */[
        cell[0],
        /* [] */0
      ];
      prec[1] = cell$1;
      _prec = cell$1;
      _cell = cell[1];
      _n = n - 1 | 0;
      continue ;
    } else {
      return false;
    }
  };
}

function splitAtAux(_n, _cell, _prec) {
  while(true) {
    var prec = _prec;
    var cell = _cell;
    var n = _n;
    if (n === 0) {
      return /* Some */[cell];
    } else if (cell) {
      var cell$1 = /* :: */[
        cell[0],
        /* [] */0
      ];
      prec[1] = cell$1;
      _prec = cell$1;
      _cell = cell[1];
      _n = n - 1 | 0;
      continue ;
    } else {
      return /* None */0;
    }
  };
}

function take(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else if (n === 0) {
    return /* Some */[/* [] */0];
  } else if (lst) {
    var cell = /* :: */[
      lst[0],
      /* [] */0
    ];
    var has = takeAux(n - 1 | 0, lst[1], cell);
    if (has) {
      return /* Some */[cell];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

function drop(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else {
    var _l = lst;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l = _l;
      if (n$1 === 0) {
        return /* Some */[l];
      } else if (l) {
        _n = n$1 - 1 | 0;
        _l = l[1];
        continue ;
      } else {
        return /* None */0;
      }
    };
  }
}

function splitAt(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else if (n === 0) {
    return /* Some */[/* tuple */[
              /* [] */0,
              lst
            ]];
  } else if (lst) {
    var cell = /* :: */[
      lst[0],
      /* [] */0
    ];
    var rest = splitAtAux(n - 1 | 0, lst[1], cell);
    if (rest) {
      return /* Some */[/* tuple */[
                cell,
                rest[0]
              ]];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

function concat(xs, ys) {
  if (xs) {
    var cell = /* :: */[
      xs[0],
      /* [] */0
    ];
    copyAuxCont(xs[1], cell)[1] = ys;
    return cell;
  } else {
    return ys;
  }
}

function mapU(xs, f) {
  if (xs) {
    var cell = /* :: */[
      f(xs[0]),
      /* [] */0
    ];
    copyAuxWithMap(xs[1], cell, f);
    return cell;
  } else {
    return /* [] */0;
  }
}

function map(xs, f) {
  return mapU(xs, Curry.__1(f));
}

function zipByU(l1, l2, f) {
  if (l1 && l2) {
    var cell = /* :: */[
      f(l1[0], l2[0]),
      /* [] */0
    ];
    copyAuxWithMap2(f, l1[1], l2[1], cell);
    return cell;
  } else {
    return /* [] */0;
  }
}

function zipBy(l1, l2, f) {
  return zipByU(l1, l2, Curry.__2(f));
}

function mapWithIndexU(xs, f) {
  if (xs) {
    var cell = /* :: */[
      f(0, xs[0]),
      /* [] */0
    ];
    copyAuxWithMapI(f, 1, xs[1], cell);
    return cell;
  } else {
    return /* [] */0;
  }
}

function mapWithIndex(xs, f) {
  return mapWithIndexU(xs, Curry.__2(f));
}

function makeByU(n, f) {
  if (n <= 0) {
    return /* [] */0;
  } else {
    var headX = /* :: */[
      f(0),
      /* [] */0
    ];
    var cur = headX;
    var i = 1;
    while(i < n) {
      var v = /* :: */[
        f(i),
        /* [] */0
      ];
      cur[1] = v;
      cur = v;
      i = i + 1 | 0;
    };
    return headX;
  }
}

function makeBy(n, f) {
  return makeByU(n, Curry.__1(f));
}

function make(n, v) {
  if (n <= 0) {
    return /* [] */0;
  } else {
    var headX = /* :: */[
      v,
      /* [] */0
    ];
    var cur = headX;
    var i = 1;
    while(i < n) {
      var v$1 = /* :: */[
        v,
        /* [] */0
      ];
      cur[1] = v$1;
      cur = v$1;
      i = i + 1 | 0;
    };
    return headX;
  }
}

function length(xs) {
  var _x = xs;
  var _acc = 0;
  while(true) {
    var acc = _acc;
    var x = _x;
    if (x) {
      _acc = acc + 1 | 0;
      _x = x[1];
      continue ;
    } else {
      return acc;
    }
  };
}

function fillAux(arr, _i, _x) {
  while(true) {
    var x = _x;
    var i = _i;
    if (x) {
      arr[i] = x[0];
      _x = x[1];
      _i = i + 1 | 0;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fromArray(a) {
  var a$1 = a;
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    } else {
      _res = /* :: */[
        a$1[i],
        res
      ];
      _i = i - 1 | 0;
      continue ;
    }
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

function reverse(l) {
  return reverseConcat(l, /* [] */0);
}

function flattenAux(_prec, _xs) {
  while(true) {
    var xs = _xs;
    var prec = _prec;
    if (xs) {
      _xs = xs[1];
      _prec = copyAuxCont(xs[0], prec);
      continue ;
    } else {
      prec[1] = /* [] */0;
      return /* () */0;
    }
  };
}

function flatten(_xs) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var match = xs[0];
      if (match) {
        var cell = /* :: */[
          match[0],
          /* [] */0
        ];
        flattenAux(copyAuxCont(match[1], cell), xs[1]);
        return cell;
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function concatMany(xs) {
  var len = xs.length;
  if (len !== 1) {
    if (len !== 0) {
      var len$1 = xs.length;
      var v = xs[len$1 - 1 | 0];
      for(var i = len$1 - 2 | 0; i >= 0; --i){
        v = concat(xs[i], v);
      }
      return v;
    } else {
      return /* [] */0;
    }
  } else {
    return xs[0];
  }
}

function mapReverseU(l, f) {
  var f$1 = f;
  var _accu = /* [] */0;
  var _xs = l;
  while(true) {
    var xs = _xs;
    var accu = _accu;
    if (xs) {
      _xs = xs[1];
      _accu = /* :: */[
        f$1(xs[0]),
        accu
      ];
      continue ;
    } else {
      return accu;
    }
  };
}

function mapReverse(l, f) {
  return mapReverseU(l, Curry.__1(f));
}

function forEachU(_xs, f) {
  while(true) {
    var xs = _xs;
    if (xs) {
      f(xs[0]);
      _xs = xs[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEach(xs, f) {
  return forEachU(xs, Curry.__1(f));
}

function forEachWithIndexU(l, f) {
  var _xs = l;
  var _i = 0;
  var f$1 = f;
  while(true) {
    var i = _i;
    var xs = _xs;
    if (xs) {
      f$1(i, xs[0]);
      _i = i + 1 | 0;
      _xs = xs[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEachWithIndex(l, f) {
  return forEachWithIndexU(l, Curry.__2(f));
}

function reduceU(_l, _accu, f) {
  while(true) {
    var accu = _accu;
    var l = _l;
    if (l) {
      _accu = f(accu, l[0]);
      _l = l[1];
      continue ;
    } else {
      return accu;
    }
  };
}

function reduce(l, accu, f) {
  return reduceU(l, accu, Curry.__2(f));
}

function reduceReverseUnsafeU(l, accu, f) {
  if (l) {
    return f(reduceReverseUnsafeU(l[1], accu, f), l[0]);
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

function mapReverse2U(l1, l2, f) {
  var _l1 = l1;
  var _l2 = l2;
  var _accu = /* [] */0;
  var f$1 = f;
  while(true) {
    var accu = _accu;
    var l2$1 = _l2;
    var l1$1 = _l1;
    if (l1$1 && l2$1) {
      _accu = /* :: */[
        f$1(l1$1[0], l2$1[0]),
        accu
      ];
      _l2 = l2$1[1];
      _l1 = l1$1[1];
      continue ;
    } else {
      return accu;
    }
  };
}

function mapReverse2(l1, l2, f) {
  return mapReverse2U(l1, l2, Curry.__2(f));
}

function forEach2U(_l1, _l2, f) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 && l2) {
      f(l1[0], l2[0]);
      _l2 = l2[1];
      _l1 = l1[1];
      continue ;
    } else {
      return /* () */0;
    }
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
    if (l1 && l2) {
      _accu = f(accu, l1[0], l2[0]);
      _l2 = l2[1];
      _l1 = l1[1];
      continue ;
    } else {
      return accu;
    }
  };
}

function reduce2(l1, l2, acc, f) {
  return reduce2U(l1, l2, acc, Curry.__3(f));
}

function reduceReverse2UnsafeU(l1, l2, accu, f) {
  if (l1 && l2) {
    return f(reduceReverse2UnsafeU(l1[1], l2[1], accu, f), l1[0], l2[0]);
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
    if (xs) {
      if (p(xs[0])) {
        _xs = xs[1];
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function every(xs, p) {
  return everyU(xs, Curry.__1(p));
}

function someU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (p(xs[0])) {
        return true;
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function some(xs, p) {
  return someU(xs, Curry.__1(p));
}

function every2U(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 && l2) {
      if (p(l1[0], l2[0])) {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function every2(l1, l2, p) {
  return every2U(l1, l2, Curry.__2(p));
}

function cmpByLength(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      } else {
        return 1;
      }
    } else if (l2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function cmpU(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        var c = p(l1[0], l2[0]);
        if (c === 0) {
          _l2 = l2[1];
          _l1 = l1[1];
          continue ;
        } else {
          return c;
        }
      } else {
        return 1;
      }
    } else if (l2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function cmp(l1, l2, f) {
  return cmpU(l1, l2, Curry.__2(f));
}

function eqU(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2 && p(l1[0], l2[0])) {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      } else {
        return false;
      }
    } else if (l2) {
      return false;
    } else {
      return true;
    }
  };
}

function eq(l1, l2, f) {
  return eqU(l1, l2, Curry.__2(f));
}

function some2U(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 && l2) {
      if (p(l1[0], l2[0])) {
        return true;
      } else {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function some2(l1, l2, p) {
  return some2U(l1, l2, Curry.__2(p));
}

function hasU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (eq(xs[0], x)) {
        return true;
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function has(xs, x, eq) {
  return hasU(xs, x, Curry.__2(eq));
}

function getAssocU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var match = xs[0];
      if (eq(match[0], x)) {
        return /* Some */[match[1]];
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return /* None */0;
    }
  };
}

function getAssoc(xs, x, eq) {
  return getAssocU(xs, x, Curry.__2(eq));
}

function hasAssocU(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (eq(xs[0][0], x)) {
        return true;
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function hasAssoc(xs, x, eq) {
  return hasAssocU(xs, x, Curry.__2(eq));
}

function removeAssocU(xs, x, eq) {
  if (xs) {
    var l = xs[1];
    var pair = xs[0];
    if (eq(pair[0], x)) {
      return l;
    } else {
      var cell = /* :: */[
        pair,
        /* [] */0
      ];
      var removed = removeAssocAuxWithMap(l, x, cell, eq);
      if (removed) {
        return cell;
      } else {
        return xs;
      }
    }
  } else {
    return /* [] */0;
  }
}

function removeAssoc(xs, x, eq) {
  return removeAssocU(xs, x, Curry.__2(eq));
}

function setAssocU(xs, x, k, eq) {
  if (xs) {
    var l = xs[1];
    var pair = xs[0];
    if (eq(pair[0], x)) {
      return /* :: */[
              /* tuple */[
                x,
                k
              ],
              l
            ];
    } else {
      var cell = /* :: */[
        pair,
        /* [] */0
      ];
      var replaced = setAssocAuxWithMap(l, x, k, cell, eq);
      if (replaced) {
        return cell;
      } else {
        return /* :: */[
                /* tuple */[
                  x,
                  k
                ],
                xs
              ];
      }
    }
  } else {
    return /* :: */[
            /* tuple */[
              x,
              k
            ],
            /* [] */0
          ];
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
    if (xs) {
      var x = xs[0];
      if (p(x)) {
        return /* Some */[x];
      } else {
        _xs = xs[1];
        continue ;
      }
    } else {
      return /* None */0;
    }
  };
}

function getBy(xs, p) {
  return getByU(xs, Curry.__1(p));
}

function keepU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var t = xs[1];
      var h = xs[0];
      if (p(h)) {
        var cell = /* :: */[
          h,
          /* [] */0
        ];
        copyAuxWitFilter(p, t, cell);
        return cell;
      } else {
        _xs = t;
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function keep(xs, p) {
  return keepU(xs, Curry.__1(p));
}

function keepMapU(_xs, p) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var t = xs[1];
      var match = p(xs[0]);
      if (match) {
        var cell = /* :: */[
          match[0],
          /* [] */0
        ];
        copyAuxWitFilterMap(p, t, cell);
        return cell;
      } else {
        _xs = t;
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function keepMap(xs, p) {
  return keepMapU(xs, Curry.__1(p));
}

function partitionU(l, p) {
  if (l) {
    var h = l[0];
    var nextX = /* :: */[
      h,
      /* [] */0
    ];
    var nextY = /* :: */[
      h,
      /* [] */0
    ];
    var b = p(h);
    partitionAux(p, l[1], nextX, nextY);
    if (b) {
      return /* tuple */[
              nextX,
              nextY[1]
            ];
    } else {
      return /* tuple */[
              nextX[1],
              nextY
            ];
    }
  } else {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
}

function partition(l, p) {
  return partitionU(l, Curry.__1(p));
}

function unzip(xs) {
  if (xs) {
    var match = xs[0];
    var cellX = /* :: */[
      match[0],
      /* [] */0
    ];
    var cellY = /* :: */[
      match[1],
      /* [] */0
    ];
    splitAux(xs[1], cellX, cellY);
    return /* tuple */[
            cellX,
            cellY
          ];
  } else {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
}

function zip(l1, l2) {
  if (l1 && l2) {
    var cell = /* :: */[
      /* tuple */[
        l1[0],
        l2[0]
      ],
      /* [] */0
    ];
    zipAux(l1[1], l2[1], cell);
    return cell;
  } else {
    return /* [] */0;
  }
}

var size = length;

exports.length = length;
exports.size = size;
exports.head = head;
exports.headExn = headExn;
exports.tail = tail;
exports.tailExn = tailExn;
exports.add = add;
exports.get = get;
exports.getExn = getExn;
exports.make = make;
exports.makeByU = makeByU;
exports.makeBy = makeBy;
exports.shuffle = shuffle;
exports.drop = drop;
exports.take = take;
exports.splitAt = splitAt;
exports.concat = concat;
exports.concatMany = concatMany;
exports.reverseConcat = reverseConcat;
exports.flatten = flatten;
exports.mapU = mapU;
exports.map = map;
exports.zip = zip;
exports.zipByU = zipByU;
exports.zipBy = zipBy;
exports.mapWithIndexU = mapWithIndexU;
exports.mapWithIndex = mapWithIndex;
exports.fromArray = fromArray;
exports.toArray = toArray;
exports.reverse = reverse;
exports.mapReverseU = mapReverseU;
exports.mapReverse = mapReverse;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.forEachWithIndexU = forEachWithIndexU;
exports.forEachWithIndex = forEachWithIndex;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.reduceReverseU = reduceReverseU;
exports.reduceReverse = reduceReverse;
exports.mapReverse2U = mapReverse2U;
exports.mapReverse2 = mapReverse2;
exports.forEach2U = forEach2U;
exports.forEach2 = forEach2;
exports.reduce2U = reduce2U;
exports.reduce2 = reduce2;
exports.reduceReverse2U = reduceReverse2U;
exports.reduceReverse2 = reduceReverse2;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.every2U = every2U;
exports.every2 = every2;
exports.some2U = some2U;
exports.some2 = some2;
exports.cmpByLength = cmpByLength;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
exports.hasU = hasU;
exports.has = has;
exports.getByU = getByU;
exports.getBy = getBy;
exports.keepU = keepU;
exports.keep = keep;
exports.keepMapU = keepMapU;
exports.keepMap = keepMap;
exports.partitionU = partitionU;
exports.partition = partition;
exports.unzip = unzip;
exports.getAssocU = getAssocU;
exports.getAssoc = getAssoc;
exports.hasAssocU = hasAssocU;
exports.hasAssoc = hasAssoc;
exports.removeAssocU = removeAssocU;
exports.removeAssoc = removeAssoc;
exports.setAssocU = setAssocU;
exports.setAssoc = setAssoc;
exports.sortU = sortU;
exports.sort = sort;
/* No side effect */
