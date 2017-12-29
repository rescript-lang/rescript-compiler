'use strict';

var Js_json = require("./js_json.js");

function headOpt(x) {
  if (x) {
    return /* Some */[x[0]];
  } else {
    return /* None */0;
  }
}

function tailOpt(x) {
  if (x) {
    return /* Some */[x[1]];
  } else {
    return /* None */0;
  }
}

function nthOpt(x, n) {
  if (n < 0) {
    return /* None */0;
  } else {
    var _x = x;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var x$1 = _x;
      if (x$1) {
        if (n$1) {
          _n = n$1 - 1 | 0;
          _x = x$1[1];
          continue ;
          
        } else {
          return /* Some */[x$1[0]];
        }
      } else {
        return /* None */0;
      }
    };
  }
}

function nthAssert(x, n) {
  if (n < 0) {
    throw new Error("nthAssert");
  } else {
    var _x = x;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var x$1 = _x;
      if (x$1) {
        if (n$1) {
          _n = n$1 - 1 | 0;
          _x = x$1[1];
          continue ;
          
        } else {
          return x$1[0];
        }
      } else {
        throw new Error("nthAssert");
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

function removeAssocAuxWithMap(_cellX, x, _prec, f) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var h = cellX[0];
      if (f(h[0], x)) {
        prec[1] = t;
        return /* () */0;
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
      return /* () */0;
    }
  };
}

function removeAssocAux(_cellX, x, _prec) {
  while(true) {
    var prec = _prec;
    var cellX = _cellX;
    if (cellX) {
      var t = cellX[1];
      var h = cellX[0];
      if (h[0] === x) {
        prec[1] = t;
        return /* () */0;
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
      return /* () */0;
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
    if (cellX) {
      if (cellY) {
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
    if (cellX) {
      if (cellY) {
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
    if (n) {
      if (cell) {
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
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function splitAtAux(_n, _cell, _prec) {
  while(true) {
    var prec = _prec;
    var cell = _cell;
    var n = _n;
    if (n) {
      if (cell) {
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
    } else {
      return /* Some */[cell];
    }
  };
}

function takeOpt(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else if (n) {
    if (lst) {
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
  } else {
    return /* Some */[/* [] */0];
  }
}

function dropOpt(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else {
    var _l = lst;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l = _l;
      if (n$1) {
        if (l) {
          _n = n$1 - 1 | 0;
          _l = l[1];
          continue ;
          
        } else {
          return /* None */0;
        }
      } else {
        return /* Some */[l];
      }
    };
  }
}

function splitAtOpt(lst, n) {
  if (n < 0) {
    return /* None */0;
  } else if (n) {
    if (lst) {
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
  } else {
    return /* Some */[/* tuple */[
              /* [] */0,
              lst
            ]];
  }
}

function append(xs, ys) {
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

function map(xs, f) {
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

function map2(l1, l2, f) {
  if (l1) {
    if (l2) {
      var cell = /* :: */[
        f(l1[0], l2[0]),
        /* [] */0
      ];
      copyAuxWithMap2(f, l1[1], l2[1], cell);
      return cell;
    } else {
      return /* [] */0;
    }
  } else {
    return /* [] */0;
  }
}

function mapi(xs, f) {
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

function init(n, f) {
  if (n < 0) {
    throw new Error("Invalid_argument");
  } else if (n) {
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
  } else {
    return /* [] */0;
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

function toArray(x) {
  var len = length(x);
  var arr = new Array(len);
  fillAux(arr, 0, x);
  return arr;
}

function fillAuxMap(arr, _i, _x, f) {
  while(true) {
    var x = _x;
    var i = _i;
    if (x) {
      arr[i] = f(x[0]);
      _x = x[1];
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function toJson(x, f) {
  var len = length(x);
  var arr = new Array(len);
  fillAuxMap(arr, 0, x, f);
  return arr;
}

function fromJson(j, f) {
  var match = Js_json.decodeArray(j);
  if (match) {
    var arr = match[0];
    var len = arr.length;
    if (len) {
      var head = /* :: */[
        f(arr[0]),
        /* [] */0
      ];
      var cell = head;
      for(var i = 1 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
        var next = /* :: */[
          f(arr[i]),
          /* [] */0
        ];
        cell[1] = next;
        cell = next;
      }
      return head;
    } else {
      return /* [] */0;
    }
  } else {
    throw new Error("Not array when decoding list");
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

function mapRev(l, f) {
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

function iter(_xs, f) {
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

function iteri(l, f) {
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

function foldLeft(_l, _accu, f) {
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

function foldRight(l, accu, f) {
  if (l) {
    return f(l[0], foldRight(l[1], accu, f));
  } else {
    return accu;
  }
}

function mapRev2(l1, l2, f) {
  var _l1 = l1;
  var _l2 = l2;
  var _accu = /* [] */0;
  var f$1 = f;
  while(true) {
    var accu = _accu;
    var l2$1 = _l2;
    var l1$1 = _l1;
    if (l1$1) {
      if (l2$1) {
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
    } else {
      return accu;
    }
  };
}

function iter2(_l1, _l2, f) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        f(l1[0], l2[0]);
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
        
      } else {
        return /* () */0;
      }
    } else {
      return /* () */0;
    }
  };
}

function foldLeft2(_l1, _l2, _accu, f) {
  while(true) {
    var accu = _accu;
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        _accu = f(accu, l1[0], l2[0]);
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
        
      } else {
        return accu;
      }
    } else {
      return accu;
    }
  };
}

function foldRight2(l1, l2, accu, f) {
  if (l1 && l2) {
    return f(l1[0], l2[0], foldRight2(l1[1], l2[1], accu, f));
  } else {
    return accu;
  }
}

function forAll(_xs, p) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (p(xs[0])) {
        _xs = xs[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function exists(_xs, p) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (p(xs[0])) {
        return /* true */1;
      } else {
        _xs = xs[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function forAll2(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (p(l1[0], l2[0])) {
          _l2 = l2[1];
          _l1 = l1[1];
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* true */1;
      }
    } else {
      return /* true */1;
    }
  };
}

function exists2(_l1, _l2, p) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (p(l1[0], l2[0])) {
          return /* true */1;
        } else {
          _l2 = l2[1];
          _l1 = l1[1];
          continue ;
          
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* false */0;
    }
  };
}

function mem(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (eq(xs[0], x)) {
        return /* true */1;
      } else {
        _xs = xs[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function memq(_xs, x) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (xs[0] === x) {
        return /* true */1;
      } else {
        _xs = xs[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function assocOpt(_xs, x, eq) {
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

function assqOpt(_xs, x) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var match = xs[0];
      if (match[0] === x) {
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

function memAssoc(_xs, x, eq) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (eq(xs[0][0], x)) {
        return /* true */1;
      } else {
        _xs = xs[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function memAssq(_xs, x) {
  while(true) {
    var xs = _xs;
    if (xs) {
      if (xs[0][0] === x) {
        return /* true */1;
      } else {
        _xs = xs[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function removeAssoc(xs, x, eq) {
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
      removeAssocAuxWithMap(l, x, cell, eq);
      return cell;
    }
  } else {
    return /* [] */0;
  }
}

function removeAssq(xs, x) {
  if (xs) {
    var l = xs[1];
    var pair = xs[0];
    if (pair[0] === x) {
      return l;
    } else {
      var cell = /* :: */[
        pair,
        /* [] */0
      ];
      removeAssocAux(l, x, cell);
      return cell;
    }
  } else {
    return /* [] */0;
  }
}

function findOpt(_xs, p) {
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

function filter(_xs, p) {
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

function partition(l, p) {
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
  if (l1) {
    if (l2) {
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
  } else {
    return /* [] */0;
  }
}

exports.headOpt = headOpt;
exports.tailOpt = tailOpt;
exports.nthOpt = nthOpt;
exports.nthAssert = nthAssert;
exports.dropOpt = dropOpt;
exports.takeOpt = takeOpt;
exports.splitAtOpt = splitAtOpt;
exports.append = append;
exports.map = map;
exports.map2 = map2;
exports.mapi = mapi;
exports.init = init;
exports.length = length;
exports.toArray = toArray;
exports.toJson = toJson;
exports.fromJson = fromJson;
exports.revAppend = revAppend;
exports.rev = rev;
exports.flatten = flatten;
exports.mapRev = mapRev;
exports.iter = iter;
exports.iteri = iteri;
exports.foldLeft = foldLeft;
exports.foldRight = foldRight;
exports.mapRev2 = mapRev2;
exports.iter2 = iter2;
exports.foldLeft2 = foldLeft2;
exports.foldRight2 = foldRight2;
exports.forAll = forAll;
exports.exists = exists;
exports.forAll2 = forAll2;
exports.exists2 = exists2;
exports.mem = mem;
exports.memq = memq;
exports.assocOpt = assocOpt;
exports.assqOpt = assqOpt;
exports.memAssoc = memAssoc;
exports.memAssq = memAssq;
exports.removeAssoc = removeAssoc;
exports.removeAssq = removeAssq;
exports.findOpt = findOpt;
exports.filter = filter;
exports.partition = partition;
exports.unzip = unzip;
exports.zip = zip;
/* No side effect */
