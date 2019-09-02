'use strict';

var Js_vector = require("./js_vector.js");
var Caml_option = require("./caml_option.js");

function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (param !== "[]") {
      _param = param.Arg1;
      _len = len + 1 | 0;
      continue ;
    } else {
      return len;
    }
  };
}

function cons(x, xs) {
  return /* constructor */{
          tag: "::",
          Arg0: x,
          Arg1: xs
        };
}

function isEmpty(x) {
  return x === "[]";
}

function hd(param) {
  if (param !== "[]") {
    return Caml_option.some(param.Arg0);
  }
  
}

function tl(param) {
  if (param !== "[]") {
    return param.Arg1;
  }
  
}

function nth(l, n) {
  if (n < 0) {
    return ;
  } else {
    var _l = l;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l$1 = _l;
      if (l$1 !== "[]") {
        if (n$1 === 0) {
          return Caml_option.some(l$1.Arg0);
        } else {
          _n = n$1 - 1 | 0;
          _l = l$1.Arg1;
          continue ;
        }
      } else {
        return ;
      }
    };
  }
}

function revAppend(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      _l2 = /* constructor */{
        tag: "::",
        Arg0: l1.Arg0,
        Arg1: l2
      };
      _l1 = l1.Arg1;
      continue ;
    } else {
      return l2;
    }
  };
}

function rev(l) {
  return revAppend(l, "[]");
}

function mapRevAux(f, _acc, _ls) {
  while(true) {
    var ls = _ls;
    var acc = _acc;
    if (ls !== "[]") {
      _ls = ls.Arg1;
      _acc = /* constructor */{
        tag: "::",
        Arg0: f(ls.Arg0),
        Arg1: acc
      };
      continue ;
    } else {
      return acc;
    }
  };
}

function mapRev(f, ls) {
  return mapRevAux(f, "[]", ls);
}

function map(f, ls) {
  return revAppend(mapRevAux(f, "[]", ls), "[]");
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      f(param.Arg0);
      _param = param.Arg1;
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
    if (param !== "[]") {
      f$1(i, param.Arg0);
      _param = param.Arg1;
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
    if (l !== "[]") {
      _l = l.Arg1;
      _accu = f(accu, l.Arg0);
      continue ;
    } else {
      return accu;
    }
  };
}

function tailLoop(f, _acc, _param) {
  while(true) {
    var param = _param;
    var acc = _acc;
    if (param !== "[]") {
      _param = param.Arg1;
      _acc = f(param.Arg0, acc);
      continue ;
    } else {
      return acc;
    }
  };
}

function foldRight(f, l, init) {
  var loop = function (n, param) {
    if (param !== "[]") {
      var t = param.Arg1;
      var h = param.Arg0;
      if (n < 1000) {
        return f(h, loop(n + 1 | 0, t));
      } else {
        return f(h, tailLoop(f, init, revAppend(t, "[]")));
      }
    } else {
      return init;
    }
  };
  return loop(0, l);
}

function flatten(lx) {
  var _acc = "[]";
  var _lx = lx;
  while(true) {
    var lx$1 = _lx;
    var acc = _acc;
    if (lx$1 !== "[]") {
      _lx = lx$1.Arg1;
      _acc = revAppend(lx$1.Arg0, acc);
      continue ;
    } else {
      return revAppend(acc, "[]");
    }
  };
}

function filterRevAux(f, _acc, _xs) {
  while(true) {
    var xs = _xs;
    var acc = _acc;
    if (xs !== "[]") {
      var ys = xs.Arg1;
      var y = xs.Arg0;
      var match = f(y);
      _xs = ys;
      if (match) {
        _acc = /* constructor */{
          tag: "::",
          Arg0: y,
          Arg1: acc
        };
        continue ;
      } else {
        continue ;
      }
    } else {
      return acc;
    }
  };
}

function filter(f, xs) {
  return revAppend(filterRevAux(f, "[]", xs), "[]");
}

function filterMapRevAux(f, _acc, _xs) {
  while(true) {
    var xs = _xs;
    var acc = _acc;
    if (xs !== "[]") {
      var ys = xs.Arg1;
      var match = f(xs.Arg0);
      _xs = ys;
      if (match !== undefined) {
        _acc = /* constructor */{
          tag: "::",
          Arg0: Caml_option.valFromOption(match),
          Arg1: acc
        };
        continue ;
      } else {
        continue ;
      }
    } else {
      return acc;
    }
  };
}

function filterMap(f, xs) {
  return revAppend(filterMapRevAux(f, "[]", xs), "[]");
}

function countBy(f, xs) {
  var f$1 = f;
  var _acc = 0;
  var _xs = xs;
  while(true) {
    var xs$1 = _xs;
    var acc = _acc;
    if (xs$1 !== "[]") {
      _xs = xs$1.Arg1;
      _acc = f$1(xs$1.Arg0) ? acc + 1 | 0 : acc;
      continue ;
    } else {
      return acc;
    }
  };
}

function init(n, f) {
  return Js_vector.toList(Js_vector.init(n, f));
}

function toVector(xs) {
  if (xs !== "[]") {
    var a = new Array(length(xs));
    var _i = 0;
    var _param = xs;
    while(true) {
      var param = _param;
      var i = _i;
      if (param !== "[]") {
        a[i] = param.Arg0;
        _param = param.Arg1;
        _i = i + 1 | 0;
        continue ;
      } else {
        return a;
      }
    };
  } else {
    return /* array */[];
  }
}

function equal(cmp, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs !== "[]") {
      if (ys !== "[]" && cmp(xs.Arg0, ys.Arg0)) {
        _ys = ys.Arg1;
        _xs = xs.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return ys === "[]";
    }
  };
}

exports.length = length;
exports.cons = cons;
exports.isEmpty = isEmpty;
exports.hd = hd;
exports.tl = tl;
exports.nth = nth;
exports.revAppend = revAppend;
exports.rev = rev;
exports.mapRev = mapRev;
exports.map = map;
exports.iter = iter;
exports.iteri = iteri;
exports.foldLeft = foldLeft;
exports.foldRight = foldRight;
exports.flatten = flatten;
exports.filter = filter;
exports.filterMap = filterMap;
exports.countBy = countBy;
exports.init = init;
exports.toVector = toVector;
exports.equal = equal;
/* No side effect */
