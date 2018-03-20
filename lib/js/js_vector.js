'use strict';


function filterInPlace(p, a) {
  var i = 0;
  var j = 0;
  while(i < a.length) {
    var v = a[i];
    if (p(v)) {
      a[j] = v;
      j = j + 1 | 0;
    }
    i = i + 1 | 0;
  };
  a.splice(j);
  return /* () */0;
}

function empty(a) {
  a.splice(0);
  return /* () */0;
}

function pushBack(x, xs) {
  xs.push(x);
  return /* () */0;
}

function memByRef(x, xs) {
  return xs.indexOf(x) >= 0;
}

function iter(f, xs) {
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    f(xs[i]);
  }
  return /* () */0;
}

function iteri(f, a) {
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    f(i, a[i]);
  }
  return /* () */0;
}

function toList(a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    } else {
      _res = /* :: */[
        a[i],
        res
      ];
      _i = i - 1 | 0;
      continue ;
    }
  };
}

function init(n, f) {
  var v = new Array(n);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    v[i] = f(i);
  }
  return v;
}

function copy(x) {
  var len = x.length;
  var b = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    b[i] = x[i];
  }
  return b;
}

function map(f, a) {
  var l = a.length;
  var r = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    r[i] = f(a[i]);
  }
  return r;
}

function foldLeft(f, x, a) {
  var r = x;
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    r = f(r, a[i]);
  }
  return r;
}

function foldRight(f, a, x) {
  var r = x;
  for(var i = a.length - 1 | 0; i >= 0; --i){
    r = f(a[i], r);
  }
  return r;
}

function mapi(f, a) {
  var l = a.length;
  if (l === 0) {
    return /* array */[];
  } else {
    var r = new Array(l);
    for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      r[i] = f(i, a[i]);
    }
    return r;
  }
}

function append(x, a) {
  return a.concat(/* array */[x]);
}

exports.filterInPlace = filterInPlace;
exports.empty = empty;
exports.pushBack = pushBack;
exports.copy = copy;
exports.memByRef = memByRef;
exports.iter = iter;
exports.iteri = iteri;
exports.toList = toList;
exports.map = map;
exports.mapi = mapi;
exports.foldLeft = foldLeft;
exports.foldRight = foldRight;
exports.init = init;
exports.append = append;
/* No side effect */
