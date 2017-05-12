'use strict';

var List = require("./list.js");

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
  return +(xs.indexOf(x) >= 0);
}

function iter(f, xs) {
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    f(xs[i]);
  }
  return /* () */0;
}

function ofList(xs) {
  if (xs) {
    var a = new Array(List.length(xs));
    var _i = 0;
    var _param = xs;
    while(true) {
      var param = _param;
      var i = _i;
      if (param) {
        a[i] = param[0];
        _param = param[1];
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

function map(f, a) {
  var l = a.length;
  var r = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    r[i] = f(a[i]);
  }
  return r;
}

exports.filterInPlace = filterInPlace;
exports.empty         = empty;
exports.pushBack      = pushBack;
exports.memByRef      = memByRef;
exports.iter          = iter;
exports.ofList        = ofList;
exports.map           = map;
/* No side effect */
