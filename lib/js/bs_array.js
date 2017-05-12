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
  return +(xs.indexOf(x) >= 0);
}

function iter(f, xs) {
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    f(xs[i]);
  }
  return /* () */0;
}

exports.filterInPlace = filterInPlace;
exports.empty         = empty;
exports.pushBack      = pushBack;
exports.memByRef      = memByRef;
exports.iter          = iter;
/* No side effect */
