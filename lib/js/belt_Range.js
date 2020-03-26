'use strict';

var Curry = require("./curry.js");

function forEachU(s, f, action) {
  for(var i = s; i <= f; ++i){
    action(i);
  }
  
}

function forEach(s, f, action) {
  return forEachU(s, f, Curry.__1(action));
}

function everyU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return true;
    }
    if (!p(s)) {
      return false;
    }
    _s = s + 1 | 0;
    continue ;
  };
}

function every(s, f, p) {
  return everyU(s, f, Curry.__1(p));
}

function everyByU(s, f, step, p) {
  if (step > 0) {
    var _s = s;
    while(true) {
      var s$1 = _s;
      if (s$1 > f) {
        return true;
      }
      if (!p(s$1)) {
        return false;
      }
      _s = s$1 + step | 0;
      continue ;
    };
  } else {
    return true;
  }
}

function everyBy(s, f, step, p) {
  return everyByU(s, f, step, Curry.__1(p));
}

function someU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return false;
    }
    if (p(s)) {
      return true;
    }
    _s = s + 1 | 0;
    continue ;
  };
}

function some(s, f, p) {
  return someU(s, f, Curry.__1(p));
}

function someByU(s, f, step, p) {
  if (step > 0) {
    var _s = s;
    while(true) {
      var s$1 = _s;
      if (s$1 > f) {
        return false;
      }
      if (p(s$1)) {
        return true;
      }
      _s = s$1 + step | 0;
      continue ;
    };
  } else {
    return false;
  }
}

function someBy(s, f, step, p) {
  return someByU(s, f, step, Curry.__1(p));
}

exports.forEachU = forEachU;
exports.forEach = forEach;
exports.everyU = everyU;
exports.every = every;
exports.everyByU = everyByU;
exports.everyBy = everyBy;
exports.someU = someU;
exports.some = some;
exports.someByU = someByU;
exports.someBy = someBy;
/* No side effect */
