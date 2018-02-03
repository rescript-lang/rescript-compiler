'use strict';

var Curry = require("./curry.js");

function forEachU(s, f, action) {
  for(var i = s; i <= f; ++i){
    action(i);
  }
  return /* () */0;
}

function forEach(s, f, action) {
  return forEachU(s, f, Curry.__1(action));
}

function everyU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return /* true */1;
    } else if (p(s)) {
      _s = s + 1 | 0;
      continue ;
      
    } else {
      return /* false */0;
    }
  };
}

function every(s, f, p) {
  return everyU(s, f, Curry.__1(p));
}

function everyByU(_s, f, step, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return /* true */1;
    } else if (p(s)) {
      _s = s + step | 0;
      continue ;
      
    } else {
      return /* false */0;
    }
  };
}

function everyBy(s, f, step, p) {
  return everyByU(s, f, step, Curry.__1(p));
}

function someU(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return /* false */0;
    } else if (p(s)) {
      return /* true */1;
    } else {
      _s = s + 1 | 0;
      continue ;
      
    }
  };
}

function some(s, f, p) {
  return someU(s, f, Curry.__1(p));
}

function someByU(_s, f, step, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return /* false */0;
    } else if (p(s)) {
      return /* true */1;
    } else {
      _s = s + step | 0;
      continue ;
      
    }
  };
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
