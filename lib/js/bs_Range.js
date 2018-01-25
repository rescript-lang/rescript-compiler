'use strict';


function every(_s, f, p) {
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

function everyBy(_s, f, step, p) {
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

function some(_s, f, p) {
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

function someBy(_s, f, step, p) {
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

exports.every = every;
exports.everyBy = everyBy;
exports.some = some;
exports.someBy = someBy;
/* No side effect */
