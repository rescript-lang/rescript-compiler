'use strict';


function forAll(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return true;
    } else if (p(s)) {
      _s = s + 1 | 0;
      continue ;
      
    } else {
      return false;
    }
  };
}

function forAllBy(_s, f, step, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return true;
    } else if (p(s)) {
      _s = s + step | 0;
      continue ;
      
    } else {
      return false;
    }
  };
}

function exists(_s, f, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return false;
    } else if (p(s)) {
      return true;
    } else {
      _s = s + 1 | 0;
      continue ;
      
    }
  };
}

function existsBy(_s, f, step, p) {
  while(true) {
    var s = _s;
    if (s > f) {
      return false;
    } else if (p(s)) {
      return true;
    } else {
      _s = s + step | 0;
      continue ;
      
    }
  };
}

exports.forAll = forAll;
exports.forAllBy = forAllBy;
exports.exists = exists;
exports.existsBy = existsBy;
/* No side effect */
