'use strict';


function add(x, y) {
  return x + y | 0;
}

var N = {
  add
};

function f1(param) {
  
}

function f2(param, param$1) {
  
}

function f3(param, param$1, param$2) {
  
}

var N0 = {
  f1,
  f2,
  f3
};

function f2$1(param, param$1) {
  
}

function f3$1(param, param$1, param$2) {
  
}

var N1 = {
  f2: f2$1,
  f3: f3$1
};

exports.N = N;
exports.N0 = N0;
exports.N1 = N1;
/* No side effect */
