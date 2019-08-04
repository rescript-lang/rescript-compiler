'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(x) {
  return x + 1 | 0;
}

function f_0(x) {
  return x - 1 | 0;
}

function f2(param) {
  if (param >= 3) {
    return /* T003 */3;
  } else {
    return param;
  }
}

function f3(param) {
  return param;
}

function f4(param) {
  return 3;
}

function f5(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return 1;
      case 1 : 
          return 3;
      case 2 : 
          return 4;
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 1;
      case 2 : 
          return 2;
      
    }
  }
}

function f6(param) {
  if (typeof param === "number") {
    if (param >= 2) {
      return 2;
    } else {
      return 0;
    }
  } else {
    return 1;
  }
}

function f7(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return 1;
      case 1 : 
          return 2;
      case 2 : 
          return -1;
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return 3;
      case 1 : 
          return 4;
      case 2 : 
          return -1;
      
    }
  }
}

function f8(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
      case 1 : 
          return 1;
      default:
        return 3;
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 2;
      default:
        return 3;
    }
  }
}

function f9(param) {
  if (typeof param === "number") {
    if (param === 3) {
      return 3;
    } else {
      return 1;
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 2;
      case 2 : 
      case 3 : 
          return 3;
      
    }
  }
}

function f10(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return 0;
      case 1 : 
          return 2;
      case 2 : 
          return 4;
      case 3 : 
          return 1;
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 2;
      case 2 : 
      case 3 : 
          return 3;
      
    }
  }
}

function f11(x) {
  if (typeof x === "number") {
    return 2;
  } else if (x.tag) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "adt_optimize_test.ml",
            191,
            9
          ]
        ];
  } else {
    return 1;
  }
}

exports.f = f;
exports.f_0 = f_0;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
/* No side effect */
