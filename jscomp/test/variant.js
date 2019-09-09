'use strict';

var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function foo(param) {
  if (typeof param === "number") {
    if (param === /* A1 */0) {
      return 1;
    } else {
      return 2;
    }
  } else {
    switch (param.tag | 0) {
      case /* B */0 :
          return param[0];
      case /* C */1 :
          return param[0] + param[1] | 0;
      case /* D */2 :
          var match = param[0];
          return match[0] + match[1] | 0;
      
    }
  }
}

function fooA1(param) {
  if (typeof param === "number" && param === 0) {
    return 1;
  } else {
    return 42;
  }
}

function fooC(param) {
  if (typeof param === "number" || param.tag !== /* C */1) {
    return 42;
  } else {
    return param[0] + param[1] | 0;
  }
}

function switchNum(param) {
  switch (param) {
    case 0 :
        return "0";
    case 1 :
        return "1";
    case 2 :
        return "2";
    default:
      return "_";
  }
}

var same = Caml_obj.caml_equal;

var compare = Caml_obj.caml_compare;

var Path = {
  same: same,
  compare: compare
};

function Make(M) {
  var find = function (x) {
    return /* () */0;
  };
  return {
          find: find
        };
}

function find(x) {
  return /* () */0;
}

var M = {
  find: find
};

function rollback_path(subst, p) {
  try {
    return "try";
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      switch (p.tag | 0) {
        case /* Pdot */1 :
            return "Pdot";
        case /* Pident */0 :
        case /* Papply */2 :
            return "Pident | Papply";
        
      }
    } else {
      throw exn;
    }
  }
}

var a1 = /* A1 */0;

var a2 = /* A2 */1;

var b = /* B */Block.__(0, [34]);

var c = /* C */Block.__(1, [
    4,
    2
  ]);

var d = /* D */Block.__(2, [/* tuple */[
      4,
      2
    ]]);

exports.a1 = a1;
exports.a2 = a2;
exports.b = b;
exports.c = c;
exports.d = d;
exports.foo = foo;
exports.fooA1 = fooA1;
exports.fooC = fooC;
exports.switchNum = switchNum;
exports.Path = Path;
exports.Make = Make;
exports.M = M;
exports.rollback_path = rollback_path;
/* No side effect */
