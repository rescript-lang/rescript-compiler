'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function foo(n) {
  if (typeof n === "number") {
    if (n === /* A1 */0) {
      return 1;
    } else {
      return 2;
    }
  }
  switch (n.tag | 0) {
    case /* B */0 :
        return n[0];
    case /* C */1 :
        return n[0] + n[1] | 0;
    case /* D */2 :
        var match = n[0];
        return match[0] + match[1] | 0;
    
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
    
  };
  return {
          find: find
        };
}

function find(x) {
  
}

var M = {
  find: find
};

function rollback_path(subst, p) {
  try {
    return "try";
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
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

var EA1 = Caml_exceptions.create("Variant.EA1");

var EA2 = Caml_exceptions.create("Variant.EA2");

var EB = Caml_exceptions.create("Variant.EB");

var EC = Caml_exceptions.create("Variant.EC");

var ED = Caml_exceptions.create("Variant.ED");

function fooExn(f) {
  try {
    return Curry._1(f, undefined);
  }
  catch (raw_n){
    var n = Caml_js_exceptions.internalToOCamlException(raw_n);
    if (n.RE_EXN_ID === EA1) {
      return 1;
    }
    if (n.RE_EXN_ID === EA2) {
      return 2;
    }
    if (n.RE_EXN_ID === EB) {
      return n._1;
    }
    if (n.RE_EXN_ID === EC) {
      return n._1 + n._2 | 0;
    }
    if (n.RE_EXN_ID === ED) {
      var match = n._1;
      return match[0] + match[1] | 0;
    }
    throw n;
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
exports.EA1 = EA1;
exports.EA2 = EA2;
exports.EB = EB;
exports.EC = EC;
exports.ED = ED;
exports.fooExn = fooExn;
/* No side effect */
