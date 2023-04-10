'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function foo(x) {
  if (typeof x !== "object") {
    if (x === "A1") {
      return 1;
    } else {
      return 2;
    }
  }
  switch (x.TAG) {
    case "B" :
        return x._0;
    case "C" :
        return x._0 + x._1 | 0;
    case "D" :
        var match = x._0;
        return match[0] + match[1] | 0;
    
  }
}

function fooA1(x) {
  if (typeof x !== "object" && x === "A1") {
    return 1;
  } else {
    return 42;
  }
}

function fooC(x) {
  if (typeof x !== "object" || x.TAG !== "C") {
    return 42;
  } else {
    return x._0 + x._1 | 0;
  }
}

function switchNum(x) {
  switch (x) {
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

var same = Caml_obj.equal;

var compare = Caml_obj.compare;

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
      switch (p.TAG) {
        case "Pdot" :
            return "Pdot";
        case "Pident" :
        case "Papply" :
            return "Pident | Papply";
        
      }
    } else {
      throw exn;
    }
  }
}

var EA1 = /* @__PURE__ */Caml_exceptions.create("Variant.EA1");

var EA2 = /* @__PURE__ */Caml_exceptions.create("Variant.EA2");

var EB = /* @__PURE__ */Caml_exceptions.create("Variant.EB");

var EC = /* @__PURE__ */Caml_exceptions.create("Variant.EC");

var ED = /* @__PURE__ */Caml_exceptions.create("Variant.ED");

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

var a1 = "A1";

var a2 = "A2";

var b = {
  TAG: "B",
  _0: 34
};

var c = {
  TAG: "C",
  _0: 4,
  _1: 2
};

var d = {
  TAG: "D",
  _0: [
    4,
    2
  ]
};

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
