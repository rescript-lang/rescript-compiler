'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var counter = {
  contents: 0
};

function add_test(loc, test) {
  counter.contents = counter.contents + 1 | 0;
  var id = loc + (" id " + String(counter.contents));
  suites.contents = {
    hd: [
      id,
      test
    ],
    tl: suites.contents
  };
  
}

function eq(loc, x, y) {
  return add_test(loc, (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: x,
                        _1: y
                      };
              }));
}

function false_(loc) {
  return add_test(loc, (function (param) {
                return {
                        TAG: /* Ok */4,
                        _0: false
                      };
              }));
}

function true_(loc) {
  return add_test(loc, (function (param) {
                return {
                        TAG: /* Ok */4,
                        _0: true
                      };
              }));
}

var exit = 0;

var e;

try {
  e = JSON.parse(" {\"x\"}");
  exit = 1;
}
catch (raw_x){
  var x = Caml_js_exceptions.internalToOCamlException(raw_x);
  if (x.RE_EXN_ID === Js_exn.$$Error) {
    add_test("File \"js_exception_catch_test.ml\", line 21, characters 10-17", (function (param) {
            return {
                    TAG: /* Ok */4,
                    _0: true
                  };
          }));
  } else {
    throw x;
  }
}

if (exit === 1) {
  add_test("File \"js_exception_catch_test.ml\", line 22, characters 16-23", (function (param) {
          return {
                  TAG: /* Ok */4,
                  _0: false
                };
        }));
}

var A = /* @__PURE__ */Caml_exceptions.create("Js_exception_catch_test.A");

var B = /* @__PURE__ */Caml_exceptions.create("Js_exception_catch_test.B");

var C = /* @__PURE__ */Caml_exceptions.create("Js_exception_catch_test.C");

function test(f) {
  try {
    Curry._1(f, undefined);
    return "No_error";
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e.RE_EXN_ID === "Not_found") {
      return "Not_found";
    } else if (e.RE_EXN_ID === "Invalid_argument") {
      if (e._1 === "x") {
        return "Invalid_argument";
      } else {
        return "Invalid_any";
      }
    } else if (e.RE_EXN_ID === A) {
      if (e._1 !== 2) {
        return "A_any";
      } else {
        return "A2";
      }
    } else if (e.RE_EXN_ID === B) {
      return "B";
    } else if (e.RE_EXN_ID === C) {
      if (e._1 !== 1 || e._2 !== 2) {
        return "C_any";
      } else {
        return "C";
      }
    } else if (e.RE_EXN_ID === Js_exn.$$Error) {
      return "Js_error";
    } else {
      return "Any";
    }
  }
}

eq("File \"js_exception_catch_test.ml\", line 43, characters 5-12", test(function (param) {
          
        }), "No_error");

eq("File \"js_exception_catch_test.ml\", line 44, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
        }), "Not_found");

eq("File \"js_exception_catch_test.ml\", line 45, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "x",
                Error: new Error()
              };
        }), "Invalid_argument");

eq("File \"js_exception_catch_test.ml\", line 46, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "",
                Error: new Error()
              };
        }), "Invalid_any");

eq("File \"js_exception_catch_test.ml\", line 47, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: A,
                _1: 2,
                Error: new Error()
              };
        }), "A2");

eq("File \"js_exception_catch_test.ml\", line 48, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: A,
                _1: 3,
                Error: new Error()
              };
        }), "A_any");

eq("File \"js_exception_catch_test.ml\", line 49, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: B,
                Error: new Error()
              };
        }), "B");

eq("File \"js_exception_catch_test.ml\", line 50, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: C,
                _1: 1,
                _2: 2,
                Error: new Error()
              };
        }), "C");

eq("File \"js_exception_catch_test.ml\", line 51, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: C,
                _1: 0,
                _2: 2,
                Error: new Error()
              };
        }), "C_any");

eq("File \"js_exception_catch_test.ml\", line 52, characters 5-12", test(function (param) {
          throw new Error("x");
        }), "Js_error");

eq("File \"js_exception_catch_test.ml\", line 53, characters 5-12", test(function (param) {
          throw {
                RE_EXN_ID: "Failure",
                _1: "x",
                Error: new Error()
              };
        }), "Any");

Mt.from_pair_suites("Js_exception_catch_test", suites.contents);

exports.suites = suites;
exports.add_test = add_test;
exports.eq = eq;
exports.false_ = false_;
exports.true_ = true_;
exports.A = A;
exports.B = B;
exports.C = C;
exports.test = test;
/*  Not a pure module */
