'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Printexc = require("../../lib/js/printexc.js");
var Exception_def = require("./exception_def.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

var Hi = Caml_exceptions.create("Exception_repr_test.Hi");

var Hello = Caml_exceptions.create("Exception_repr_test.Hello");

var A = Caml_exceptions.create("Exception_repr_test.A");

Printexc.register_printer(function (s) {
      if (s.RE_EXN_ID === Hi) {
        return "hey";
      } else if (s.RE_EXN_ID === A) {
        return Curry._1(Format.asprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "A(",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* Char_literal */12,
                              _0: /* ')' */41,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "A(%d)"
                      }), s._1);
      } else {
        return ;
      }
    });

eq("File \"exception_repr_test.ml\", line 24, characters 7-14", "hey", Printexc.to_string({
          RE_EXN_ID: Hi
        }));

eq("File \"exception_repr_test.ml\", line 25, characters 7-14", "A(1)", Printexc.to_string({
          RE_EXN_ID: A,
          _1: 1
        }));

eq("File \"exception_repr_test.ml\", line 26, characters 7-14", Printexc.to_string({
            RE_EXN_ID: Hello
          }).startsWith("Exception_repr_test.Hello"), true);

eq("File \"exception_repr_test.ml\", line 27, characters 7-14", "A", Printexc.to_string({
          RE_EXN_ID: Exception_def.A,
          _1: 3
        }));

Mt.from_pair_suites("Exception_repr_test", suites.contents);

var AAA = Exception_def.A;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Hi = Hi;
exports.Hello = Hello;
exports.A = A;
exports.AAA = AAA;
/*  Not a pure module */
