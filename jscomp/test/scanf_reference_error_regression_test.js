'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var Printf = require("../../lib/js/printf.js");
var Mt_global = require("./mt_global.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

function scan_rest(ib, accu) {
  return Curry._1(Scanf.bscanf(ib, /* Format */[
                  /* Scan_char_set */Block.__(20, [
                      undefined,
                      "\0\0\0\0\0\0\0\0\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      /* End_of_format */0
                    ]),
                  "%[]]"
                ]), (function (param) {
                if (param === "]") {
                  return accu;
                } else {
                  var ib$1 = ib;
                  var accu$1 = accu;
                  return Curry._1(Scanf.bscanf(ib$1, /* Format */[
                                  /* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* Int */Block.__(4, [
                                          /* Int_i */3,
                                          /* No_padding */0,
                                          /* No_precision */0,
                                          /* Char_literal */Block.__(12, [
                                              /* " " */32,
                                              /* End_of_format */0
                                            ])
                                        ])
                                    ]),
                                  " %i "
                                ]), (function (i) {
                                var ib$2 = ib$1;
                                var accu$2 = /* :: */[
                                  i,
                                  accu$1
                                ];
                                return Curry._1(Scanf.bscanf(ib$2, /* Format */[
                                                /* Scan_char_set */Block.__(20, [
                                                    1,
                                                    "\0\0\0\0\0\0\0\b\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                                    /* End_of_format */0
                                                  ]),
                                                "%1[];]"
                                              ]), (function (param) {
                                              switch (param) {
                                                case ";" :
                                                    return scan_rest(ib$2, accu$2);
                                                case "]" :
                                                    return accu$2;
                                                default:
                                                  var s = Printf.sprintf(/* Format */[
                                                        /* String_literal */Block.__(11, [
                                                            "scan_int_list",
                                                            /* End_of_format */0
                                                          ]),
                                                        "scan_int_list"
                                                      ]);
                                                  throw [
                                                        Caml_builtin_exceptions.failure,
                                                        s
                                                      ];
                                              }
                                            }));
                              }));
                }
              }));
}

function scan_int_list(ib) {
  Curry._1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */Block.__(11, [
                " [ ",
                /* End_of_format */0
              ]),
            " [ "
          ]), /* () */0);
  return List.rev(scan_rest(ib, /* [] */0));
}

eq("File \"scanf_reference_error_regression_test.ml\", line 36, characters 5-12", /* tuple */[
      scan_int_list(Scanf.Scanning.from_string("[]")),
      /* [] */0
    ]);

Mt.from_pair_suites("Scanf_reference_error_regression_test", suites.contents);

/*  Not a pure module */
