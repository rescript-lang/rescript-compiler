// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Mt                      = require("./mt");
var Mt_global               = require("./mt_global");
var Scanf                   = require("../stdlib/scanf");
var Printf                  = require("../stdlib/printf");
var Caml_curry              = require("../runtime/caml_curry");
var List                    = require("../stdlib/list");

var suites = [/* [] */0];

var test_id = [0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

function scan_rest(ib, accu) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Scan_char_set */{
                    0: /* None */0,
                    1: "\0\0\0\0\0\0\0\0\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                    2: /* End_of_format */0,
                    length: 3,
                    tag: 20
                  },
                  "%[]]"
                ]), function (param) {
              if (param === "]") {
                return accu;
              }
              else {
                var ib$1 = ib;
                var accu$1 = accu;
                return Caml_curry.app1(Scanf.bscanf(ib$1, /* Format */[
                                /* Char_literal */{
                                  0: /* " " */32,
                                  1: /* Int */{
                                    0: /* Int_i */3,
                                    1: /* No_padding */0,
                                    2: /* No_precision */0,
                                    3: /* Char_literal */{
                                      0: /* " " */32,
                                      1: /* End_of_format */0,
                                      length: 2,
                                      tag: 12
                                    },
                                    length: 4,
                                    tag: 4
                                  },
                                  length: 2,
                                  tag: 12
                                },
                                " %i "
                              ]), function (i) {
                            var ib$2 = ib$1;
                            var accu$2 = /* :: */[
                              i,
                              accu$1
                            ];
                            return Caml_curry.app1(Scanf.bscanf(ib$2, /* Format */[
                                            /* Scan_char_set */{
                                              0: /* Some */[1],
                                              1: "\0\0\0\0\0\0\0\b\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                              2: /* End_of_format */0,
                                              length: 3,
                                              tag: 20
                                            },
                                            "%1[];]"
                                          ]), function (param) {
                                        switch (param) {
                                          case ";" : 
                                              return scan_rest(ib$2, accu$2);
                                          case "]" : 
                                              return accu$2;
                                          default:
                                            var s = Printf.sprintf(/* Format */[
                                                  /* String_literal */{
                                                    0: "scan_int_list",
                                                    1: /* End_of_format */0,
                                                    length: 2,
                                                    tag: 11
                                                  },
                                                  "scan_int_list"
                                                ]);
                                            throw [
                                                  Caml_builtin_exceptions.failure,
                                                  s
                                                ];
                                        }
                                      });
                          });
              }
            });
}

function scan_int_list(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " [ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " [ "
          ]), /* () */0);
  return List.rev(scan_rest(ib, /* [] */0));
}

eq('File "scanf_reference_error_regression_test.ml", line 36, characters 5-12', /* tuple */[
      scan_int_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")),
      /* [] */0
    ]);

Mt.from_pair_suites("scanf_reference_error_regression_test.ml", suites[0]);

/*  Not a pure module */
