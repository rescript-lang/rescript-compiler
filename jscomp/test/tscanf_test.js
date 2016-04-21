// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64              = require("../runtime/caml_int64");
var Caml_obj                = require("../runtime/caml_obj");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Bytes                   = require("../stdlib/bytes");
var Pervasives              = require("../stdlib/pervasives");
var Caml_format             = require("../runtime/caml_format");
var Mt                      = require("./mt");
var Testing                 = require("./testing");
var Mt_global               = require("./mt_global");
var Scanf                   = require("../stdlib/scanf");
var Printf                  = require("../stdlib/printf");
var Buffer                  = require("../stdlib/buffer");
var Caml_curry              = require("../runtime/caml_curry");
var $$String                = require("../stdlib/string");
var List                    = require("../stdlib/list");
var Caml_string             = require("../runtime/caml_string");

var suites = [/* [] */0];

var test_id = [0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

function test(loc, b) {
  return eq(loc, /* tuple */[
              b,
              /* true */1
            ]);
}

function id(x) {
  return x;
}

function test0() {
  return ((((Caml_curry.app2(Scanf.sscanf("", /* Format */[
                          /* End_of_format */0,
                          ""
                        ]), id, 1) + Caml_curry.app2(Scanf.sscanf("", /* Format */[
                          /* Char_literal */{
                            0: /* " " */32,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          " "
                        ]), id, 2) | 0) + Caml_curry.app2(Scanf.sscanf(" ", /* Format */[
                        /* Char_literal */{
                          0: /* " " */32,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 12
                        },
                        " "
                      ]), id, 3) | 0) + Caml_curry.app2(Scanf.sscanf("\t", /* Format */[
                      /* Char_literal */{
                        0: /* " " */32,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      " "
                    ]), id, 4) | 0) + Caml_curry.app2(Scanf.sscanf("\n", /* Format */[
                    /* Char_literal */{
                      0: /* " " */32,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    " "
                  ]), id, 5) | 0) + Caml_curry.app1(Scanf.sscanf("\n\t 6", /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  " %d"
                ]), id) | 0;
}

test('File "tscanf_test.ml", line 42, characters 5-12', +(test0(/* () */0) === 21));

function test1() {
  return (((Caml_curry.app1(Scanf.sscanf("1", /* Format */[
                        /* Int */{
                          0: /* Int_d */0,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 4
                        },
                        "%d"
                      ]), id) + Caml_curry.app1(Scanf.sscanf(" 2", /* Format */[
                        /* Char_literal */{
                          0: /* " " */32,
                          1: /* Int */{
                            0: /* Int_d */0,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* End_of_format */0,
                            length: 4,
                            tag: 4
                          },
                          length: 2,
                          tag: 12
                        },
                        " %d"
                      ]), id) | 0) + Caml_curry.app1(Scanf.sscanf(" -2", /* Format */[
                      /* Char_literal */{
                        0: /* " " */32,
                        1: /* Int */{
                          0: /* Int_d */0,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 4
                        },
                        length: 2,
                        tag: 12
                      },
                      " %d"
                    ]), id) | 0) + Caml_curry.app1(Scanf.sscanf(" +2", /* Format */[
                    /* Char_literal */{
                      0: /* " " */32,
                      1: /* Int */{
                        0: /* Int_d */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      length: 2,
                      tag: 12
                    },
                    " %d"
                  ]), id) | 0) + Caml_curry.app1(Scanf.sscanf(" 2a ", /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "a" */97,
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
                  " %da"
                ]), id) | 0;
}

test('File "tscanf_test.ml", line 54, characters 5-12', +(test1(/* () */0) === 5));

function test2() {
  return (Caml_curry.app1(Scanf.sscanf("123", /* Format */[
                    /* Int */{
                      0: /* Int_i */3,
                      1: /* Lit_padding */{
                        0: /* Right */1,
                        1: 2,
                        length: 2,
                        tag: 0
                      },
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    "%2i"
                  ]), id) + Caml_curry.app1(Scanf.sscanf("245", /* Format */[
                    /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    "%d"
                  ]), id) | 0) + Caml_curry.app1(Scanf.sscanf(" 2a ", /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* Lit_padding */{
                        0: /* Right */1,
                        1: 1,
                        length: 2,
                        tag: 0
                      },
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "a" */97,
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
                  " %1da"
                ]), id) | 0;
}

test('File "tscanf_test.ml", line 63, characters 5-12', +(test2(/* () */0) === 259));

function test3() {
  return ((Caml_curry.app1(Scanf.sscanf("0xff", /* Format */[
                      /* Int */{
                        0: /* Int_i */3,
                        1: /* Lit_padding */{
                          0: /* Right */1,
                          1: 3,
                          length: 2,
                          tag: 0
                        },
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      "%3i"
                    ]), id) + Caml_curry.app1(Scanf.sscanf("0XEF", /* Format */[
                      /* Int */{
                        0: /* Int_i */3,
                        1: /* Lit_padding */{
                          0: /* Right */1,
                          1: 3,
                          length: 2,
                          tag: 0
                        },
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      "%3i"
                    ]), id) | 0) + Caml_curry.app1(Scanf.sscanf("x=-245", /* Format */[
                    /* String_literal */{
                      0: " x = ",
                      1: /* Int */{
                        0: /* Int_d */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      length: 2,
                      tag: 11
                    },
                    " x = %d"
                  ]), id) | 0) + Caml_curry.app1(Scanf.sscanf(" 2a ", /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* Lit_padding */{
                        0: /* Right */1,
                        1: 1,
                        length: 2,
                        tag: 0
                      },
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "a" */97,
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
                  " %1da"
                ]), id) | 0;
}

test('File "tscanf_test.ml", line 73, characters 5-12', +(test3(/* () */0) === -214));

function test4() {
  if (Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "-1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === -1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "+1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1."), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], ".1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 0.1);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "-.1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === -0.1);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "+.1"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 0.1);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "+1."), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "-1."), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]), function (b0) {
          return +(b0 === -1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "0 1. 1.3"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* " " */32,
                  1: /* Float */{
                    0: /* Float_f */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Float */{
                        0: /* Float_f */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 8
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 8
                  },
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 8
              },
              "%f %f %f"
            ]), function (b0, b1, b2) {
          return b0 === 0.0 && b1 === 1.0 ? +(b2 === 1.3) : /* false */0;
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "0.113"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* Lit_padding */{
                  0: /* Right */1,
                  1: 4,
                  length: 2,
                  tag: 0
                },
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%4f"
            ]), function (b0) {
          return +(b0 === 0.11);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "0.113"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* Lit_padding */{
                  0: /* Right */1,
                  1: 5,
                  length: 2,
                  tag: 0
                },
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%5f"
            ]), function (b0) {
          return +(b0 === 0.113);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "000.113"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* Lit_padding */{
                  0: /* Right */1,
                  1: 15,
                  length: 2,
                  tag: 0
                },
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%15f"
            ]), function (b0) {
          return +(b0 === 0.113);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "+000.113"), /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* Lit_padding */{
                  0: /* Right */1,
                  1: 15,
                  length: 2,
                  tag: 0
                },
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%15f"
            ]), function (b0) {
          return +(b0 === 0.113);
        })) {
    return Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "-000.113"), /* Format */[
                    /* Float */{
                      0: /* Float_f */0,
                      1: /* Lit_padding */{
                        0: /* Right */1,
                        1: 15,
                        length: 2,
                        tag: 0
                      },
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 8
                    },
                    "%15f"
                  ]), function (b0) {
                return +(b0 === -0.113);
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 110, characters 5-12', test4(/* () */0));

function test5() {
  if (Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1e1"), /* Format */[
              /* Float */{
                0: /* Float_e */3,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%e"
            ]), function (b) {
          return +(b === 10.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1e+1"), /* Format */[
              /* Float */{
                0: /* Float_e */3,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%e"
            ]), function (b) {
          return +(b === 10.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "10e-1"), /* Format */[
              /* Float */{
                0: /* Float_e */3,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%e"
            ]), function (b) {
          return +(b === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "10.e-1"), /* Format */[
              /* Float */{
                0: /* Float_e */3,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%e"
            ]), function (b) {
          return +(b === 1.0);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1e1 1.e+1 1.3e-1"), /* Format */[
              /* Float */{
                0: /* Float_e */3,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* " " */32,
                  1: /* Float */{
                    0: /* Float_e */3,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Float */{
                        0: /* Float_e */3,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 8
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 8
                  },
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 8
              },
              "%e %e %e"
            ]), function (b1, b2, b3) {
          return b1 === 10.0 && b2 === b1 ? +(b3 === 0.13) : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "1 1.1 0e+1 1.3e-1"), /* Format */[
                    /* Float */{
                      0: /* Float_g */9,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Float */{
                          0: /* Float_g */9,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* Char_literal */{
                            0: /* " " */32,
                            1: /* Float */{
                              0: /* Float_g */9,
                              1: /* No_padding */0,
                              2: /* No_precision */0,
                              3: /* Char_literal */{
                                0: /* " " */32,
                                1: /* Float */{
                                  0: /* Float_g */9,
                                  1: /* No_padding */0,
                                  2: /* No_precision */0,
                                  3: /* End_of_format */0,
                                  length: 4,
                                  tag: 8
                                },
                                length: 2,
                                tag: 12
                              },
                              length: 4,
                              tag: 8
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 4,
                          tag: 8
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 8
                    },
                    "%g %g %g %g"
                  ]), function (b1, b2, b3, b4) {
                if (b1 === 1.0 && b2 === 1.1 && b3 === 0.0) {
                  return +(b4 === 0.13);
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 133, characters 5-12', test5(/* () */0));

function test6() {
  if (Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "truetrue"), /* Format */[
              /* Bool */{
                0: /* Bool */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 9
                },
                length: 1,
                tag: 9
              },
              "%B%B"
            ]), function (b1, b2) {
          return Caml_obj.caml_equal(/* tuple */[
                      b1,
                      b2
                    ], /* tuple */[
                      /* true */1,
                      /* true */1
                    ]);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "truefalse"), /* Format */[
              /* Bool */{
                0: /* Bool */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 9
                },
                length: 1,
                tag: 9
              },
              "%B%B"
            ]), function (b1, b2) {
          return Caml_obj.caml_equal(/* tuple */[
                      b1,
                      b2
                    ], /* tuple */[
                      /* true */1,
                      /* false */0
                    ]);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "falsetrue"), /* Format */[
              /* Bool */{
                0: /* Bool */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 9
                },
                length: 1,
                tag: 9
              },
              "%B%B"
            ]), function (b1, b2) {
          return Caml_obj.caml_equal(/* tuple */[
                      b1,
                      b2
                    ], /* tuple */[
                      /* false */0,
                      /* true */1
                    ]);
        }) && Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "falsefalse"), /* Format */[
              /* Bool */{
                0: /* Bool */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 9
                },
                length: 1,
                tag: 9
              },
              "%B%B"
            ]), function (b1, b2) {
          return Caml_obj.caml_equal(/* tuple */[
                      b1,
                      b2
                    ], /* tuple */[
                      /* false */0,
                      /* false */0
                    ]);
        })) {
    return Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "true false"), /* Format */[
                    /* Bool */{
                      0: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Bool */{
                          0: /* End_of_format */0,
                          length: 1,
                          tag: 9
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 9
                    },
                    "%B %B"
                  ]), function (b1, b2) {
                return Caml_obj.caml_equal(/* tuple */[
                            b1,
                            b2
                          ], /* tuple */[
                            /* true */1,
                            /* false */0
                          ]);
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 150, characters 5-12', test6(/* () */0));

function test7() {
  if (Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "'a' '\n' '\t' '\0' ' '"), /* Format */[
              /* Caml_char */{
                0: /* Char_literal */{
                  0: /* " " */32,
                  1: /* Caml_char */{
                    0: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Caml_char */{
                        0: /* Char_literal */{
                          0: /* " " */32,
                          1: /* Caml_char */{
                            0: /* Char_literal */{
                              0: /* " " */32,
                              1: /* Caml_char */{
                                0: /* End_of_format */0,
                                length: 1,
                                tag: 1
                              },
                              length: 2,
                              tag: 12
                            },
                            length: 1,
                            tag: 1
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 1,
                        tag: 1
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 1,
                    tag: 1
                  },
                  length: 2,
                  tag: 12
                },
                length: 1,
                tag: 1
              },
              "%C %C %C %C %C"
            ]), function (c1, c2, c3, c4, c5) {
          return c1 === /* "a" */97 && c2 === /* "\n" */10 && c3 === /* "\t" */9 && !c4 ? +(c5 === /* " " */32) : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.bscanf(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "a \n \t \0  b"), /* Format */[
                    /* Char */{
                      0: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Char */{
                          0: /* Char_literal */{
                            0: /* " " */32,
                            1: /* Char */{
                              0: /* Char_literal */{
                                0: /* " " */32,
                                1: /* End_of_format */0,
                                length: 2,
                                tag: 12
                              },
                              length: 1,
                              tag: 0
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 1,
                          tag: 0
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 0
                    },
                    "%c %c %c "
                  ]), function (c1, c2, c3) {
                if (c1 === /* "a" */97 && !c2) {
                  return +(c3 === /* "b" */98);
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 168, characters 5-12', test7(/* () */0));

function verify_read(c) {
  var s = Caml_curry.app1(Printf.sprintf(/* Format */[
            /* Caml_char */{
              0: /* End_of_format */0,
              length: 1,
              tag: 1
            },
            "%C"
          ]), c);
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], s);
  if (Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
              /* Caml_char */{
                0: /* End_of_format */0,
                length: 1,
                tag: 1
              },
              "%C"
            ]), id) === c) {
    return 0;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "tscanf_test.ml",
            174,
            2
          ]
        ];
  }
}

function verify_scan_Chars() {
  for(var i = 0; i<= 255; ++i){
    verify_read(Pervasives.char_of_int(i));
  }
  return /* () */0;
}

function test8() {
  return +(verify_scan_Chars(/* () */0) === /* () */0);
}

test('File "tscanf_test.ml", line 183, characters 5-12', +(verify_scan_Chars(/* () */0) === /* () */0));

function unit(fmt, s) {
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], Caml_curry.app1(Printf.sprintf(/* Format */[
                /* Caml_string */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 3
                },
                "%S"
              ]), s));
  return Caml_curry.app1(Scanf.bscanf(ib, fmt), id);
}

function test_fmt(fmt, s) {
  return +(unit(fmt, s) === s);
}

var test9_string = "\xef\xbb\xbf";

var partial_arg = /* Format */[
  /* Caml_string */{
    0: /* No_padding */0,
    1: /* End_of_format */0,
    length: 2,
    tag: 3
  },
  "%S"
];

function test_S(param) {
  return test_fmt(partial_arg, param);
}

function test9() {
  if (test_S("poi") && test_S('a"b') && test_S("a\nb") && test_S("a\nb") && test_S("a\\\nb \\\nc\n\\\nb") && test_S("a\\\n\\\n\\\nb \\\nc\n\\\nb") && test_S("\xef") && test_S("\\xef") && Caml_curry.app1(Scanf.sscanf('"\\xef"', /* Format */[
              /* Caml_string */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 3
              },
              "%S"
            ]), function (s) {
          return s;
        }) === "\xef" && Caml_curry.app1(Scanf.sscanf('"\\xef\\xbb\\xbf"', /* Format */[
              /* Caml_string */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 3
              },
              "%S"
            ]), function (s) {
          return s;
        }) === test9_string && Caml_curry.app1(Scanf.sscanf('"\\xef\\xbb\\xbf"', /* Format */[
              /* Caml_string */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 3
              },
              "%S"
            ]), function (s) {
          return s;
        }) === "\xef\xbb\xbf" && Caml_curry.app1(Scanf.sscanf('"\xef\xbb\xbf"', /* Format */[
              /* Caml_string */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 3
              },
              "%S"
            ]), function (s) {
          return s;
        }) === test9_string && Caml_curry.app1(Scanf.sscanf('"\\\\xef\\\\xbb\\\\xbf"', /* Format */[
              /* Caml_string */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 3
              },
              "%S"
            ]), function (s) {
          return s;
        }) === "\\xef\\xbb\\xbf") {
    return +(Caml_curry.app1(Scanf.sscanf('" "', /* Format */[
                      /* Caml_string */{
                        0: /* No_padding */0,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 3
                      },
                      "%S"
                    ]), function (s) {
                  return s;
                }) === " ");
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 230, characters 5-12', test9(/* () */0));

function test10() {
  var unit = function (s) {
    var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], s);
    return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                    /* Caml_string */{
                      0: /* No_padding */0,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 3
                    },
                    "%S"
                  ]), id);
  };
  var res = Caml_curry.app1(Scanf.sscanf('Une chaine: "celle-ci" et "celle-la"!', /* Format */[
            /* String */{
              0: /* No_padding */0,
              1: /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* " " */32,
                    1: /* Caml_string */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* " " */32,
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* Caml_string */{
                              0: /* No_padding */0,
                              1: /* Char_literal */{
                                0: /* " " */32,
                                1: /* String */{
                                  0: /* No_padding */0,
                                  1: /* End_of_format */0,
                                  length: 2,
                                  tag: 2
                                },
                                length: 2,
                                tag: 12
                              },
                              length: 2,
                              tag: 3
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 3
                    },
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              length: 2,
              tag: 2
            },
            "%s %s %S %s %S %s"
          ]), function (s1, s2, s3, s4, s5, s6) {
        return s1 + (s2 + (s3 + (s4 + (s5 + s6))));
      });
  if (res === "Unechaine:celle-cietcelle-la!" && unit('"a\\\n  b"') === "ab" && unit('"\\\n  ab"') === "ab" && unit('"\n\\\n  ab"') === "\nab" && unit('"\n\\\n  a\nb"') === "\na\nb" && unit('"\n\\\n  \\\n  a\nb"') === "\na\nb" && unit('"\n\\\n  a\n\\\nb\\\n"') === "\na\nb" && unit('"a\\\n  "') === "a") {
    return /* true */1;
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 254, characters 5-12', test10(/* () */0));

function test11() {
  if (Caml_curry.app1(Scanf.sscanf("Pierre\tWeis\t70", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* " " */32,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Char_literal */{
                      0: /* " " */32,
                      1: /* String */{
                        0: /* No_padding */0,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 2
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s %s %s"
            ]), function (prenom, nom, poids) {
          return prenom === "Pierre" && nom === "Weis" ? +(Caml_format.caml_int_of_string(poids) === 70) : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Jean-Luc\tde Leage\t68", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\xff\xfd\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                2: /* Char_literal */{
                  0: /* " " */32,
                  1: /* Scan_char_set */{
                    0: /* None */0,
                    1: "\xff\xfd\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    2: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Int */{
                        0: /* Int_d */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 3,
                    tag: 20
                  },
                  length: 2,
                  tag: 12
                },
                length: 3,
                tag: 20
              },
              "%[^\t] %[^\t] %d"
            ]), function (prenom, nom, poids) {
          return prenom === "Jean-Luc" && nom === "de Leage" ? +(poids === 68) : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.sscanf("Daniel\tde Rauglaudre\t66", /* Format */[
                    /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_lit */{
                        0: /* Scan_indic */{
                          0: /* "\t" */9,
                          length: 1,
                          tag: 2
                        },
                        1: /* Char_literal */{
                          0: /* " " */32,
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Formatting_lit */{
                              0: /* Scan_indic */{
                                0: /* "\t" */9,
                                length: 1,
                                tag: 2
                              },
                              1: /* Char_literal */{
                                0: /* " " */32,
                                1: /* Int */{
                                  0: /* Int_d */0,
                                  1: /* No_padding */0,
                                  2: /* No_precision */0,
                                  3: /* End_of_format */0,
                                  length: 4,
                                  tag: 4
                                },
                                length: 2,
                                tag: 12
                              },
                              length: 2,
                              tag: 17
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 2,
                        tag: 17
                      },
                      length: 2,
                      tag: 2
                    },
                    "%s@\t %s@\t %d"
                  ]), function (prenom, nom, poids) {
                if (prenom === "Daniel" && nom === "de Rauglaudre") {
                  return +(poids === 66);
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

function test110() {
  if (Caml_curry.app2(Scanf.sscanf("", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* End_of_format */0,
                length: 2,
                tag: 12
              },
              " "
            ]), function (x) {
          return x;
        }, "") === "" && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 2
              },
              "%s"
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s"
            ]), function (x, y) {
          return x === "" ? +(y === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* " " */32,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s "
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s"
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* " " */32,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s "
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                2: /* End_of_format */0,
                length: 3,
                tag: 20
              },
              "%[^\n]"
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                2: /* Char_literal */{
                  0: /* " " */32,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 3,
                tag: 20
              },
              "%[^\n] "
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 2
              },
              "%s"
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s"
            ]), function (x, y) {
          return x === "" ? +(y === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* " " */32,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s "
            ]), function (x) {
          return +(x === "");
        }) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* " " */32,
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s %s"
            ]), function (x, y) {
          return x === "" ? +(x === y) : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Formatting_lit */{
                    0: /* Break */{
                      0: "@ ",
                      1: 1,
                      2: 0,
                      length: 3,
                      tag: 0
                    },
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 17
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s@ %s"
            ]), function (x, y) {
          return x === "" ? +(x === y) : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf(" poi !", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Formatting_lit */{
                    0: /* Break */{
                      0: "@ ",
                      1: 1,
                      2: 0,
                      length: 3,
                      tag: 0
                    },
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_lit */{
                        0: /* Flush_newline */4,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 17
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 17
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 12
              },
              " %s@ %s@."
            ]), function (x, y) {
          return x === "poi" ? +(y === "!") : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.sscanf(" poi !", /* Format */[
                    /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_lit */{
                        0: /* Break */{
                          0: "@ ",
                          1: 1,
                          2: 0,
                          length: 3,
                          tag: 0
                        },
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* Formatting_lit */{
                            0: /* Flush_newline */4,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 17
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 17
                      },
                      length: 2,
                      tag: 2
                    },
                    "%s@ %s@."
                  ]), function (x, y) {
                if (x === "") {
                  return +(y === "poi !");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

function test111() {
  return Caml_curry.app1(Scanf.sscanf("", /* Format */[
                  /* Scan_char_set */{
                    0: /* None */0,
                    1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    2: /* Formatting_lit */{
                      0: /* Force_newline */3,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 17
                    },
                    length: 3,
                    tag: 20
                  },
                  "%[^\n]@\n"
                ]), function (x) {
              return +(x === "");
            });
}

test('File "tscanf_test.ml", line 293, characters 5-12', +(test11(/* () */0) && test110(/* () */0) && test111(/* () */0)));

function ib() {
  return Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]");
}

function f(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " [",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ["
          ]), /* () */0);
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* ";" */59,
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
                  " %i;"
                ]), function (i) {
              return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                              /* Char_literal */{
                                0: /* " " */32,
                                1: /* Int */{
                                  0: /* Int_i */3,
                                  1: /* No_padding */0,
                                  2: /* No_precision */0,
                                  3: /* Char_literal */{
                                    0: /* ";" */59,
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
                              " %i;"
                            ]), function (j) {
                          return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                                          /* Char_literal */{
                                            0: /* " " */32,
                                            1: /* Int */{
                                              0: /* Int_i */3,
                                              1: /* No_padding */0,
                                              2: /* No_precision */0,
                                              3: /* Char_literal */{
                                                0: /* ";" */59,
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
                                          " %i;"
                                        ]), function (k) {
                                      return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                                                      /* Char_literal */{
                                                        0: /* " " */32,
                                                        1: /* Int */{
                                                          0: /* Int_i */3,
                                                          1: /* No_padding */0,
                                                          2: /* No_precision */0,
                                                          3: /* Char_literal */{
                                                            0: /* ";" */59,
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
                                                      " %i;"
                                                    ]), function (l) {
                                                  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                                                            /* String_literal */{
                                                              0: " ]",
                                                              1: /* End_of_format */0,
                                                              length: 2,
                                                              tag: 11
                                                            },
                                                            " ]"
                                                          ]), /* () */0);
                                                  return /* :: */[
                                                          i,
                                                          /* :: */[
                                                            j,
                                                            /* :: */[
                                                              k,
                                                              /* :: */[
                                                                l,
                                                                /* [] */0
                                                              ]
                                                            ]
                                                          ]
                                                        ];
                                                });
                                    });
                        });
            });
}

function test12() {
  return Caml_obj.caml_equal(f(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]")), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

test('File "tscanf_test.ml", line 311, characters 5-12', test12(/* () */0));

function scan_elems(ib, accu) {
  try {
    return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                    /* Char_literal */{
                      0: /* " " */32,
                      1: /* Int */{
                        0: /* Int_i */3,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* Char_literal */{
                          0: /* ";" */59,
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
                    " %i;"
                  ]), function (i) {
                return scan_elems(ib, /* :: */[
                            i,
                            accu
                          ]);
              });
  }
  catch (exn){
    return accu;
  }
}

function g(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  return List.rev(scan_elems(ib, /* [] */0));
}

function test13() {
  return Caml_obj.caml_equal(g(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]")), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

test('File "tscanf_test.ml", line 324, characters 5-12', test13(/* () */0));

function scan_int_list(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems(ib, /* [] */0);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

function test14() {
  return Caml_obj.caml_equal(scan_int_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]")), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

test('File "tscanf_test.ml", line 337, characters 5-12', test14(/* () */0));

function scan_elems$1(ib, accu) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Char */{
                          0: /* End_of_format */0,
                          length: 1,
                          tag: 0
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  " %i %c"
                ]), function (i, c) {
              if (c !== 59) {
                if (c !== 93) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "scan_elems"
                      ];
                }
                else {
                  return List.rev(/* :: */[
                              i,
                              accu
                            ]);
                }
              }
              else {
                return scan_elems$1(ib, /* :: */[
                            i,
                            accu
                          ]);
              }
            });
}

function scan_int_list$1(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  return scan_elems$1(ib, /* [] */0);
}

function test15() {
  return Caml_obj.caml_equal(scan_int_list$1(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

test('File "tscanf_test.ml", line 357, characters 5-12', test15(/* () */0));

function scan_elems$2(ib, accu) {
  try {
    return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                    /* Char */{
                      0: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Int */{
                          0: /* Int_i */3,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 4
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 0
                    },
                    "%c %i"
                  ]), function (c, i) {
                var exit = 0;
                if (c >= 91) {
                  if (c >= 94) {
                    exit = 1;
                  }
                  else {
                    switch (c - 91 | 0) {
                      case 0 : 
                          if (accu) {
                            exit = 1;
                          }
                          else {
                            return scan_elems$2(ib, /* :: */[
                                        i,
                                        accu
                                      ]);
                          }
                          break;
                      case 1 : 
                          exit = 1;
                          break;
                      case 2 : 
                          return List.rev(/* :: */[
                                      i,
                                      accu
                                    ]);
                      
                    }
                  }
                }
                else if (c !== 59) {
                  exit = 1;
                }
                else {
                  return scan_elems$2(ib, /* :: */[
                              i,
                              accu
                            ]);
                }
                if (exit === 1) {
                  console.log(Caml_string.bytes_to_string(Bytes.make(1, c)));
                  throw [
                        Caml_builtin_exceptions.failure,
                        "scan_elems"
                      ];
                }
                
              });
  }
  catch (exn){
    if (exn[0] === Scanf.Scan_failure) {
      Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                /* Char_literal */{
                  0: /* "]" */93,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                "]"
              ]), /* () */0);
      return accu;
    }
    else if (exn === Caml_builtin_exceptions.end_of_file) {
      return accu;
    }
    else {
      throw exn;
    }
  }
}

function test16() {
  if (Caml_obj.caml_equal(scan_elems$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]"), /* [] */0), List.rev(/* [] */0)) && Caml_obj.caml_equal(scan_elems$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]"), /* [] */0), List.rev(/* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ])) && Caml_obj.caml_equal(scan_elems$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]"), /* [] */0), List.rev(/* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]))) {
    return Caml_obj.caml_equal(scan_elems$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4"), /* [] */0), List.rev(/* :: */[
                    1,
                    /* :: */[
                      2,
                      /* :: */[
                        3,
                        /* :: */[
                          4,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]));
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 383, characters 5-12', test16(/* () */0));

function scan_elems$3(ib, accu) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Scan_char_set */{
                        0: /* None */0,
                        1: "\0&\0\0\x01\0\0\b\0\0\0 \0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* End_of_format */0,
                        length: 3,
                        tag: 20
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  " %i%[]; \t\n\r]"
                ]), function (i, s) {
              switch (s) {
                case ";" : 
                    return scan_elems$3(ib, /* :: */[
                                i,
                                accu
                              ]);
                case "]" : 
                    return List.rev(/* :: */[
                                i,
                                accu
                              ]);
                default:
                  return List.rev(/* :: */[
                              i,
                              accu
                            ]);
              }
            });
}

function scan_int_list$2(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " [",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ["
          ]), /* () */0);
  return scan_elems$3(ib, /* [] */0);
}

function test17() {
  if (Caml_obj.caml_equal(scan_int_list$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ]) && Caml_obj.caml_equal(scan_int_list$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_int_list$2(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4 5]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 406, characters 5-12', test17(/* () */0));

function scan_elems$4(ib, accu) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Char */{
                      0: /* Char_literal */{
                        0: /* " " */32,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 0
                    },
                    length: 2,
                    tag: 12
                  },
                  " %c "
                ]), function (c) {
              if (c !== 91) {
                throw [
                      Caml_builtin_exceptions.failure,
                      "scan_elems"
                    ];
              }
              else if (accu) {
                throw [
                      Caml_builtin_exceptions.failure,
                      "scan_elems"
                    ];
              }
              else {
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
                              return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
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
                                          return scan_rest(ib, /* :: */[
                                                      i,
                                                      accu
                                                    ]);
                                        });
                            }
                          });
              }
            });
}

function scan_rest(ib, accu) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Char */{
                      0: /* Char_literal */{
                        0: /* " " */32,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 0
                    },
                    length: 2,
                    tag: 12
                  },
                  " %c "
                ]), function (c) {
              if (c !== 59) {
                if (c !== 93) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "scan_rest"
                      ];
                }
                else {
                  return accu;
                }
              }
              else {
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
                              return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
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
                                          return scan_rest(ib, /* :: */[
                                                      i,
                                                      accu
                                                    ]);
                                        });
                            }
                          });
              }
            });
}

function scan_int_list$3(ib) {
  return List.rev(scan_elems$4(ib, /* [] */0));
}

function test18() {
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]");
  if (List.rev(scan_elems$4(ib, /* [] */0))) {
    return /* false */0;
  }
  else {
    var ib$1 = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]");
    if (List.rev(scan_elems$4(ib$1, /* [] */0))) {
      return /* false */0;
    }
    else {
      var ib$2 = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]");
      if (Caml_obj.caml_equal(List.rev(scan_elems$4(ib$2, /* [] */0)), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ])) {
        var ib$3 = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4; ]");
        return Caml_obj.caml_equal(List.rev(scan_elems$4(ib$3, /* [] */0)), /* :: */[
                    1,
                    /* :: */[
                      2,
                      /* :: */[
                        3,
                        /* :: */[
                          4,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]);
      }
      else {
        return /* false */0;
      }
    }
  }
}

test('File "tscanf_test.ml", line 446, characters 5-12', test18(/* () */0));

function test19() {
  return Testing.failure_test(scan_int_list$3, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4 5]"), "scan_rest");
}

test19(/* () */0);

function test20() {
  return Testing.scan_failure_test(scan_int_list$3, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;; 5]"));
}

test20(/* () */0);

function test21() {
  return Testing.scan_failure_test(scan_int_list$3, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;;"));
}

test21(/* () */0);

function scan_rest$1(ib, accu) {
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
                                              return scan_rest$1(ib$2, accu$2);
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

function scan_int_list$4(ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " [ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " [ "
          ]), /* () */0);
  return List.rev(scan_rest$1(ib, /* [] */0));
}

function test22() {
  if (scan_int_list$4(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && scan_int_list$4(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_int_list$4(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1]")), /* :: */[
          1,
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_int_list$4(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_int_list$4(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 506, characters 5-12', test22(/* () */0));

function scan_elems$5(ib, scan_elem, accu) {
  try {
    return Caml_curry.app2(scan_elem, ib, function (i, s) {
                var accu$1 = /* :: */[
                  i,
                  accu
                ];
                if (s === "") {
                  return accu$1;
                }
                else {
                  return scan_elems$5(ib, scan_elem, accu$1);
                }
              });
  }
  catch (exn){
    if (exn[0] === Scanf.Scan_failure) {
      return accu;
    }
    else {
      throw exn;
    }
  }
}

function scan_list(scan_elem, ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems$5(ib, scan_elem, /* [] */0);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

function scan_int_elem(ib) {
  return Scanf.bscanf(ib, /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* Int */{
                  0: /* Int_i */3,
                  1: /* No_padding */0,
                  2: /* No_precision */0,
                  3: /* Char_literal */{
                    0: /* " " */32,
                    1: /* Scan_char_set */{
                      0: /* Some */[1],
                      1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      2: /* End_of_format */0,
                      length: 3,
                      tag: 20
                    },
                    length: 2,
                    tag: 12
                  },
                  length: 4,
                  tag: 4
                },
                length: 2,
                tag: 12
              },
              " %i %1[;]"
            ]);
}

function scan_int_list$5(param) {
  return scan_list(scan_int_elem, param);
}

function test23() {
  if (scan_list(scan_int_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && scan_list(scan_int_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list(scan_int_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1]")), /* :: */[
          1,
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_list(scan_int_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_list(scan_int_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 562, characters 5-12', test23(/* () */0));

function test24() {
  return Testing.scan_failure_test(scan_int_list$5, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4 5]"));
}

function test25() {
  return Testing.scan_failure_test(scan_int_list$5, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;;"));
}

function test26() {
  return Testing.scan_failure_test(scan_int_list$5, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;; 5]"));
}

function test27() {
  return Testing.scan_failure_test(scan_int_list$5, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;; 23]"));
}

+(test24(/* () */0) && test25(/* () */0) && test26(/* () */0) && test27(/* () */0));

function scan_string_elem(ib) {
  return Scanf.bscanf(ib, /* Format */[
              /* String_literal */{
                0: ' "',
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Formatting_lit */{
                    0: /* Scan_indic */{
                      0: /* "\"" */34,
                      length: 1,
                      tag: 2
                    },
                    1: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Scan_char_set */{
                        0: /* Some */[1],
                        1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* End_of_format */0,
                        length: 3,
                        tag: 20
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 17
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              ' "%s@" %1[;]'
            ]);
}

function scan_String_elem(ib) {
  return Scanf.bscanf(ib, /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* Caml_string */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* " " */32,
                    1: /* Scan_char_set */{
                      0: /* Some */[1],
                      1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      2: /* End_of_format */0,
                      length: 3,
                      tag: 20
                    },
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 3
                },
                length: 2,
                tag: 12
              },
              " %S %1[;]"
            ]);
}

function scan_String_list(param) {
  return scan_list(scan_String_elem, param);
}

function test28() {
  if (scan_list(scan_string_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_obj.caml_equal(scan_list(scan_string_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le"]')), /* :: */[
          "Le",
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_list(scan_string_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le";"langage";"Objective";"Caml"]')), /* :: */[
          "Le",
          /* :: */[
            "langage",
            /* :: */[
              "Objective",
              /* :: */[
                "Caml",
                /* [] */0
              ]
            ]
          ]
        ]) && Caml_obj.caml_equal(scan_list(scan_string_elem, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le";"langage";"Objective";"Caml"; ]')), /* :: */[
          "Le",
          /* :: */[
            "langage",
            /* :: */[
              "Objective",
              /* :: */[
                "Caml",
                /* [] */0
              ]
            ]
          ]
        ]) && scan_String_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_obj.caml_equal(scan_String_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le"]')), /* :: */[
          "Le",
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_String_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le";"langage";"Objective";"Caml"]')), /* :: */[
          "Le",
          /* :: */[
            "langage",
            /* :: */[
              "Objective",
              /* :: */[
                "Caml",
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_String_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["Le";"langage";"Objective";"Caml"; ]')), /* :: */[
                "Le",
                /* :: */[
                  "langage",
                  /* :: */[
                    "Objective",
                    /* :: */[
                      "Caml",
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 609, characters 5-12', test28(/* () */0));

function scan_elems$6(ib, scan_elem, accu) {
  return Caml_curry.app3(scan_elem, ib, function (i, s) {
              var accu$1 = /* :: */[
                i,
                accu
              ];
              if (s === "") {
                return accu$1;
              }
              else {
                return scan_elems$6(ib, scan_elem, accu$1);
              }
            }, function (_, _$1) {
              return accu;
            });
}

function scan_list$1(scan_elem, ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems$6(ib, scan_elem, /* [] */0);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

function scan_int_elem$1(ib, f, ek) {
  return Caml_curry.app1(Scanf.kscanf(ib, ek, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Scan_char_set */{
                          0: /* Some */[1],
                          1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                          2: /* End_of_format */0,
                          length: 3,
                          tag: 20
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  " %i %1[;]"
                ]), f);
}

function test29() {
  if (scan_list$1(scan_int_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && scan_list$1(scan_int_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1]")), /* :: */[
          1,
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_list$1(scan_int_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 639, characters 5-12', test29(/* () */0));

function scan_string_elem$1(ib, f, ek) {
  return Caml_curry.app1(Scanf.kscanf(ib, ek, /* Format */[
                  /* Char_literal */{
                    0: /* " " */32,
                    1: /* Caml_string */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Scan_char_set */{
                          0: /* Some */[1],
                          1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                          2: /* End_of_format */0,
                          length: 3,
                          tag: 20
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 3
                    },
                    length: 2,
                    tag: 12
                  },
                  " %S %1[;]"
                ]), f);
}

function test30() {
  if (scan_list$1(scan_string_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && scan_list$1(scan_string_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '[ "1" ]')), /* :: */[
          "1",
          /* [] */0
        ]) && Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4"]')), /* :: */[
          "1",
          /* :: */[
            "2",
            /* :: */[
              "3",
              /* :: */[
                "4",
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(scan_list$1(scan_string_elem$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4";]')), /* :: */[
                "1",
                /* :: */[
                  "2",
                  /* :: */[
                    "3",
                    /* :: */[
                      "4",
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 656, characters 5-12', test30(/* () */0));

function scan_elem(fmt, ib, f, ek) {
  return Caml_curry.app1(Scanf.kscanf(ib, ek, fmt), f);
}

function scan_elems$7(ib, scan_elem, accu) {
  return Caml_curry.app3(scan_elem, ib, function (i) {
              var accu$1 = /* :: */[
                i,
                accu
              ];
              return Caml_curry.app1(Scanf.kscanf(ib, function (_, _$1) {
                              return accu$1;
                            }, /* Format */[
                              /* Char_literal */{
                                0: /* " " */32,
                                1: /* Scan_char_set */{
                                  0: /* Some */[1],
                                  1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                  2: /* End_of_format */0,
                                  length: 3,
                                  tag: 20
                                },
                                length: 2,
                                tag: 12
                              },
                              " %1[;]"
                            ]), function (s) {
                          if (s === "") {
                            return accu$1;
                          }
                          else {
                            return scan_elems$7(ib, scan_elem, accu$1);
                          }
                        });
            }, function (_, _$1) {
              return accu;
            });
}

function scan_list$2(scan_elem, ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems$7(ib, scan_elem, /* [] */0);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

var partial_arg$1 = /* Format */[
  /* Char_literal */{
    0: /* " " */32,
    1: /* Int */{
      0: /* Int_i */3,
      1: /* No_padding */0,
      2: /* No_precision */0,
      3: /* End_of_format */0,
      length: 4,
      tag: 4
    },
    length: 2,
    tag: 12
  },
  " %i"
];

function partial_arg$2(param, param$1, param$2) {
  return scan_elem(partial_arg$1, param, param$1, param$2);
}

function scan_int_list$6(param) {
  return scan_list$2(partial_arg$2, param);
}

var partial_arg$3 = /* Format */[
  /* Char_literal */{
    0: /* " " */32,
    1: /* Caml_string */{
      0: /* No_padding */0,
      1: /* End_of_format */0,
      length: 2,
      tag: 3
    },
    length: 2,
    tag: 12
  },
  " %S"
];

function partial_arg$4(param, param$1, param$2) {
  return scan_elem(partial_arg$3, param, param$1, param$2);
}

function scan_string_list(param) {
  return scan_list$2(partial_arg$4, param);
}

function test31() {
  if (Caml_curry.app1(scan_int_list$6, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_curry.app1(scan_int_list$6, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$6, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1]")), /* :: */[
          1,
          /* [] */0
        ]) && Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$6, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$6, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 714, characters 5-12', test31(/* () */0));

function test32() {
  if (Caml_curry.app1(scan_string_list, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_curry.app1(scan_string_list, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(Caml_curry.app1(scan_string_list, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '[ "1" ]')), /* :: */[
          "1",
          /* [] */0
        ]) && Caml_obj.caml_equal(Caml_curry.app1(scan_string_list, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4"]')), /* :: */[
          "1",
          /* :: */[
            "2",
            /* :: */[
              "3",
              /* :: */[
                "4",
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(scan_string_list, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4";]')), /* :: */[
                "1",
                /* :: */[
                  "2",
                  /* :: */[
                    "3",
                    /* :: */[
                      "4",
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 728, characters 5-12', test32(/* () */0));

function scan_elems$8(ib, scan_elem_fmt, accu) {
  return Caml_curry.app1(Scanf.kscanf(ib, function (_, _$1) {
                  return accu;
                }, scan_elem_fmt), function (i) {
              var accu$1 = /* :: */[
                i,
                accu
              ];
              return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                              /* Char_literal */{
                                0: /* " " */32,
                                1: /* Scan_char_set */{
                                  0: /* Some */[1],
                                  1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                  2: /* Char_literal */{
                                    0: /* " " */32,
                                    1: /* End_of_format */0,
                                    length: 2,
                                    tag: 12
                                  },
                                  length: 3,
                                  tag: 20
                                },
                                length: 2,
                                tag: 12
                              },
                              " %1[;] "
                            ]), function (param) {
                          if (param === "") {
                            return accu$1;
                          }
                          else {
                            return scan_elems$8(ib, scan_elem_fmt, accu$1);
                          }
                        });
            });
}

function scan_list$3(scan_elem_fmt, ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems$8(ib, scan_elem_fmt, /* [] */0);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

var partial_arg$5 = /* Format */[
  /* Int */{
    0: /* Int_i */3,
    1: /* No_padding */0,
    2: /* No_precision */0,
    3: /* End_of_format */0,
    length: 4,
    tag: 4
  },
  "%i"
];

function scan_int_list$7(param) {
  return scan_list$3(partial_arg$5, param);
}

var partial_arg$6 = /* Format */[
  /* Caml_string */{
    0: /* No_padding */0,
    1: /* End_of_format */0,
    length: 2,
    tag: 3
  },
  "%S"
];

function scan_string_list$1(param) {
  return scan_list$3(partial_arg$6, param);
}

function test33() {
  if (Caml_curry.app1(scan_int_list$7, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_curry.app1(scan_int_list$7, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$7, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ 1 ]")), /* :: */[
          1,
          /* [] */0
        ]) && Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$7, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ 1; 2; 3; 4 ]")), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(scan_int_list$7, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[1;2;3;4;]")), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 773, characters 5-12', test33(/* () */0));

function test34() {
  if (Caml_curry.app1(scan_string_list$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[]")) === /* [] */0 && Caml_curry.app1(scan_string_list$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[ ]")) === /* [] */0 && Caml_obj.caml_equal(Caml_curry.app1(scan_string_list$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '[ "1" ]')), /* :: */[
          "1",
          /* [] */0
        ]) && Caml_obj.caml_equal(Caml_curry.app1(scan_string_list$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4"]')), /* :: */[
          "1",
          /* :: */[
            "2",
            /* :: */[
              "3",
              /* :: */[
                "4",
                /* [] */0
              ]
            ]
          ]
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(scan_string_list$1, Caml_curry.app1(Scanf.Scanning[/* from_string */6], '["1"; "2"; "3"; "4";]')), /* :: */[
                "1",
                /* :: */[
                  "2",
                  /* :: */[
                    "3",
                    /* :: */[
                      "4",
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 787, characters 5-12', test34(/* () */0));

function scan_elems$9(scan_elem, accu, ib) {
  return Caml_curry.app2(Scanf.kscanf(ib, function (_, _$1) {
                  return accu;
                }, /* Format */[
                  /* Reader */{
                    0: /* End_of_format */0,
                    length: 1,
                    tag: 19
                  },
                  "%r"
                ]), function (ib) {
              return Caml_curry.app2(scan_elem, ib, function (elem) {
                          var accu$1 = /* :: */[
                            elem,
                            accu
                          ];
                          return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                                          /* Char_literal */{
                                            0: /* " " */32,
                                            1: /* Scan_char_set */{
                                              0: /* Some */[1],
                                              1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                                              2: /* Char_literal */{
                                                0: /* " " */32,
                                                1: /* End_of_format */0,
                                                length: 2,
                                                tag: 12
                                              },
                                              length: 3,
                                              tag: 20
                                            },
                                            length: 2,
                                            tag: 12
                                          },
                                          " %1[;] "
                                        ]), function (param) {
                                      if (param === "") {
                                        return accu$1;
                                      }
                                      else {
                                        return scan_elems$9(scan_elem, accu$1, ib);
                                      }
                                    });
                        });
            }, function (l) {
              return l;
            });
}

function scan_list$4(scan_elem, ib) {
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: "[ ",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            "[ "
          ]), /* () */0);
  var accu = scan_elems$9(scan_elem, /* [] */0, ib);
  Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
            /* String_literal */{
              0: " ]",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            " ]"
          ]), /* () */0);
  return List.rev(accu);
}

function scan_float(ib) {
  return Scanf.bscanf(ib, /* Format */[
              /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 8
              },
              "%f"
            ]);
}

function scan_int_list$8(param) {
  return scan_list$4(function (ib) {
              return Scanf.bscanf(ib, /* Format */[
                          /* Int */{
                            0: /* Int_i */3,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* End_of_format */0,
                            length: 4,
                            tag: 4
                          },
                          "%i"
                        ]);
            }, param);
}

function scan_string_list$2(param) {
  return scan_list$4(function (ib) {
              return Scanf.bscanf(ib, /* Format */[
                          /* Caml_string */{
                            0: /* No_padding */0,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 3
                          },
                          "%S"
                        ]);
            }, param);
}

function scan_bool_list(param) {
  return scan_list$4(function (ib) {
              return Scanf.bscanf(ib, /* Format */[
                          /* Bool */{
                            0: /* End_of_format */0,
                            length: 1,
                            tag: 9
                          },
                          "%B"
                        ]);
            }, param);
}

function scan_char_list(param) {
  return scan_list$4(function (ib) {
              return Scanf.bscanf(ib, /* Format */[
                          /* Caml_char */{
                            0: /* End_of_format */0,
                            length: 1,
                            tag: 1
                          },
                          "%C"
                        ]);
            }, param);
}

function scan_float_list_list(param) {
  return scan_list$4(function (ib, k) {
              return Caml_curry.app1(k, scan_list$4(scan_float, ib));
            }, param);
}

function test340() {
  return Caml_obj.caml_equal(scan_float_list_list(Caml_curry.app1(Scanf.Scanning[/* from_string */6], "[[1.0] ; []; [2.0; 3; 5.0; 6.];]")), /* :: */[
              /* :: */[
                1,
                /* [] */0
              ],
              /* :: */[
                /* [] */0,
                /* :: */[
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* :: */[
                        5,
                        /* :: */[
                          6,
                          /* [] */0
                        ]
                      ]
                    ]
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

function scan_list_list(scan_elems, ib) {
  return scan_list$4(function (ib, k) {
              return Caml_curry.app1(k, Caml_curry.app1(scan_elems, ib));
            }, ib);
}

function scan_float_item(ib, k) {
  return Caml_curry.app1(k, Caml_curry.app1(scan_float(ib), function (x) {
                  return x;
                }));
}

function scan_float_list(ib, k) {
  return Caml_curry.app1(k, scan_list$4(scan_float_item, ib));
}

function scan_float_list_list$1(ib, k) {
  return Caml_curry.app1(k, scan_list$4(scan_float_list, ib));
}

function test35() {
  if (Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Scan_get_counter */{
                0: /* Token_counter */2,
                1: /* End_of_format */0,
                length: 2,
                tag: 21
              },
              "%N"
            ]), function (x) {
          return x;
        }) === 0 && Caml_curry.app1(Scanf.sscanf("456", /* Format */[
              /* Scan_get_counter */{
                0: /* Token_counter */2,
                1: /* End_of_format */0,
                length: 2,
                tag: 21
              },
              "%N"
            ]), function (x) {
          return x;
        }) === 0 && Caml_obj.caml_equal(Caml_curry.app1(Scanf.sscanf("456", /* Format */[
                  /* Int */{
                    0: /* Int_d */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Scan_get_counter */{
                      0: /* Token_counter */2,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 21
                    },
                    length: 4,
                    tag: 4
                  },
                  "%d%N"
                ]), function (x, y) {
              return /* tuple */[
                      x,
                      y
                    ];
            }), /* tuple */[
          456,
          1
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
                        /* Scan_get_counter */{
                          0: /* Token_counter */2,
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Scan_get_counter */{
                              0: /* Token_counter */2,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 21
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 21
                        },
                        "%N%s%N"
                      ]), function (x, s, y) {
                    return /* tuple */[
                            x,
                            s,
                            y
                          ];
                  }), /* tuple */[
                0,
                "",
                1
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 940, characters 5-12', +(test340(/* () */0) && test35(/* () */0)));

function read_elems(read_elem, accu, ib) {
  return Caml_curry.app2(Scanf.kscanf(ib, function (_, _$1) {
                  return accu;
                }, /* Format */[
                  /* Reader */{
                    0: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Scan_char_set */{
                        0: /* Some */[1],
                        1: "\0\0\0\0\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* Char_literal */{
                          0: /* " " */32,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 12
                        },
                        length: 3,
                        tag: 20
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 1,
                    tag: 19
                  },
                  "%r %1[;] "
                ]), Caml_curry.app1(read_elem, function (elem) {
                  return /* :: */[
                          elem,
                          accu
                        ];
                }), function (accu, s) {
              if (s === "") {
                return accu;
              }
              else {
                return read_elems(read_elem, accu, ib);
              }
            });
}

function read_list(read_elem, ib) {
  return Caml_curry.app2(Scanf.bscanf(ib, /* Format */[
                  /* String_literal */{
                    0: "[ ",
                    1: /* Reader */{
                      0: /* String_literal */{
                        0: " ]",
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 11
                      },
                      length: 1,
                      tag: 19
                    },
                    length: 2,
                    tag: 11
                  },
                  "[ %r ]"
                ]), function (param) {
              return read_elems(read_elem, /* [] */0, param);
            }, List.rev);
}

function make_read_elem(fmt, f, ib) {
  return Caml_curry.app1(Scanf.bscanf(ib, fmt), f);
}

function scan_List(fmt) {
  return function (param) {
    return read_list(function (param, param$1) {
                return Caml_curry.app1(Scanf.bscanf(param$1, fmt), param);
              }, param);
  };
}

function test36() {
  if (Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Scan_get_counter */{
                0: /* Char_counter */1,
                1: /* End_of_format */0,
                length: 2,
                tag: 21
              },
              "%n"
            ]), function (x) {
          return x;
        }) === 0 && Caml_curry.app1(Scanf.sscanf("456", /* Format */[
              /* Scan_get_counter */{
                0: /* Char_counter */1,
                1: /* End_of_format */0,
                length: 2,
                tag: 21
              },
              "%n"
            ]), function (x) {
          return x;
        }) === 0 && Caml_obj.caml_equal(Caml_curry.app1(Scanf.sscanf("456", /* Format */[
                  /* Int */{
                    0: /* Int_d */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Scan_get_counter */{
                      0: /* Char_counter */1,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 21
                    },
                    length: 4,
                    tag: 4
                  },
                  "%d%n"
                ]), function (x, y) {
              return /* tuple */[
                      x,
                      y
                    ];
            }), /* tuple */[
          456,
          3
        ])) {
    return Caml_obj.caml_equal(Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
                        /* Scan_get_counter */{
                          0: /* Char_counter */1,
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Scan_get_counter */{
                              0: /* Char_counter */1,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 21
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 21
                        },
                        "%n%s%n"
                      ]), function (x, s, y) {
                    return /* tuple */[
                            x,
                            s,
                            y
                          ];
                  }), /* tuple */[
                0,
                "",
                0
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 995, characters 5-12', test36(/* () */0));

function test37() {
  if (Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* End_of_format */0,
              ""
            ]), /* true */1) && Caml_curry.app2(Scanf.sscanf("", /* Format */[
              /* End_of_format */0,
              ""
            ]), function (x) {
          return x;
        }, 1) === 1) {
    return +(Caml_curry.app2(Scanf.sscanf("123", /* Format */[
                      /* End_of_format */0,
                      ""
                    ]), function (x) {
                  return x;
                }, 1) === 1);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1005, characters 5-12', test37(/* () */0));

function test38() {
  if (Caml_curry.app1(Scanf.sscanf("a", /* Format */[
              /* Char_literal */{
                0: /* "a" */97,
                1: /* Flush */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 12
              },
              "a%!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf("a", /* Format */[
              /* Char_literal */{
                0: /* "a" */97,
                1: /* Flush */{
                  0: /* Flush */{
                    0: /* End_of_format */0,
                    length: 1,
                    tag: 10
                  },
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 12
              },
              "a%!%!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf(" a", /* Format */[
              /* String_literal */{
                0: " a",
                1: /* Flush */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 11
              },
              " a%!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf("a ", /* Format */[
              /* String_literal */{
                0: "a ",
                1: /* Flush */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 11
              },
              "a %!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Flush */{
                0: /* End_of_format */0,
                length: 1,
                tag: 10
              },
              "%!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf(" ", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* Flush */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 12
              },
              " %!"
            ]), /* true */1) && Caml_curry.app1(Scanf.sscanf("", /* Format */[
              /* Char_literal */{
                0: /* " " */32,
                1: /* Flush */{
                  0: /* End_of_format */0,
                  length: 1,
                  tag: 10
                },
                length: 2,
                tag: 12
              },
              " %!"
            ]), /* true */1)) {
    return Caml_curry.app1(Scanf.sscanf("", /* Format */[
                    /* Char_literal */{
                      0: /* " " */32,
                      1: /* Flush */{
                        0: /* Flush */{
                          0: /* End_of_format */0,
                          length: 1,
                          tag: 10
                        },
                        length: 1,
                        tag: 10
                      },
                      length: 2,
                      tag: 12
                    },
                    " %!%!"
                  ]), /* true */1);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1020, characters 5-12', test38(/* () */0));

function test39() {
  var is_empty_buff = function (ib) {
    if (Caml_curry.app1(Scanf.Scanning[/* beginning_of_input */10], ib)) {
      return Caml_curry.app1(Scanf.Scanning[/* end_of_input */9], ib);
    }
    else {
      return /* false */0;
    }
  };
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "");
  if (is_empty_buff(ib)) {
    return is_empty_buff(ib);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1036, characters 5-12', test39(/* () */0));

function test40() {
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "cba");
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Scan_char_set */{
                    0: /* None */0,
                    1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf9\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    2: /* String */{
                      0: /* No_padding */0,
                      1: /* Flush */{
                        0: /* End_of_format */0,
                        length: 1,
                        tag: 10
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 3,
                    tag: 20
                  },
                  "%[^ab]%s%!"
                ]), function (s1, s2) {
              if (s1 === "c") {
                return +(s2 === "ba");
              }
              else {
                return /* false */0;
              }
            });
}

test('File "tscanf_test.ml", line 1046, characters 5-12', test40(/* () */0));

function test41() {
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "cba");
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Scan_char_set */{
                    0: /* None */0,
                    1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf1\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                    2: /* Scan_char_set */{
                      0: /* None */0,
                      1: "\0\0\0\0\0\0\0\0\0\0\0\0\x0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      2: /* Flush */{
                        0: /* End_of_format */0,
                        length: 1,
                        tag: 10
                      },
                      length: 3,
                      tag: 20
                    },
                    length: 3,
                    tag: 20
                  },
                  "%[^abc]%[cba]%!"
                ]), function (s1, s2) {
              if (s1 === "") {
                return +(s2 === "cba");
              }
              else {
                return /* false */0;
              }
            });
}

test('File "tscanf_test.ml", line 1055, characters 5-12', test41(/* () */0));

function test42() {
  var s = "defcbaaghi";
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], s);
  if (Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf1\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                2: /* Scan_char_set */{
                  0: /* None */0,
                  1: "\0\0\0\0\0\0\0\0\0\0\0\0\x0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  2: /* String */{
                    0: /* No_padding */0,
                    1: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 3,
                  tag: 20
                },
                length: 3,
                tag: 20
              },
              "%[^abc]%[abc]%s%!"
            ]), function (s1, s2, s3) {
          if (s1 === "def" && s2 === "cbaa") {
            return +(s3 === "ghi");
          }
          else {
            return /* false */0;
          }
        })) {
    var ib$1 = Caml_curry.app1(Scanf.Scanning[/* from_string */6], s);
    return Caml_curry.app1(Scanf.bscanf(ib$1, /* Format */[
                    /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_lit */{
                        0: /* Scan_indic */{
                          0: /* "\t" */9,
                          length: 1,
                          tag: 2
                        },
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 17
                      },
                      length: 2,
                      tag: 2
                    },
                    "%s@\t"
                  ]), function (s) {
                return +(s === "defcbaaghi");
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1067, characters 5-12', test42(/* () */0));

var ib$1 = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "");

function match_000() {
  return Caml_curry.app1(Scanf.bscanf(ib$1, /* Format */[
                  /* Int */{
                    0: /* Int_i */3,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 4,
                    tag: 4
                  },
                  "%i%!"
                ]), function (i) {
              return i;
            });
}

function match_001() {
  return Caml_curry.app1(Scanf.bscanf(ib$1, /* Format */[
                  /* Flush */{
                    0: /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    length: 1,
                    tag: 10
                  },
                  "%!%i"
                ]), function (i) {
              return i;
            });
}

var test44 = match_001;

var test43 = match_000;

+(Testing.test_raises_this_exc(Caml_builtin_exceptions.end_of_file)(test43, /* () */0) && Testing.test_raises_this_exc(Caml_builtin_exceptions.end_of_file)(test44, /* () */0));

function test45() {
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_string */6], "12.2");
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* Scan_char_set */{
                    0: /* None */0,
                    1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                    2: /* Char_literal */{
                      0: /* "." */46,
                      1: /* Scan_char_set */{
                        0: /* None */0,
                        1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* String */{
                          0: /* No_padding */0,
                          1: /* Flush */{
                            0: /* End_of_format */0,
                            length: 1,
                            tag: 10
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 3,
                        tag: 20
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 3,
                    tag: 20
                  },
                  "%[0-9].%[0-9]%s%!"
                ]), function (s1, s2, s3) {
              if (s1 === "12" && s2 === "2") {
                return +(s3 === "");
              }
              else {
                return /* false */0;
              }
            });
}

test('File "tscanf_test.ml", line 1090, characters 5-12', test45(/* () */0));

function match_000$1() {
  return Caml_curry.app3(Printf.sprintf(/* Format */[
                  /* Int */{
                    0: /* Int_i */3,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Format_subst */{
                        0: /* None */0,
                        1: /* String_ty */{
                          0: /* End_of_fmtty */0,
                          length: 1,
                          tag: 1
                        },
                        2: /* Char_literal */{
                          0: /* "." */46,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 12
                        },
                        length: 3,
                        tag: 14
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 4
                  },
                  "%i %(%s%)."
                ]), 1, /* Format */[
              /* String_literal */{
                0: "spells one, ",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              "spells one, %s"
            ], "in english");
}

function match_001$1() {
  return Caml_curry.app3(Printf.sprintf(/* Format */[
                  /* Int */{
                    0: /* Int_i */3,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Format_arg */{
                        0: /* None */0,
                        1: /* String_ty */{
                          0: /* End_of_fmtty */0,
                          length: 1,
                          tag: 1
                        },
                        2: /* String_literal */{
                          0: ", ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* "." */46,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        length: 3,
                        tag: 13
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 4
                  },
                  "%i %{%s%}, %s."
                ]), 1, /* Format */[
              /* String_literal */{
                0: "spells one ",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              "spells one %s"
            ], "in english");
}

var test47 = match_001$1;

var test46 = match_000$1;

test('File "tscanf_test.ml", line 1104, characters 5-12', +(Caml_curry.app1(test46, /* () */0) === "1 spells one, in english."));

test('File "tscanf_test.ml", line 1106, characters 5-12', +(Caml_curry.app1(test47, /* () */0) === "1 %s, in english."));

function test48() {
  var test_meta_read = function (s, fmt, efmt) {
    return Caml_obj.caml_equal(Scanf.format_from_string(s, fmt), efmt);
  };
  var fmt = /* Format */[
    /* Int */{
      0: /* Int_i */3,
      1: /* No_padding */0,
      2: /* No_precision */0,
      3: /* End_of_format */0,
      length: 4,
      tag: 4
    },
    "%i"
  ];
  if (test_meta_read("%i", fmt, fmt)) {
    if (test_meta_read("%i", /* Format */[
            /* Int */{
              0: /* Int_d */0,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* End_of_format */0,
              length: 4,
              tag: 4
            },
            "%d"
          ], /* Format */[
            /* Int */{
              0: /* Int_i */3,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* End_of_format */0,
              length: 4,
              tag: 4
            },
            "%i"
          ])) {
      if (Caml_curry.app1(Scanf.sscanf('12 "%i"89 ', /* Format */[
                  /* Int */{
                    0: /* Int_i */3,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Format_arg */{
                        0: /* None */0,
                        1: /* Int_ty */{
                          0: /* End_of_fmtty */0,
                          length: 1,
                          tag: 2
                        },
                        2: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* Flush */{
                              0: /* End_of_format */0,
                              length: 1,
                              tag: 10
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 3,
                        tag: 13
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 4
                  },
                  "%i %{%d%}%s %!"
                ]), function (i, f, s) {
              if (i === 12 && Caml_obj.caml_equal(f, /* Format */[
                      /* Int */{
                        0: /* Int_i */3,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      "%i"
                    ])) {
                return +(s === "89");
              }
              else {
                return /* false */0;
              }
            })) {
        var k = function (s) {
          return Caml_curry.app1(Scanf.sscanf(s, /* Format */[
                          /* Format_subst */{
                            0: /* None */0,
                            1: /* Float_ty */{
                              0: /* End_of_fmtty */0,
                              length: 1,
                              tag: 6
                            },
                            2: /* End_of_format */0,
                            length: 3,
                            tag: 14
                          },
                          "%(%f%)"
                        ]), function (_, i) {
                      return i;
                    });
        };
        if (k('" : %1f": 987654321') === 9.0) {
          if (k('" : %2f": 987654321') === 98.0) {
            if (k('" : %3f": 9.87654321') === 9.8) {
              if (k('" : %4f": 9.87654321') === 9.87) {
                var h = function (s) {
                  return Caml_curry.app1(Scanf.sscanf(s, /* Format */[
                                  /* String_literal */{
                                    0: "Read integers with ",
                                    1: /* Format_subst */{
                                      0: /* None */0,
                                      1: /* Int_ty */{
                                        0: /* End_of_fmtty */0,
                                        length: 1,
                                        tag: 2
                                      },
                                      2: /* End_of_format */0,
                                      length: 3,
                                      tag: 14
                                    },
                                    length: 2,
                                    tag: 11
                                  },
                                  "Read integers with %(%i%)"
                                ]), function (_, i) {
                              return i;
                            });
                };
                if (h('Read integers with "%1d"987654321') === 9) {
                  if (h('Read integers with "%2d"987654321') === 98) {
                    if (h('Read integers with "%3u"987654321') === 987) {
                      if (h('Read integers with "%4x"987654321') === 39030) {
                        var i = function (s) {
                          return Caml_curry.app1(Scanf.sscanf(s, /* Format */[
                                          /* String_literal */{
                                            0: "with ",
                                            1: /* Format_subst */{
                                              0: /* None */0,
                                              1: /* Int_ty */{
                                                0: /* String_ty */{
                                                  0: /* End_of_fmtty */0,
                                                  length: 1,
                                                  tag: 1
                                                },
                                                length: 1,
                                                tag: 2
                                              },
                                              2: /* End_of_format */0,
                                              length: 3,
                                              tag: 14
                                            },
                                            length: 2,
                                            tag: 11
                                          },
                                          "with %(%i %s%)"
                                        ]), function (_, amount, currency) {
                                      return /* tuple */[
                                              amount,
                                              currency
                                            ];
                                    });
                        };
                        if (Caml_obj.caml_equal(i('with " : %d %s" :        21 euros'), /* tuple */[
                                21,
                                "euros"
                              ])) {
                          if (Caml_obj.caml_equal(i('with " : %d %s" : 987654321 dollars'), /* tuple */[
                                  987654321,
                                  "dollars"
                                ])) {
                            if (Caml_obj.caml_equal(i('with " : %u %s" :     54321 pounds'), /* tuple */[
                                    54321,
                                    "pounds"
                                  ])) {
                              if (Caml_obj.caml_equal(i('with " : %x %s" :       321 yens'), /* tuple */[
                                      801,
                                      "yens"
                                    ])) {
                                var j = function (s) {
                                  return Caml_curry.app1(Scanf.sscanf(s, /* Format */[
                                                  /* String_literal */{
                                                    0: "with ",
                                                    1: /* Format_subst */{
                                                      0: /* None */0,
                                                      1: /* Int_ty */{
                                                        0: /* String_ty */{
                                                          0: /* End_of_fmtty */0,
                                                          length: 1,
                                                          tag: 1
                                                        },
                                                        length: 1,
                                                        tag: 2
                                                      },
                                                      2: /* End_of_format */0,
                                                      length: 3,
                                                      tag: 14
                                                    },
                                                    length: 2,
                                                    tag: 11
                                                  },
                                                  "with %(%i %_s %s%)"
                                                ]), function (_, amount, currency) {
                                              return /* tuple */[
                                                      amount,
                                                      currency
                                                    ];
                                            });
                                };
                                if (Caml_obj.caml_equal(j('with " : %1d %_s %s" : 987654321 euros'), /* tuple */[
                                        9,
                                        "euros"
                                      ]) && Caml_obj.caml_equal(j('with " : %2d %_s %s" : 987654321 dollars'), /* tuple */[
                                        98,
                                        "dollars"
                                      ]) && Caml_obj.caml_equal(j('with " : %3u %_s %s" : 987654321 pounds'), /* tuple */[
                                        987,
                                        "pounds"
                                      ])) {
                                  return Caml_obj.caml_equal(j('with " : %4x %_s %s" : 987654321 yens'), /* tuple */[
                                              39030,
                                              "yens"
                                            ]);
                                }
                                else {
                                  return /* false */0;
                                }
                              }
                              else {
                                return /* false */0;
                              }
                            }
                            else {
                              return /* false */0;
                            }
                          }
                          else {
                            return /* false */0;
                          }
                        }
                        else {
                          return /* false */0;
                        }
                      }
                      else {
                        return /* false */0;
                      }
                    }
                    else {
                      return /* false */0;
                    }
                  }
                  else {
                    return /* false */0;
                  }
                }
                else {
                  return /* false */0;
                }
              }
              else {
                return /* false */0;
              }
            }
            else {
              return /* false */0;
            }
          }
          else {
            return /* false */0;
          }
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* false */0;
    }
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1157, characters 5-12', test48(/* () */0));

function test49() {
  if (Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* End_of_format */0,
                length: 3,
                tag: 20
              },
              "%[\\]"
            ]), function (s) {
          return +(s === "");
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 3,
                tag: 20
              },
              "%[\\]%s"
            ]), function (s, t) {
          return s === "" ? +(t === "as") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* String */{
                  0: /* No_padding */0,
                  1: /* Flush */{
                    0: /* End_of_format */0,
                    length: 1,
                    tag: 10
                  },
                  length: 2,
                  tag: 2
                },
                length: 3,
                tag: 20
              },
              "%[\\]%s%!"
            ]), function (s, t) {
          return s === "" ? +(t === "as") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0@\0\0\0\0\0\0\x02\0\0\x04\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* End_of_format */0,
                length: 3,
                tag: 20
              },
              "%[a..z]"
            ]), function (s) {
          return +(s === "a");
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* End_of_format */0,
                length: 3,
                tag: 20
              },
              "%[a-z]"
            ]), function (s) {
          return +(s === "as");
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0@\0\0\0\0\0\0\x02\0\0\x04\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 3,
                tag: 20
              },
              "%[a..z]%s"
            ]), function (s, t) {
          return s === "a" ? +(t === "s") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 3,
                tag: 20
              },
              "%[a-z]%s"
            ]), function (s, t) {
          return s === "as" ? +(t === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("-as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0 \0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* End_of_format */0,
                length: 3,
                tag: 20
              },
              "%[-a-z]"
            ]), function (s) {
          return +(s === "-as");
        }) && Caml_curry.app1(Scanf.sscanf("-as", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0 \0\0\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* Formatting_lit */{
                  0: /* Scan_indic */{
                    0: /* "s" */115,
                    length: 1,
                    tag: 2
                  },
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 17
                },
                length: 3,
                tag: 20
              },
              "%[-a-z]@s"
            ]), function (s) {
          return +(s === "-a");
        }) && Caml_curry.app1(Scanf.sscanf("-as", /* Format */[
              /* Char_literal */{
                0: /* "-" */45,
                1: /* Scan_char_set */{
                  0: /* None */0,
                  1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  2: /* Formatting_lit */{
                    0: /* Scan_indic */{
                      0: /* "s" */115,
                      length: 1,
                      tag: 2
                    },
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 17
                  },
                  length: 3,
                  tag: 20
                },
                length: 2,
                tag: 12
              },
              "-%[a]@s"
            ]), function (s) {
          return +(s === "a");
        }) && Caml_curry.app1(Scanf.sscanf("-asb", /* Format */[
              /* Char_literal */{
                0: /* "-" */45,
                1: /* Scan_char_set */{
                  0: /* None */0,
                  1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                  2: /* Formatting_lit */{
                    0: /* Scan_indic */{
                      0: /* "s" */115,
                      length: 1,
                      tag: 2
                    },
                    1: /* Char_literal */{
                      0: /* "b" */98,
                      1: /* Flush */{
                        0: /* End_of_format */0,
                        length: 1,
                        tag: 10
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 17
                  },
                  length: 3,
                  tag: 20
                },
                length: 2,
                tag: 12
              },
              "-%[a]@sb%!"
            ]), function (s) {
          return +(s === "a");
        })) {
    return Caml_curry.app1(Scanf.sscanf("-asb", /* Format */[
                    /* Char_literal */{
                      0: /* "-" */45,
                      1: /* Scan_char_set */{
                        0: /* None */0,
                        1: "\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* Formatting_lit */{
                          0: /* Scan_indic */{
                            0: /* "s" */115,
                            length: 1,
                            tag: 2
                          },
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 17
                        },
                        length: 3,
                        tag: 20
                      },
                      length: 2,
                      tag: 12
                    },
                    "-%[a]@s%s"
                  ]), function (s, t) {
                if (s === "a") {
                  return +(t === "b");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1176, characters 5-12', test49(/* () */0));

function next_char(ob, _) {
  var s = Buffer.contents(ob);
  var len = s.length;
  if (len) {
    var c = s.charCodeAt(0);
    ob[/* position */1] = 0;
    Buffer.add_string(ob, $$String.sub(s, 1, len - 1 | 0));
    return c;
  }
  else {
    throw Caml_builtin_exceptions.end_of_file;
  }
}

function send_string(ob, s) {
  Buffer.add_string(ob, s);
  return Buffer.add_char(ob, /* "\n" */10);
}

function send_int(ob, i) {
  return send_string(ob, "" + i);
}

var count = [0];

function reader(ib, ob) {
  if (Caml_curry.app1(Scanf.Scanning[/* beginning_of_input */10], ib)) {
    count[0] = 0;
    send_string(ob, "start");
    return writer(ib, ob);
  }
  else {
    return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                    /* Scan_char_set */{
                      0: /* None */0,
                      1: "\xff\xfb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                      2: /* Char_literal */{
                        0: /* "\n" */10,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 3,
                      tag: 20
                    },
                    "%[^\n]\n"
                  ]), function (s) {
                if (s === "stop") {
                  send_string(ob, "stop");
                  return writer(ib, ob);
                }
                else {
                  var l = s.length;
                  count[0] = l + count[0] | 0;
                  if (count[0] >= 100) {
                    send_string(ob, "stop");
                    send_string(ob, "" + count[0]);
                  }
                  else {
                    send_string(ob, "" + l);
                  }
                  return writer(ib, ob);
                }
              });
  }
}

function writer(ib, ob) {
  return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                  /* String */{
                    0: /* No_padding */0,
                    1: /* Char_literal */{
                      0: /* "\n" */10,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 2
                  },
                  "%s\n"
                ]), function (s) {
              switch (s) {
                case "start" : 
                    send_string(ob, "Hello World!");
                    return reader(ib, ob);
                case "stop" : 
                    return Caml_curry.app1(Scanf.bscanf(ib, /* Format */[
                                    /* Int */{
                                      0: /* Int_i */3,
                                      1: /* No_padding */0,
                                      2: /* No_precision */0,
                                      3: /* End_of_format */0,
                                      length: 4,
                                      tag: 4
                                    },
                                    "%i"
                                  ]), function (i) {
                                return i;
                              });
                default:
                  var i = Caml_format.caml_int_of_string(s);
                  send_string(ob, "" + i);
                  return reader(ib, ob);
              }
            });
}

function go() {
  var ob = Buffer.create(17);
  var ib = Caml_curry.app1(Scanf.Scanning[/* from_function */7], function (param) {
        return next_char(ob, param);
      });
  return reader(ib, ob);
}

function test50() {
  return +(go(/* () */0) === 100);
}

test('File "tscanf_test.ml", line 1228, characters 5-12', +(go(/* () */0) === 100));

function test51() {
  if (Caml_curry.app1(Scanf.sscanf("Hello", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* End_of_format */0,
                length: 2,
                tag: 2
              },
              "%s"
            ]), id) === "Hello" && Caml_curry.app1(Scanf.sscanf("Hello\n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n"
            ]), id) === "Hello" && Caml_curry.app1(Scanf.sscanf("Hello\n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* "\n" */10,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n%s%!"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld!", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n%s"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World!") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n%s"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.sscanf("Hello \n", /* Format */[
                    /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_lit */{
                        0: /* Force_newline */3,
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 17
                      },
                      length: 2,
                      tag: 2
                    },
                    "%s@\n%s"
                  ]), function (s1, s2) {
                if (s1 === "Hello ") {
                  return +(s2 === "");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1248, characters 5-12', test51(/* () */0));

function test52() {
  if (Caml_curry.app1(Scanf.sscanf("Hello\n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n"
            ]), id) === "Hello" && Caml_curry.app1(Scanf.sscanf("Hello", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n"
            ]), id) === "Hello" && Caml_curry.app1(Scanf.sscanf("Hello", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Formatting_lit */{
                    0: /* Force_newline */3,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 17
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s@\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n%s%!"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld!", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Formatting_lit */{
                      0: /* Force_newline */3,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 17
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n%s@\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World!") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Force_newline */3,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@\n%s"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello \n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Formatting_lit */{
                    0: /* Force_newline */3,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 17
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s@\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === " ") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello \n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Ignored_param */{
                    0: /* Ignored_scan_char_set */{
                      0: /* Some */[1],
                      1: "\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                      length: 2,
                      tag: 9
                    },
                    1: /* Char_literal */{
                      0: /* "\n" */10,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 23
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%s%s%_1[ ]\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello \n", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Ignored_param */{
                  0: /* Ignored_scan_char_set */{
                    0: /* Some */[1],
                    1: "\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                    length: 2,
                    tag: 9
                  },
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Char_literal */{
                      0: /* "\n" */10,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 23
                },
                length: 2,
                tag: 2
              },
              "%s%_1[ ]%s\n"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n%s%!"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld!", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Flush */{
                      0: /* End_of_format */0,
                      length: 1,
                      tag: 10
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n%s%!"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World!") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello\nWorld!", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* Formatting_lit */{
                      0: /* Scan_indic */{
                        0: /* "!" */33,
                        length: 1,
                        tag: 2
                      },
                      1: /* Flush */{
                        0: /* End_of_format */0,
                        length: 1,
                        tag: 10
                      },
                      length: 2,
                      tag: 17
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 2,
                tag: 2
              },
              "%s\n%s@!%!"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "World") : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("Hello{foo}", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_gen */{
                  0: /* Open_tag */{
                    0: /* Format */[
                      /* End_of_format */0,
                      ""
                    ],
                    length: 1,
                    tag: 0
                  },
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 18
                },
                length: 2,
                tag: 2
              },
              "%s@{%s"
            ]), function (s1, s2) {
          return s1 === "Hello" ? +(s2 === "foo}") : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.sscanf("Hello[foo]", /* Format */[
                    /* String */{
                      0: /* No_padding */0,
                      1: /* Formatting_gen */{
                        0: /* Open_box */{
                          0: /* Format */[
                            /* End_of_format */0,
                            ""
                          ],
                          length: 1,
                          tag: 1
                        },
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 18
                      },
                      length: 2,
                      tag: 2
                    },
                    "%s@[%s"
                  ]), function (s1, s2) {
                if (s1 === "Hello") {
                  return +(s2 === "foo]");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1286, characters 5-12', test52(/* () */0));

function test53() {
  if (Caml_curry.app1(Scanf.sscanf("123", /* Format */[
              /* Nativeint */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 6
              },
              "%nd"
            ]), id) === 123 && Caml_curry.app1(Scanf.sscanf("124", /* Format */[
              /* Nativeint */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 6
              },
              "%nd"
            ]), function (i) {
          return +(i - 1 === 123);
        }) && Caml_curry.app1(Scanf.sscanf("123", /* Format */[
              /* Int32 */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 5
              },
              "%ld"
            ]), id) === 123 && Caml_curry.app1(Scanf.sscanf("124", /* Format */[
              /* Int32 */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* End_of_format */0,
                length: 4,
                tag: 5
              },
              "%ld"
            ]), function (i) {
          return +((i + 1 | 0) === 125);
        }) && Caml_int64.eq(Caml_curry.app1(Scanf.sscanf("123", /* Format */[
                  /* Int64 */{
                    0: /* Int_d */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* End_of_format */0,
                    length: 4,
                    tag: 7
                  },
                  "%Ld"
                ]), id), /* int64 */[
          0,
          123
        ])) {
    return Caml_curry.app1(Scanf.sscanf("124", /* Format */[
                    /* Int64 */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 7
                    },
                    "%Ld"
                  ]), function (i) {
                return Caml_int64.eq(Caml_int64.sub(i, /* int64 */[
                                0,
                                1
                              ]), /* int64 */[
                            0,
                            123
                          ]);
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1301, characters 5-12', test53(/* () */0));

function test56() {
  var g = function (s) {
    return Caml_curry.app1(Scanf.sscanf(s, /* Format */[
                    /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Scan_get_counter */{
                        0: /* Char_counter */1,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 21
                      },
                      length: 4,
                      tag: 4
                    },
                    "%d%n"
                  ]), function (i, n) {
                return /* tuple */[
                        i,
                        n
                      ];
              });
  };
  if (Caml_obj.caml_equal(g("99"), /* tuple */[
          99,
          2
        ]) && Caml_obj.caml_equal(g("99 syntaxes all in a row"), /* tuple */[
          99,
          2
        ])) {
    return Caml_obj.caml_equal(g("-20 degrees Celsius"), /* tuple */[
                -20,
                3
              ]);
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1316, characters 5-12', test56(/* () */0));

function test57() {
  var test_format_scan = function (s, fmt, efmt) {
    return Caml_obj.caml_equal(Scanf.format_from_string(s, fmt), efmt);
  };
  if (test_format_scan(" %i ", /* Format */[
          /* Int */{
            0: /* Int_i */3,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* End_of_format */0,
            length: 4,
            tag: 4
          },
          "%i"
        ], /* Format */[
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
        ]) && test_format_scan("%i", /* Format */[
          /* Int */{
            0: /* Int_d */0,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* End_of_format */0,
            length: 4,
            tag: 4
          },
          "%d"
        ], /* Format */[
          /* Int */{
            0: /* Int_i */3,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* End_of_format */0,
            length: 4,
            tag: 4
          },
          "%i"
        ]) && test_format_scan("Read an int %i then a string %s.", /* Format */[
          /* String_literal */{
            0: "Spec",
            1: /* Int */{
              0: /* Int_d */0,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: "ifi",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* String_literal */{
                    0: "cation",
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 11
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          "Spec%difi%scation"
        ], /* Format */[
          /* String_literal */{
            0: "Read an int ",
            1: /* Int */{
              0: /* Int_i */3,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: " then a string ",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* "." */46,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          "Read an int %i then a string %s."
        ]) && test_format_scan('Read an int %i then a string "%s".', /* Format */[
          /* String_literal */{
            0: "Spec",
            1: /* Int */{
              0: /* Int_d */0,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: "ifi",
                1: /* Caml_string */{
                  0: /* No_padding */0,
                  1: /* String_literal */{
                    0: "cation",
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 11
                  },
                  length: 2,
                  tag: 3
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          "Spec%difi%Scation"
        ], /* Format */[
          /* String_literal */{
            0: "Read an int ",
            1: /* Int */{
              0: /* Int_i */3,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: ' then a string "',
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* String_literal */{
                    0: '".',
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 11
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          'Read an int %i then a string "%s".'
        ]) && test_format_scan('Read an int %i then a string "%s".', /* Format */[
          /* String_literal */{
            0: "Spec",
            1: /* Int */{
              0: /* Int_d */0,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: "ifi",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* String_literal */{
                    0: "cation",
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 11
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          "Spec%difi%scation"
        ], /* Format */[
          /* String_literal */{
            0: "Read an int ",
            1: /* Int */{
              0: /* Int_i */3,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* String_literal */{
                0: ' then a string "',
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* String_literal */{
                    0: '".',
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 11
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          'Read an int %i then a string "%s".'
        ])) {
    return Caml_curry.app1(Scanf.sscanf('12 "%i"89 ', /* Format */[
                    /* Int */{
                      0: /* Int_i */3,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* " " */32,
                        1: /* Format_arg */{
                          0: /* None */0,
                          1: /* Int_ty */{
                            0: /* End_of_fmtty */0,
                            length: 1,
                            tag: 2
                          },
                          2: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* Flush */{
                                0: /* End_of_format */0,
                                length: 1,
                                tag: 10
                              },
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 3,
                          tag: 13
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    "%i %{%d%}%s %!"
                  ]), function (i, f, s) {
                if (i === 12 && Caml_obj.caml_equal(f, /* Format */[
                        /* Int */{
                          0: /* Int_i */3,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 4
                        },
                        "%i"
                      ])) {
                  return +(s === "89");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1357, characters 5-12', test57(/* () */0));

function test58() {
  if (Caml_curry.app1(Scanf.sscanf("string1%string2", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Escaped_percent */6,
                  1: /* Char_literal */{
                    0: /* "s" */115,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@%%s"
            ]), id) === "string1" && Caml_curry.app1(Scanf.sscanf("string1%string2", /* Format */[
              /* String */{
                0: /* No_padding */0,
                1: /* Formatting_lit */{
                  0: /* Escaped_percent */6,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 17
                },
                length: 2,
                tag: 2
              },
              "%s@%%%s"
            ]), Pervasives.$caret) === "string1string2" && Caml_curry.app1(Scanf.sscanf("string1@string2", /* Format */[
              /* Scan_char_set */{
                0: /* None */0,
                1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                2: /* Char_literal */{
                  0: /* "@" */64,
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 12
                },
                length: 3,
                tag: 20
              },
              "%[a-z0-9]@%s"
            ]), Pervasives.$caret) === "string1string2") {
    return +(Caml_curry.app1(Scanf.sscanf("string1@%string2", /* Format */[
                      /* Scan_char_set */{
                        0: /* None */0,
                        1: "\0\0\0\0\0\0\xff\x03\0\0\0\0\xfe\xff\xff\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
                        2: /* Char_literal */{
                          0: /* "@" */64,
                          1: /* Char_literal */{
                            0: /* "%" */37,
                            1: /* String */{
                              0: /* No_padding */0,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 2
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 3,
                        tag: 20
                      },
                      "%[a-z0-9]%@%%%s"
                    ]), Pervasives.$caret) === "string1string2");
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1367, characters 5-12', test58(/* () */0));

test('File "tscanf_test.ml", line 1371, characters 14-21', /* true */1);

function test60() {
  if (Caml_curry.app1(Scanf.sscanf("abc", /* Format */[
              /* Scan_next_char */{
                0: /* Scan_next_char */{
                  0: /* Char */{
                    0: /* Scan_get_counter */{
                      0: /* Char_counter */1,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 21
                    },
                    length: 1,
                    tag: 0
                  },
                  length: 1,
                  tag: 22
                },
                length: 1,
                tag: 22
              },
              "%0c%0c%c%n"
            ]), function (c1, c2, c3, n) {
          return c1 === /* "a" */97 && c2 === /* "a" */97 && c3 === /* "a" */97 ? +(n === 1) : /* false */0;
        }) && Caml_curry.app1(Scanf.sscanf("abc", /* Format */[
              /* String */{
                0: /* Lit_padding */{
                  0: /* Right */1,
                  1: 0,
                  length: 2,
                  tag: 0
                },
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 2
              },
              "%0s%s"
            ]), function (s1, s2) {
          return s1 === "" ? +(s2 === "abc") : /* false */0;
        })) {
    return Caml_curry.app1(Scanf.sscanf("abc", /* Format */[
                    /* String */{
                      0: /* Lit_padding */{
                        0: /* Right */1,
                        1: 1,
                        length: 2,
                        tag: 0
                      },
                      1: /* String */{
                        0: /* No_padding */0,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 2
                      },
                      length: 2,
                      tag: 2
                    },
                    "%1s%s"
                  ]), function (s1, s2) {
                if (s1 === "a") {
                  return +(s2 === "bc");
                }
                else {
                  return /* false */0;
                }
              });
  }
  else {
    return /* false */0;
  }
}

test('File "tscanf_test.ml", line 1414, characters 5-12', test60(/* () */0));

Mt.from_pair_suites("tscanf_test.ml", suites[0]);

var tscanf_data_file_lines = /* :: */[
  /* tuple */[
    "Objective",
    "Caml"
  ],
  /* [] */0
];

exports.suites                 = suites;
exports.test_id                = test_id;
exports.eq                     = eq;
exports.test                   = test;
exports.id                     = id;
exports.test0                  = test0;
exports.test1                  = test1;
exports.test2                  = test2;
exports.test3                  = test3;
exports.test4                  = test4;
exports.test5                  = test5;
exports.test6                  = test6;
exports.test7                  = test7;
exports.verify_read            = verify_read;
exports.verify_scan_Chars      = verify_scan_Chars;
exports.test8                  = test8;
exports.unit                   = unit;
exports.test_fmt               = test_fmt;
exports.test9_string           = test9_string;
exports.test_S                 = test_S;
exports.test9                  = test9;
exports.test10                 = test10;
exports.test11                 = test11;
exports.test110                = test110;
exports.test111                = test111;
exports.ib                     = ib;
exports.f                      = f;
exports.test12                 = test12;
exports.g                      = g;
exports.test13                 = test13;
exports.test14                 = test14;
exports.test15                 = test15;
exports.test16                 = test16;
exports.test17                 = test17;
exports.test18                 = test18;
exports.test19                 = test19;
exports.test20                 = test20;
exports.test21                 = test21;
exports.scan_rest              = scan_rest$1;
exports.test22                 = test22;
exports.test23                 = test23;
exports.test24                 = test24;
exports.test25                 = test25;
exports.test26                 = test26;
exports.test27                 = test27;
exports.scan_String_elem       = scan_String_elem;
exports.scan_String_list       = scan_String_list;
exports.test28                 = test28;
exports.scan_int_elem          = scan_int_elem$1;
exports.test29                 = test29;
exports.scan_string_elem       = scan_string_elem$1;
exports.test30                 = test30;
exports.scan_elem              = scan_elem;
exports.test31                 = test31;
exports.test32                 = test32;
exports.test33                 = test33;
exports.test34                 = test34;
exports.scan_elems             = scan_elems$9;
exports.scan_list              = scan_list$4;
exports.scan_float             = scan_float;
exports.scan_int_list          = scan_int_list$8;
exports.scan_string_list       = scan_string_list$2;
exports.scan_bool_list         = scan_bool_list;
exports.scan_char_list         = scan_char_list;
exports.test340                = test340;
exports.scan_list_list         = scan_list_list;
exports.scan_float_item        = scan_float_item;
exports.scan_float_list        = scan_float_list;
exports.scan_float_list_list   = scan_float_list_list$1;
exports.test35                 = test35;
exports.read_elems             = read_elems;
exports.read_list              = read_list;
exports.make_read_elem         = make_read_elem;
exports.scan_List              = scan_List;
exports.test36                 = test36;
exports.test37                 = test37;
exports.test38                 = test38;
exports.test39                 = test39;
exports.test40                 = test40;
exports.test41                 = test41;
exports.test42                 = test42;
exports.test43                 = test43;
exports.test44                 = test44;
exports.test45                 = test45;
exports.test46                 = test46;
exports.test47                 = test47;
exports.test48                 = test48;
exports.test49                 = test49;
exports.next_char              = next_char;
exports.send_string            = send_string;
exports.send_int               = send_int;
exports.reader                 = reader;
exports.writer                 = writer;
exports.go                     = go;
exports.test50                 = test50;
exports.test51                 = test51;
exports.test52                 = test52;
exports.test53                 = test53;
exports.test56                 = test56;
exports.tscanf_data_file_lines = tscanf_data_file_lines;
exports.test57                 = test57;
exports.test58                 = test58;
exports.test60                 = test60;
/*  Not a pure module */
