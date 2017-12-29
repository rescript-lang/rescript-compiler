'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function tst01(s) {
  if (s === "") {
    return 0;
  } else {
    return 1;
  }
}

if (tst01("") !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          20,
          2
        ]
      ];
}

if (tst01("\0\0\0\x03") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          21,
          2
        ]
      ];
}

if (tst01("\0\0\0\0\0\0\0\x07") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          22,
          2
        ]
      ];
}

function tst02(s) {
  var len = s.length;
  if (s === "") {
    if (len < 0) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "stringmatch_test.ml",
              30,
              23
            ]
          ];
    } else {
      return 1;
    }
  } else if (len) {
    if (s === "A") {
      return 2;
    } else {
      return 3;
    }
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "stringmatch_test.ml",
            32,
            22
          ]
        ];
  }
}

if (tst02("") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          37,
          2
        ]
      ];
}

if (tst02("A") !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          38,
          2
        ]
      ];
}

if (tst02("B") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          39,
          2
        ]
      ];
}

if (tst02("\0\0\0\0\0\0\0\x07") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          40,
          2
        ]
      ];
}

if (tst02("\0\0\0\x03") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          41,
          2
        ]
      ];
}

function tst03(s) {
  switch (s) {
    case "app_const" : 
        return 5;
    case "app_const_const" : 
        return 9;
    case "app_const_env" : 
        return 11;
    case "app_const_meth" : 
        return 12;
    case "app_const_var" : 
        return 10;
    case "app_env" : 
        return 7;
    case "app_env_const" : 
        return 14;
    case "app_meth" : 
        return 8;
    case "app_meth_const" : 
        return 15;
    case "app_var" : 
        return 6;
    case "app_var_const" : 
        return 13;
    case "get_const" : 
        return 0;
    case "get_env" : 
        return 2;
    case "get_meth" : 
        return 3;
    case "get_var" : 
        return 1;
    case "meth_app_const" : 
        return 16;
    case "meth_app_env" : 
        return 18;
    case "meth_app_meth" : 
        return 19;
    case "meth_app_var" : 
        return 17;
    case "send_const" : 
        return 20;
    case "send_env" : 
        return 22;
    case "send_meth" : 
        return 23;
    case "send_var" : 
        return 21;
    case "set_var" : 
        return 4;
    default:
      return -1;
  }
}

if (tst03("get_const") !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          123,
          2
        ]
      ];
}

if (tst03("set_congt") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          124,
          2
        ]
      ];
}

if (tst03("get_var") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          125,
          2
        ]
      ];
}

if (tst03("gat_ver") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          126,
          2
        ]
      ];
}

if (tst03("get_env") !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          127,
          2
        ]
      ];
}

if (tst03("get_env") !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          128,
          2
        ]
      ];
}

if (tst03("get_meth") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          129,
          2
        ]
      ];
}

if (tst03("met_geth") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          130,
          2
        ]
      ];
}

if (tst03("set_var") !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          131,
          2
        ]
      ];
}

if (tst03("sev_tar") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          132,
          2
        ]
      ];
}

if (tst03("app_const") !== 5) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          133,
          2
        ]
      ];
}

if (tst03("ppa_const") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          134,
          2
        ]
      ];
}

if (tst03("app_var") !== 6) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          135,
          2
        ]
      ];
}

if (tst03("app_var") !== 6) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          136,
          2
        ]
      ];
}

if (tst03("app_env") !== 7) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          137,
          2
        ]
      ];
}

if (tst03("epp_anv") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          138,
          2
        ]
      ];
}

if (tst03("app_meth") !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          139,
          2
        ]
      ];
}

if (tst03("atp_meph") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          140,
          2
        ]
      ];
}

if (tst03("app_const_const") !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          141,
          2
        ]
      ];
}

if (tst03("app_const_const") !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          142,
          2
        ]
      ];
}

if (tst03("app_const_var") !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          143,
          2
        ]
      ];
}

if (tst03("atp_consp_var") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          144,
          2
        ]
      ];
}

if (tst03("app_const_env") !== 11) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          145,
          2
        ]
      ];
}

if (tst03("app_constne_v") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          146,
          2
        ]
      ];
}

if (tst03("app_const_meth") !== 12) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          147,
          2
        ]
      ];
}

if (tst03("spp_conat_meth") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          148,
          2
        ]
      ];
}

if (tst03("app_var_const") !== 13) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          149,
          2
        ]
      ];
}

if (tst03("app_va_rconst") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          150,
          2
        ]
      ];
}

if (tst03("app_env_const") !== 14) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          151,
          2
        ]
      ];
}

if (tst03("app_env_const") !== 14) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          152,
          2
        ]
      ];
}

if (tst03("app_meth_const") !== 15) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          153,
          2
        ]
      ];
}

if (tst03("app_teth_consm") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          154,
          2
        ]
      ];
}

if (tst03("meth_app_const") !== 16) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          155,
          2
        ]
      ];
}

if (tst03("math_epp_const") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          156,
          2
        ]
      ];
}

if (tst03("meth_app_var") !== 17) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          157,
          2
        ]
      ];
}

if (tst03("meth_app_var") !== 17) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          158,
          2
        ]
      ];
}

if (tst03("meth_app_env") !== 18) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          159,
          2
        ]
      ];
}

if (tst03("eeth_app_mnv") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          160,
          2
        ]
      ];
}

if (tst03("meth_app_meth") !== 19) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          161,
          2
        ]
      ];
}

if (tst03("meth_apt_meph") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          162,
          2
        ]
      ];
}

if (tst03("send_const") !== 20) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          163,
          2
        ]
      ];
}

if (tst03("tend_conss") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          164,
          2
        ]
      ];
}

if (tst03("send_var") !== 21) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          165,
          2
        ]
      ];
}

if (tst03("serd_van") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          166,
          2
        ]
      ];
}

if (tst03("send_env") !== 22) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          167,
          2
        ]
      ];
}

if (tst03("sen_denv") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          168,
          2
        ]
      ];
}

if (tst03("send_meth") !== 23) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          169,
          2
        ]
      ];
}

if (tst03("tend_mesh") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          170,
          2
        ]
      ];
}

function tst04(s) {
  switch (s) {
    case "AAAAAAAA" : 
        return 0;
    case "AAAAAAAAAAAAAAAA" : 
        return 1;
    case "AAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 2;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 3;
    case "BBBBBBBB" : 
        return 4;
    case "BBBBBBBBBBBBBBBB" : 
        return 5;
    case "BBBBBBBBBBBBBBBBBBBBBBBB" : 
        return 6;
    case "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" : 
        return 7;
    case "CCCCCCCC" : 
        return 8;
    case "CCCCCCCCCCCCCCCC" : 
        return 9;
    case "CCCCCCCCCCCCCCCCCCCCCCCC" : 
        return 10;
    case "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC" : 
        return 11;
    default:
      return -1;
  }
}

if (tst04("AAAAAAAA") !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          204,
          2
        ]
      ];
}

if (tst04("AAAAAAAAAAAAAAAA") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          205,
          2
        ]
      ];
}

if (tst04("AAAAAAAAAAAAAAAAAAAAAAAA") !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          206,
          2
        ]
      ];
}

if (tst04("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          207,
          2
        ]
      ];
}

if (tst04("BBBBBBBB") !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          208,
          2
        ]
      ];
}

if (tst04("BBBBBBBBBBBBBBBB") !== 5) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          209,
          2
        ]
      ];
}

if (tst04("BBBBBBBBBBBBBBBBBBBBBBBB") !== 6) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          210,
          2
        ]
      ];
}

if (tst04("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB") !== 7) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          211,
          2
        ]
      ];
}

if (tst04("CCCCCCCC") !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          212,
          2
        ]
      ];
}

if (tst04("CCCCCCCCCCCCCCCC") !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          213,
          2
        ]
      ];
}

if (tst04("CCCCCCCCCCCCCCCCCCCCCCCC") !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          214,
          2
        ]
      ];
}

if (tst04("CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC") !== 11) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          215,
          2
        ]
      ];
}

if (tst04("") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          216,
          2
        ]
      ];
}

if (tst04("DDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          217,
          2
        ]
      ];
}

if (tst04("DDDDDDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          218,
          2
        ]
      ];
}

if (tst04("AAADDDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          219,
          2
        ]
      ];
}

if (tst04("AAAAAAADDDDDDDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          220,
          2
        ]
      ];
}

if (tst04("AAAAAAADDDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          221,
          2
        ]
      ];
}

if (tst04("AAAAAAAAAAAAAAADDDD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          222,
          2
        ]
      ];
}

function tst05(s) {
  switch (s) {
    case "AAA" : 
        return 0;
    case "AAAA" : 
        return 1;
    case "AAAAA" : 
        return 2;
    case "AAAAAA" : 
        return 3;
    case "AAAAAAA" : 
        return 4;
    case "AAAAAAAAAAAA" : 
        return 5;
    case "AAAAAAAAAAAAAAAA" : 
        return 6;
    case "AAAAAAAAAAAAAAAAAAAA" : 
        return 7;
    case "BBB" : 
        return 8;
    case "BBBB" : 
        return 9;
    case "BBBBB" : 
        return 10;
    case "BBBBBB" : 
        return 11;
    case "BBBBBBB" : 
        return 12;
    default:
      return -1;
  }
}

if (tst05("AAA") !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          258,
          2
        ]
      ];
}

if (tst05("AAAA") !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          259,
          2
        ]
      ];
}

if (tst05("AAAAA") !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          260,
          2
        ]
      ];
}

if (tst05("AAAAAA") !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          261,
          2
        ]
      ];
}

if (tst05("AAAAAAA") !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          262,
          2
        ]
      ];
}

if (tst05("AAAAAAAAAAAA") !== 5) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          263,
          2
        ]
      ];
}

if (tst05("AAAAAAAAAAAAAAAA") !== 6) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          264,
          2
        ]
      ];
}

if (tst05("AAAAAAAAAAAAAAAAAAAA") !== 7) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          265,
          2
        ]
      ];
}

if (tst05("BBB") !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          266,
          2
        ]
      ];
}

if (tst05("BBBB") !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          267,
          2
        ]
      ];
}

if (tst05("BBBBB") !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          268,
          2
        ]
      ];
}

if (tst05("BBBBBB") !== 11) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          269,
          2
        ]
      ];
}

if (tst05("BBBBBBB") !== 12) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          270,
          2
        ]
      ];
}

if (tst05("") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          271,
          2
        ]
      ];
}

if (tst05("AAD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          272,
          2
        ]
      ];
}

if (tst05("AAAD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          273,
          2
        ]
      ];
}

if (tst05("AAAAAAD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          274,
          2
        ]
      ];
}

if (tst05("AAAAAAAD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          275,
          2
        ]
      ];
}

if (tst05("BBD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          276,
          2
        ]
      ];
}

if (tst05("BBBD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          277,
          2
        ]
      ];
}

if (tst05("BBBBBBD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          278,
          2
        ]
      ];
}

if (tst05("BBBBBBBD") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          279,
          2
        ]
      ];
}

var s00 = "and";

var t00 = "nad";

var s01 = "as";

var t01 = "sa";

var s02 = "assert";

var t02 = "asesrt";

var s03 = "begin";

var t03 = "negib";

var s04 = "class";

var t04 = "lcass";

var s05 = "constraint";

var t05 = "constiarnt";

var s06 = "do";

var t06 = "od";

var s07 = "done";

var t07 = "eond";

var s08 = "downto";

var t08 = "dowtno";

var s09 = "else";

var t09 = "lese";

var s10 = "end";

var t10 = "edn";

var s11 = "exception";

var t11 = "exception";

var s12 = "external";

var t12 = "external";

var s13 = "false";

var t13 = "fslae";

var s14 = "for";

var t14 = "ofr";

var s15 = "fun";

var t15 = "fnu";

var s16 = "function";

var t16 = "function";

var s17 = "functor";

var t17 = "ounctfr";

var s18 = "if";

var t18 = "fi";

var s19 = "in";

var t19 = "in";

var s20 = "include";

var t20 = "inculde";

var s21 = "inherit";

var t21 = "iehnrit";

var s22 = "initializer";

var t22 = "enitializir";

var s23 = "lazy";

var t23 = "zaly";

var s24 = "let";

var t24 = "elt";

var s25 = "match";

var t25 = "match";

var s26 = "method";

var t26 = "methdo";

var s27 = "module";

var t27 = "modelu";

var s28 = "mutable";

var t28 = "butamle";

var s29 = "new";

var t29 = "wen";

var s30 = "object";

var t30 = "objcet";

var s31 = "of";

var t31 = "of";

var s32 = "open";

var t32 = "epon";

var s33 = "or";

var t33 = "ro";

var s34 = "private";

var t34 = "privaet";

var s35 = "rec";

var t35 = "rec";

var s36 = "sig";

var t36 = "gis";

var s37 = "struct";

var t37 = "scrutt";

var s38 = "then";

var t38 = "hten";

var s39 = "to";

var t39 = "to";

var s40 = "true";

var t40 = "teur";

var s41 = "try";

var t41 = "try";

var s42 = "type";

var t42 = "pyte";

var s43 = "val";

var t43 = "val";

var s44 = "virtual";

var t44 = "vritual";

var s45 = "when";

var t45 = "whne";

var s46 = "while";

var t46 = "wlihe";

var s47 = "with";

var t47 = "iwth";

var s48 = "mod";

var t48 = "mod";

var s49 = "land";

var t49 = "alnd";

var s50 = "lor";

var t50 = "rol";

var s51 = "lxor";

var t51 = "lxor";

var s52 = "lsl";

var t52 = "lsl";

var s53 = "lsr";

var t53 = "lsr";

var s54 = "asr";

var t54 = "sar";

var s55 = "A";

var t55 = "A";

var s56 = "AA";

var t56 = "AA";

var s57 = "AAA";

var t57 = "AAA";

var s58 = "AAAA";

var t58 = "AAAA";

var s59 = "AAAAA";

var t59 = "AAAAA";

var s60 = "AAAAAA";

var t60 = "AAAAAA";

var s61 = "AAAAAAA";

var t61 = "AAAAAAA";

var s62 = "AAAAAAAA";

var t62 = "AAAAAAAA";

var s63 = "AAAAAAAAA";

var t63 = "AAAAAAAAA";

var s64 = "AAAAAAAAAA";

var t64 = "AAAAAAAAAA";

var s65 = "AAAAAAAAAAA";

var t65 = "AAAAAAAAAAA";

var s66 = "AAAAAAAAAAAA";

var t66 = "AAAAAAAAAAAA";

var s67 = "AAAAAAAAAAAAA";

var t67 = "AAAAAAAAAAAAA";

var s68 = "AAAAAAAAAAAAAA";

var t68 = "AAAAAAAAAAAAAA";

var s69 = "AAAAAAAAAAAAAAA";

var t69 = "AAAAAAAAAAAAAAA";

var s70 = "AAAAAAAAAAAAAAAA";

var t70 = "AAAAAAAAAAAAAAAA";

var s71 = "AAAAAAAAAAAAAAAAA";

var t71 = "AAAAAAAAAAAAAAAAA";

var s72 = "AAAAAAAAAAAAAAAAAA";

var t72 = "AAAAAAAAAAAAAAAAAA";

var s73 = "AAAAAAAAAAAAAAAAAAA";

var t73 = "AAAAAAAAAAAAAAAAAAA";

var s74 = "AAAAAAAAAAAAAAAAAAAA";

var t74 = "AAAAAAAAAAAAAAAAAAAA";

var s75 = "AAAAAAAAAAAAAAAAAAAAA";

var t75 = "AAAAAAAAAAAAAAAAAAAAA";

var s76 = "AAAAAAAAAAAAAAAAAAAAAA";

var t76 = "AAAAAAAAAAAAAAAAAAAAAA";

var s77 = "AAAAAAAAAAAAAAAAAAAAAAA";

var t77 = "AAAAAAAAAAAAAAAAAAAAAAA";

var s78 = "AAAAAAAAAAAAAAAAAAAAAAAA";

var t78 = "AAAAAAAAAAAAAAAAAAAAAAAA";

var s79 = "AAAAAAAAAAAAAAAAAAAAAAAAA";

var t79 = "AAAAAAAAAAAAAAAAAAAAAAAAA";

var s80 = "AAAAAAAAAAAAAAAAAAAAAAAAAA";

var t80 = "AAAAAAAAAAAAAAAAAAAAAAAAAA";

var s81 = "AAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t81 = "AAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s82 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t82 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s83 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t83 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s84 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t84 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s85 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t85 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s86 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t86 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s87 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t87 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s88 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var t88 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

var s89 = "BBBBBBBBBBBBBBB";

var t89 = "BBBBBBBBBBBBBBB";

var s90 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

var t90 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

var s91 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

var t91 = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";

function tst06(s) {
  switch (s) {
    case "A" : 
        return 55;
    case "AA" : 
        return 56;
    case "AAA" : 
        return 57;
    case "AAAA" : 
        return 58;
    case "AAAAA" : 
        return 59;
    case "AAAAAA" : 
        return 60;
    case "AAAAAAA" : 
        return 61;
    case "AAAAAAAA" : 
        return 62;
    case "AAAAAAAAA" : 
        return 63;
    case "AAAAAAAAAA" : 
        return 64;
    case "AAAAAAAAAAA" : 
        return 65;
    case "AAAAAAAAAAAA" : 
        return 66;
    case "AAAAAAAAAAAAA" : 
        return 67;
    case "AAAAAAAAAAAAAA" : 
        return 68;
    case "AAAAAAAAAAAAAAA" : 
        return 69;
    case "AAAAAAAAAAAAAAAA" : 
        return 70;
    case "AAAAAAAAAAAAAAAAA" : 
        return 71;
    case "AAAAAAAAAAAAAAAAAA" : 
        return 72;
    case "AAAAAAAAAAAAAAAAAAA" : 
        return 73;
    case "AAAAAAAAAAAAAAAAAAAA" : 
        return 74;
    case "AAAAAAAAAAAAAAAAAAAAA" : 
        return 75;
    case "AAAAAAAAAAAAAAAAAAAAAA" : 
        return 76;
    case "AAAAAAAAAAAAAAAAAAAAAAA" : 
        return 77;
    case "AAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 78;
    case "AAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 79;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 80;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 81;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 82;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 83;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 84;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 85;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 86;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 87;
    case "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" : 
        return 88;
    case "BBBBBBBBBBBBBBB" : 
        return 89;
    case "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" : 
        return 90;
    case "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" : 
        return 91;
    case "and" : 
        return 0;
    case "as" : 
        return 1;
    case "asr" : 
        return 54;
    case "assert" : 
        return 2;
    case "begin" : 
        return 3;
    case "class" : 
        return 4;
    case "constraint" : 
        return 5;
    case "do" : 
        return 6;
    case "done" : 
        return 7;
    case "downto" : 
        return 8;
    case "else" : 
        return 9;
    case "end" : 
        return 10;
    case "exception" : 
        return 11;
    case "external" : 
        return 12;
    case "false" : 
        return 13;
    case "for" : 
        return 14;
    case "fun" : 
        return 15;
    case "function" : 
        return 16;
    case "functor" : 
        return 17;
    case "if" : 
        return 18;
    case "in" : 
        return 19;
    case "include" : 
        return 20;
    case "inherit" : 
        return 21;
    case "initializer" : 
        return 22;
    case "land" : 
        return 49;
    case "lazy" : 
        return 23;
    case "let" : 
        return 24;
    case "lor" : 
        return 50;
    case "lsl" : 
        return 52;
    case "lsr" : 
        return 53;
    case "lxor" : 
        return 51;
    case "match" : 
        return 25;
    case "method" : 
        return 26;
    case "mod" : 
        return 48;
    case "module" : 
        return 27;
    case "mutable" : 
        return 28;
    case "new" : 
        return 29;
    case "object" : 
        return 30;
    case "of" : 
        return 31;
    case "open" : 
        return 32;
    case "or" : 
        return 33;
    case "private" : 
        return 34;
    case "rec" : 
        return 35;
    case "sig" : 
        return 36;
    case "struct" : 
        return 37;
    case "then" : 
        return 38;
    case "to" : 
        return 39;
    case "true" : 
        return 40;
    case "try" : 
        return 41;
    case "type" : 
        return 42;
    case "val" : 
        return 43;
    case "virtual" : 
        return 44;
    case "when" : 
        return 45;
    case "while" : 
        return 46;
    case "with" : 
        return 47;
    default:
      return -1;
  }
}

if (tst06(s00) !== 0) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          565,
          2
        ]
      ];
}

if (tst06(t00) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          566,
          2
        ]
      ];
}

if (tst06(s01) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          567,
          2
        ]
      ];
}

if (tst06(t01) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          568,
          2
        ]
      ];
}

if (tst06(s02) !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          569,
          2
        ]
      ];
}

if (tst06(t02) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          570,
          2
        ]
      ];
}

if (tst06(s03) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          571,
          2
        ]
      ];
}

if (tst06(t03) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          572,
          2
        ]
      ];
}

if (tst06(s04) !== 4) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          573,
          2
        ]
      ];
}

if (tst06(t04) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          574,
          2
        ]
      ];
}

if (tst06(s05) !== 5) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          575,
          2
        ]
      ];
}

if (tst06(t05) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          576,
          2
        ]
      ];
}

if (tst06(s06) !== 6) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          577,
          2
        ]
      ];
}

if (tst06(t06) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          578,
          2
        ]
      ];
}

if (tst06(s07) !== 7) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          579,
          2
        ]
      ];
}

if (tst06(t07) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          580,
          2
        ]
      ];
}

if (tst06(s08) !== 8) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          581,
          2
        ]
      ];
}

if (tst06(t08) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          582,
          2
        ]
      ];
}

if (tst06(s09) !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          583,
          2
        ]
      ];
}

if (tst06(t09) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          584,
          2
        ]
      ];
}

if (tst06(s10) !== 10) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          585,
          2
        ]
      ];
}

if (tst06(t10) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          586,
          2
        ]
      ];
}

if (tst06(s11) !== 11) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          587,
          2
        ]
      ];
}

if (tst06(t11) !== 11) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          588,
          2
        ]
      ];
}

if (tst06(s12) !== 12) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          589,
          2
        ]
      ];
}

if (tst06(t12) !== 12) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          590,
          2
        ]
      ];
}

if (tst06(s13) !== 13) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          591,
          2
        ]
      ];
}

if (tst06(t13) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          592,
          2
        ]
      ];
}

if (tst06(s14) !== 14) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          593,
          2
        ]
      ];
}

if (tst06(t14) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          594,
          2
        ]
      ];
}

if (tst06(s15) !== 15) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          595,
          2
        ]
      ];
}

if (tst06(t15) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          596,
          2
        ]
      ];
}

if (tst06(s16) !== 16) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          597,
          2
        ]
      ];
}

if (tst06(t16) !== 16) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          598,
          2
        ]
      ];
}

if (tst06(s17) !== 17) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          599,
          2
        ]
      ];
}

if (tst06(t17) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          600,
          2
        ]
      ];
}

if (tst06(s18) !== 18) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          601,
          2
        ]
      ];
}

if (tst06(t18) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          602,
          2
        ]
      ];
}

if (tst06(s19) !== 19) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          603,
          2
        ]
      ];
}

if (tst06(t19) !== 19) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          604,
          2
        ]
      ];
}

if (tst06(s20) !== 20) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          605,
          2
        ]
      ];
}

if (tst06(t20) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          606,
          2
        ]
      ];
}

if (tst06(s21) !== 21) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          607,
          2
        ]
      ];
}

if (tst06(t21) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          608,
          2
        ]
      ];
}

if (tst06(s22) !== 22) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          609,
          2
        ]
      ];
}

if (tst06(t22) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          610,
          2
        ]
      ];
}

if (tst06(s23) !== 23) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          611,
          2
        ]
      ];
}

if (tst06(t23) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          612,
          2
        ]
      ];
}

if (tst06(s24) !== 24) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          613,
          2
        ]
      ];
}

if (tst06(t24) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          614,
          2
        ]
      ];
}

if (tst06(s25) !== 25) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          615,
          2
        ]
      ];
}

if (tst06(t25) !== 25) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          616,
          2
        ]
      ];
}

if (tst06(s26) !== 26) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          617,
          2
        ]
      ];
}

if (tst06(t26) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          618,
          2
        ]
      ];
}

if (tst06(s27) !== 27) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          619,
          2
        ]
      ];
}

if (tst06(t27) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          620,
          2
        ]
      ];
}

if (tst06(s28) !== 28) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          621,
          2
        ]
      ];
}

if (tst06(t28) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          622,
          2
        ]
      ];
}

if (tst06(s29) !== 29) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          623,
          2
        ]
      ];
}

if (tst06(t29) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          624,
          2
        ]
      ];
}

if (tst06(s30) !== 30) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          625,
          2
        ]
      ];
}

if (tst06(t30) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          626,
          2
        ]
      ];
}

if (tst06(s31) !== 31) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          627,
          2
        ]
      ];
}

if (tst06(t31) !== 31) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          628,
          2
        ]
      ];
}

if (tst06(s32) !== 32) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          629,
          2
        ]
      ];
}

if (tst06(t32) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          630,
          2
        ]
      ];
}

if (tst06(s33) !== 33) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          631,
          2
        ]
      ];
}

if (tst06(t33) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          632,
          2
        ]
      ];
}

if (tst06(s34) !== 34) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          633,
          2
        ]
      ];
}

if (tst06(t34) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          634,
          2
        ]
      ];
}

if (tst06(s35) !== 35) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          635,
          2
        ]
      ];
}

if (tst06(t35) !== 35) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          636,
          2
        ]
      ];
}

if (tst06(s36) !== 36) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          637,
          2
        ]
      ];
}

if (tst06(t36) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          638,
          2
        ]
      ];
}

if (tst06(s37) !== 37) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          639,
          2
        ]
      ];
}

if (tst06(t37) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          640,
          2
        ]
      ];
}

if (tst06(s38) !== 38) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          641,
          2
        ]
      ];
}

if (tst06(t38) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          642,
          2
        ]
      ];
}

if (tst06(s39) !== 39) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          643,
          2
        ]
      ];
}

if (tst06(t39) !== 39) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          644,
          2
        ]
      ];
}

if (tst06(s40) !== 40) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          645,
          2
        ]
      ];
}

if (tst06(t40) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          646,
          2
        ]
      ];
}

if (tst06(s41) !== 41) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          647,
          2
        ]
      ];
}

if (tst06(t41) !== 41) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          648,
          2
        ]
      ];
}

if (tst06(s42) !== 42) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          649,
          2
        ]
      ];
}

if (tst06(t42) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          650,
          2
        ]
      ];
}

if (tst06(s43) !== 43) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          651,
          2
        ]
      ];
}

if (tst06(t43) !== 43) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          652,
          2
        ]
      ];
}

if (tst06(s44) !== 44) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          653,
          2
        ]
      ];
}

if (tst06(t44) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          654,
          2
        ]
      ];
}

if (tst06(s45) !== 45) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          655,
          2
        ]
      ];
}

if (tst06(t45) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          656,
          2
        ]
      ];
}

if (tst06(s46) !== 46) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          657,
          2
        ]
      ];
}

if (tst06(t46) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          658,
          2
        ]
      ];
}

if (tst06(s47) !== 47) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          659,
          2
        ]
      ];
}

if (tst06(t47) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          660,
          2
        ]
      ];
}

if (tst06(s48) !== 48) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          661,
          2
        ]
      ];
}

if (tst06(t48) !== 48) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          662,
          2
        ]
      ];
}

if (tst06(s49) !== 49) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          663,
          2
        ]
      ];
}

if (tst06(t49) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          664,
          2
        ]
      ];
}

if (tst06(s50) !== 50) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          665,
          2
        ]
      ];
}

if (tst06(t50) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          666,
          2
        ]
      ];
}

if (tst06(s51) !== 51) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          667,
          2
        ]
      ];
}

if (tst06(t51) !== 51) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          668,
          2
        ]
      ];
}

if (tst06(s52) !== 52) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          669,
          2
        ]
      ];
}

if (tst06(t52) !== 52) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          670,
          2
        ]
      ];
}

if (tst06(s53) !== 53) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          671,
          2
        ]
      ];
}

if (tst06(t53) !== 53) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          672,
          2
        ]
      ];
}

if (tst06(s54) !== 54) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          673,
          2
        ]
      ];
}

if (tst06(t54) !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          674,
          2
        ]
      ];
}

if (tst06(s55) !== 55) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          675,
          2
        ]
      ];
}

if (tst06(t55) !== 55) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          676,
          2
        ]
      ];
}

if (tst06(s56) !== 56) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          677,
          2
        ]
      ];
}

if (tst06(t56) !== 56) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          678,
          2
        ]
      ];
}

if (tst06(s57) !== 57) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          679,
          2
        ]
      ];
}

if (tst06(t57) !== 57) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          680,
          2
        ]
      ];
}

if (tst06(s58) !== 58) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          681,
          2
        ]
      ];
}

if (tst06(t58) !== 58) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          682,
          2
        ]
      ];
}

if (tst06(s59) !== 59) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          683,
          2
        ]
      ];
}

if (tst06(t59) !== 59) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          684,
          2
        ]
      ];
}

if (tst06(s60) !== 60) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          685,
          2
        ]
      ];
}

if (tst06(t60) !== 60) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          686,
          2
        ]
      ];
}

if (tst06(s61) !== 61) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          687,
          2
        ]
      ];
}

if (tst06(t61) !== 61) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          688,
          2
        ]
      ];
}

if (tst06(s62) !== 62) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          689,
          2
        ]
      ];
}

if (tst06(t62) !== 62) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          690,
          2
        ]
      ];
}

if (tst06(s63) !== 63) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          691,
          2
        ]
      ];
}

if (tst06(t63) !== 63) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          692,
          2
        ]
      ];
}

if (tst06(s64) !== 64) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          693,
          2
        ]
      ];
}

if (tst06(t64) !== 64) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          694,
          2
        ]
      ];
}

if (tst06(s65) !== 65) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          695,
          2
        ]
      ];
}

if (tst06(t65) !== 65) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          696,
          2
        ]
      ];
}

if (tst06(s66) !== 66) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          697,
          2
        ]
      ];
}

if (tst06(t66) !== 66) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          698,
          2
        ]
      ];
}

if (tst06(s67) !== 67) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          699,
          2
        ]
      ];
}

if (tst06(t67) !== 67) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          700,
          2
        ]
      ];
}

if (tst06(s68) !== 68) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          701,
          2
        ]
      ];
}

if (tst06(t68) !== 68) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          702,
          2
        ]
      ];
}

if (tst06(s69) !== 69) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          703,
          2
        ]
      ];
}

if (tst06(t69) !== 69) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          704,
          2
        ]
      ];
}

if (tst06(s70) !== 70) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          705,
          2
        ]
      ];
}

if (tst06(t70) !== 70) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          706,
          2
        ]
      ];
}

if (tst06(s71) !== 71) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          707,
          2
        ]
      ];
}

if (tst06(t71) !== 71) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          708,
          2
        ]
      ];
}

if (tst06(s72) !== 72) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          709,
          2
        ]
      ];
}

if (tst06(t72) !== 72) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          710,
          2
        ]
      ];
}

if (tst06(s73) !== 73) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          711,
          2
        ]
      ];
}

if (tst06(t73) !== 73) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          712,
          2
        ]
      ];
}

if (tst06(s74) !== 74) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          713,
          2
        ]
      ];
}

if (tst06(t74) !== 74) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          714,
          2
        ]
      ];
}

if (tst06(s75) !== 75) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          715,
          2
        ]
      ];
}

if (tst06(t75) !== 75) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          716,
          2
        ]
      ];
}

if (tst06(s76) !== 76) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          717,
          2
        ]
      ];
}

if (tst06(t76) !== 76) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          718,
          2
        ]
      ];
}

if (tst06(s77) !== 77) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          719,
          2
        ]
      ];
}

if (tst06(t77) !== 77) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          720,
          2
        ]
      ];
}

if (tst06(s78) !== 78) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          721,
          2
        ]
      ];
}

if (tst06(t78) !== 78) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          722,
          2
        ]
      ];
}

if (tst06(s79) !== 79) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          723,
          2
        ]
      ];
}

if (tst06(t79) !== 79) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          724,
          2
        ]
      ];
}

if (tst06(s80) !== 80) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          725,
          2
        ]
      ];
}

if (tst06(t80) !== 80) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          726,
          2
        ]
      ];
}

if (tst06(s81) !== 81) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          727,
          2
        ]
      ];
}

if (tst06(t81) !== 81) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          728,
          2
        ]
      ];
}

if (tst06(s82) !== 82) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          729,
          2
        ]
      ];
}

if (tst06(t82) !== 82) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          730,
          2
        ]
      ];
}

if (tst06(s83) !== 83) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          731,
          2
        ]
      ];
}

if (tst06(t83) !== 83) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          732,
          2
        ]
      ];
}

if (tst06(s84) !== 84) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          733,
          2
        ]
      ];
}

if (tst06(t84) !== 84) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          734,
          2
        ]
      ];
}

if (tst06(s85) !== 85) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          735,
          2
        ]
      ];
}

if (tst06(t85) !== 85) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          736,
          2
        ]
      ];
}

if (tst06(s86) !== 86) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          737,
          2
        ]
      ];
}

if (tst06(t86) !== 86) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          738,
          2
        ]
      ];
}

if (tst06(s87) !== 87) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          739,
          2
        ]
      ];
}

if (tst06(t87) !== 87) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          740,
          2
        ]
      ];
}

if (tst06(s88) !== 88) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          741,
          2
        ]
      ];
}

if (tst06(t88) !== 88) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          742,
          2
        ]
      ];
}

if (tst06(s89) !== 89) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          743,
          2
        ]
      ];
}

if (tst06(t89) !== 89) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          744,
          2
        ]
      ];
}

if (tst06(s90) !== 90) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          745,
          2
        ]
      ];
}

if (tst06(t90) !== 90) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          746,
          2
        ]
      ];
}

if (tst06(s91) !== 91) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          747,
          2
        ]
      ];
}

if (tst06(t91) !== 91) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          748,
          2
        ]
      ];
}

if (tst06("") !== -1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "stringmatch_test.ml",
          749,
          2
        ]
      ];
}

exports.tst01 = tst01;
exports.tst02 = tst02;
exports.tst03 = tst03;
exports.tst04 = tst04;
exports.tst05 = tst05;
exports.s00 = s00;
exports.t00 = t00;
exports.s01 = s01;
exports.t01 = t01;
exports.s02 = s02;
exports.t02 = t02;
exports.s03 = s03;
exports.t03 = t03;
exports.s04 = s04;
exports.t04 = t04;
exports.s05 = s05;
exports.t05 = t05;
exports.s06 = s06;
exports.t06 = t06;
exports.s07 = s07;
exports.t07 = t07;
exports.s08 = s08;
exports.t08 = t08;
exports.s09 = s09;
exports.t09 = t09;
exports.s10 = s10;
exports.t10 = t10;
exports.s11 = s11;
exports.t11 = t11;
exports.s12 = s12;
exports.t12 = t12;
exports.s13 = s13;
exports.t13 = t13;
exports.s14 = s14;
exports.t14 = t14;
exports.s15 = s15;
exports.t15 = t15;
exports.s16 = s16;
exports.t16 = t16;
exports.s17 = s17;
exports.t17 = t17;
exports.s18 = s18;
exports.t18 = t18;
exports.s19 = s19;
exports.t19 = t19;
exports.s20 = s20;
exports.t20 = t20;
exports.s21 = s21;
exports.t21 = t21;
exports.s22 = s22;
exports.t22 = t22;
exports.s23 = s23;
exports.t23 = t23;
exports.s24 = s24;
exports.t24 = t24;
exports.s25 = s25;
exports.t25 = t25;
exports.s26 = s26;
exports.t26 = t26;
exports.s27 = s27;
exports.t27 = t27;
exports.s28 = s28;
exports.t28 = t28;
exports.s29 = s29;
exports.t29 = t29;
exports.s30 = s30;
exports.t30 = t30;
exports.s31 = s31;
exports.t31 = t31;
exports.s32 = s32;
exports.t32 = t32;
exports.s33 = s33;
exports.t33 = t33;
exports.s34 = s34;
exports.t34 = t34;
exports.s35 = s35;
exports.t35 = t35;
exports.s36 = s36;
exports.t36 = t36;
exports.s37 = s37;
exports.t37 = t37;
exports.s38 = s38;
exports.t38 = t38;
exports.s39 = s39;
exports.t39 = t39;
exports.s40 = s40;
exports.t40 = t40;
exports.s41 = s41;
exports.t41 = t41;
exports.s42 = s42;
exports.t42 = t42;
exports.s43 = s43;
exports.t43 = t43;
exports.s44 = s44;
exports.t44 = t44;
exports.s45 = s45;
exports.t45 = t45;
exports.s46 = s46;
exports.t46 = t46;
exports.s47 = s47;
exports.t47 = t47;
exports.s48 = s48;
exports.t48 = t48;
exports.s49 = s49;
exports.t49 = t49;
exports.s50 = s50;
exports.t50 = t50;
exports.s51 = s51;
exports.t51 = t51;
exports.s52 = s52;
exports.t52 = t52;
exports.s53 = s53;
exports.t53 = t53;
exports.s54 = s54;
exports.t54 = t54;
exports.s55 = s55;
exports.t55 = t55;
exports.s56 = s56;
exports.t56 = t56;
exports.s57 = s57;
exports.t57 = t57;
exports.s58 = s58;
exports.t58 = t58;
exports.s59 = s59;
exports.t59 = t59;
exports.s60 = s60;
exports.t60 = t60;
exports.s61 = s61;
exports.t61 = t61;
exports.s62 = s62;
exports.t62 = t62;
exports.s63 = s63;
exports.t63 = t63;
exports.s64 = s64;
exports.t64 = t64;
exports.s65 = s65;
exports.t65 = t65;
exports.s66 = s66;
exports.t66 = t66;
exports.s67 = s67;
exports.t67 = t67;
exports.s68 = s68;
exports.t68 = t68;
exports.s69 = s69;
exports.t69 = t69;
exports.s70 = s70;
exports.t70 = t70;
exports.s71 = s71;
exports.t71 = t71;
exports.s72 = s72;
exports.t72 = t72;
exports.s73 = s73;
exports.t73 = t73;
exports.s74 = s74;
exports.t74 = t74;
exports.s75 = s75;
exports.t75 = t75;
exports.s76 = s76;
exports.t76 = t76;
exports.s77 = s77;
exports.t77 = t77;
exports.s78 = s78;
exports.t78 = t78;
exports.s79 = s79;
exports.t79 = t79;
exports.s80 = s80;
exports.t80 = t80;
exports.s81 = s81;
exports.t81 = t81;
exports.s82 = s82;
exports.t82 = t82;
exports.s83 = s83;
exports.t83 = t83;
exports.s84 = s84;
exports.t84 = t84;
exports.s85 = s85;
exports.t85 = t85;
exports.s86 = s86;
exports.t86 = t86;
exports.s87 = s87;
exports.t87 = t87;
exports.s88 = s88;
exports.t88 = t88;
exports.s89 = s89;
exports.t89 = t89;
exports.s90 = s90;
exports.t90 = t90;
exports.s91 = s91;
exports.t91 = t91;
exports.tst06 = tst06;
/*  Not a pure module */
