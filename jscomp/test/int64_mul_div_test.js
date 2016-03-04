// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64 = require("../runtime/caml_int64");
var Caml_obj   = require("../runtime/caml_obj");
var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");

function commutative_mul(result, a, b) {
  return /* Eq */{
          0: /* tuple */[
            result,
            result
          ],
          1: /* tuple */[
            Caml_int64.mul(a, b),
            Caml_int64.mul(b, a)
          ],
          length: 2,
          tag: 0
        };
}

var pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          -1753181728,
          -1482940033
        ],
        /* int64 */[
          525340320,
          1831202545
        ],
        /* int64 */[
          634982515,
          165328154
        ]
      ],
      /* tuple */[
        /* int64 */[
          -45257752,
          2086642202
        ],
        /* int64 */[
          -1243123636,
          1756378018
        ],
        /* int64 */[
          -162347938,
          1129387921
        ]
      ],
      /* tuple */[
        /* int64 */[
          1358520104,
          -2133087767
        ],
        /* int64 */[
          -814703364,
          209351581
        ],
        /* int64 */[
          -1595781194,
          2047885301
        ]
      ],
      /* tuple */[
        /* int64 */[
          -561804740,
          781938191
        ],
        /* int64 */[
          -664434255,
          1243995318
        ],
        /* int64 */[
          -1752170244,
          965315102
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1173434256,
          -1526298485
        ],
        /* int64 */[
          -241239064,
          806837349
        ],
        /* int64 */[
          -1873028186,
          973509509
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1817540358,
          -1053250751
        ],
        /* int64 */[
          -827251842,
          297218479
        ],
        /* int64 */[
          1639039171,
          1353404045
        ]
      ],
      /* tuple */[
        /* int64 */[
          1100802137,
          -1614556777
        ],
        /* int64 */[
          732603925,
          1812737275
        ],
        /* int64 */[
          1587408437,
          1686007471
        ]
      ],
      /* tuple */[
        /* int64 */[
          -955806000,
          166521361
        ],
        /* int64 */[
          -1809921861,
          585926665
        ],
        /* int64 */[
          -1411520656,
          1193284387
        ]
      ],
      /* tuple */[
        /* int64 */[
          -2095936707,
          -1556851713
        ],
        /* int64 */[
          -977007627,
          563693579
        ],
        /* int64 */[
          700349737,
          1423006973
        ]
      ],
      /* tuple */[
        /* int64 */[
          -2082262446,
          2096650716
        ],
        /* int64 */[
          -275710143,
          1413202597
        ],
        /* int64 */[
          -843247662,
          495794945
        ]
      ],
      /* tuple */[
        /* int64 */[
          1313884544,
          1149398987
        ],
        /* int64 */[
          255480485,
          556147957
        ],
        /* int64 */[
          929852288,
          1711350082
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1787508521,
          908394614
        ],
        /* int64 */[
          725956947,
          1053216964
        ],
        /* int64 */[
          -310080019,
          2145390454
        ]
      ],
      /* tuple */[
        /* int64 */[
          1441406688,
          962684198
        ],
        /* int64 */[
          468792198,
          1528894622
        ],
        /* int64 */[
          -375108656,
          1893431833
        ]
      ],
      /* tuple */[
        /* int64 */[
          -966612576,
          -603570361
        ],
        /* int64 */[
          1999781536,
          1842921977
        ],
        /* int64 */[
          -1039472903,
          1854314037
        ]
      ],
      /* tuple */[
        /* int64 */[
          1187405920,
          1827458543
        ],
        /* int64 */[
          -1788999968,
          1066436782
        ],
        /* int64 */[
          2020291989,
          1004254249
        ]
      ],
      /* tuple */[
        /* int64 */[
          1878451246,
          -1818789533
        ],
        /* int64 */[
          1999625579,
          247021097
        ],
        /* int64 */[
          -1924515318,
          1434621550
        ]
      ],
      /* tuple */[
        /* int64 */[
          2103538455,
          1714915951
        ],
        /* int64 */[
          45872671,
          1071186049
        ],
        /* int64 */[
          -1656179703,
          911777108
        ]
      ],
      /* tuple */[
        /* int64 */[
          -344232772,
          365880810
        ],
        /* int64 */[
          1477626470,
          1215123423
        ],
        /* int64 */[
          1816687658,
          1155052099
        ]
      ],
      /* tuple */[
        /* int64 */[
          1713682280,
          -1590309406
        ],
        /* int64 */[
          -1964710772,
          1236324221
        ],
        /* int64 */[
          1538765150,
          871497139
        ]
      ],
      /* tuple */[
        /* int64 */[
          1074580892,
          -1335640207
        ],
        /* int64 */[
          -1999389012,
          153491040
        ],
        /* int64 */[
          1001897781,
          469100620
        ]
      ]
    ]);

function from_pairs(prefix, pairs) {
  return $$Array.to_list($$Array.mapi(function (i, param) {
                  var b = param[2];
                  var a = param[1];
                  var result = param[0];
                  return /* tuple */[
                          Caml_curry.app2(Printf.sprintf(/* Format */{
                                    0: /* String */{
                                      0: /* No_padding */0,
                                      1: /* Char_literal */{
                                        0: /* "_" */95,
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
                                      tag: 2
                                    },
                                    1: "%s_%d",
                                    length: 2,
                                    tag: 0
                                  }), prefix, i),
                          function () {
                            return commutative_mul(result, a, b);
                          }
                        ];
                }, pairs));
}

var small_pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          121,
          0
        ],
        /* int64 */[
          11,
          0
        ],
        /* int64 */[
          11,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          126736,
          0
        ],
        /* int64 */[
          356,
          0
        ],
        /* int64 */[
          356,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          137176406,
          0
        ],
        /* int64 */[
          12346,
          0
        ],
        /* int64 */[
          11111,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1048576,
          268435455
        ],
        /* int64 */[
          -1,
          255
        ],
        /* int64 */[
          1048576,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          1048576,
          -268435456
        ],
        /* int64 */[
          1,
          -256
        ],
        /* int64 */[
          1048576,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          1275262484,
          -639559543
        ],
        /* int64 */[
          -1147274043,
          1209011959
        ],
        /* int64 */[
          242558724,
          1831626934
        ]
      ]
    ]);

Mt.from_pair_suites("int64_mul_div_test.ml", Pervasives.$at(from_pairs("random", pairs), from_pairs("small", small_pairs)));

exports.commutative_mul = commutative_mul;
exports.pairs           = pairs;
exports.from_pairs      = from_pairs;
exports.small_pairs     = small_pairs;
/*  Not a pure module */
