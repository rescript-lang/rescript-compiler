'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var Printf = require("../../lib/js/printf.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

function commutative_mul(result, a, b) {
  return /* Eq */Block.__(0, [
            /* tuple */[
              result,
              result
            ],
            /* tuple */[
              Caml_int64.mul(a, b),
              Caml_int64.mul(b, a)
            ]
          ]);
}

var pairs = /* array */[
  /* tuple */[
    /* int64 */{
      hi: -1482940033,
      lo: 2541785568
    },
    /* int64 */{
      hi: 1831202545,
      lo: 525340320
    },
    /* int64 */{
      hi: 165328154,
      lo: 634982515
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 2086642202,
      lo: 4249709544
    },
    /* int64 */{
      hi: 1756378018,
      lo: 3051843660
    },
    /* int64 */{
      hi: 1129387921,
      lo: 4132619358
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -2133087767,
      lo: 1358520104
    },
    /* int64 */{
      hi: 209351581,
      lo: 3480263932
    },
    /* int64 */{
      hi: 2047885301,
      lo: 2699186102
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 781938191,
      lo: 3733162556
    },
    /* int64 */{
      hi: 1243995318,
      lo: 3630533041
    },
    /* int64 */{
      hi: 965315102,
      lo: 2542797052
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1526298485,
      lo: 3121533040
    },
    /* int64 */{
      hi: 806837349,
      lo: 4053728232
    },
    /* int64 */{
      hi: 973509509,
      lo: 2421939110
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1053250751,
      lo: 2477426938
    },
    /* int64 */{
      hi: 297218479,
      lo: 3467715454
    },
    /* int64 */{
      hi: 1353404045,
      lo: 1639039171
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1614556777,
      lo: 1100802137
    },
    /* int64 */{
      hi: 1812737275,
      lo: 732603925
    },
    /* int64 */{
      hi: 1686007471,
      lo: 1587408437
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 166521361,
      lo: 3339161296
    },
    /* int64 */{
      hi: 585926665,
      lo: 2485045435
    },
    /* int64 */{
      hi: 1193284387,
      lo: 2883446640
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1556851713,
      lo: 2199030589
    },
    /* int64 */{
      hi: 563693579,
      lo: 3317959669
    },
    /* int64 */{
      hi: 1423006973,
      lo: 700349737
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 2096650716,
      lo: 2212704850
    },
    /* int64 */{
      hi: 1413202597,
      lo: 4019257153
    },
    /* int64 */{
      hi: 495794945,
      lo: 3451719634
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1149398987,
      lo: 1313884544
    },
    /* int64 */{
      hi: 556147957,
      lo: 255480485
    },
    /* int64 */{
      hi: 1711350082,
      lo: 929852288
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 908394614,
      lo: 2507458775
    },
    /* int64 */{
      hi: 1053216964,
      lo: 725956947
    },
    /* int64 */{
      hi: 2145390454,
      lo: 3984887277
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 962684198,
      lo: 1441406688
    },
    /* int64 */{
      hi: 1528894622,
      lo: 468792198
    },
    /* int64 */{
      hi: 1893431833,
      lo: 3919858640
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -603570361,
      lo: 3328354720
    },
    /* int64 */{
      hi: 1842921977,
      lo: 1999781536
    },
    /* int64 */{
      hi: 1854314037,
      lo: 3255494393
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1827458543,
      lo: 1187405920
    },
    /* int64 */{
      hi: 1066436782,
      lo: 2505967328
    },
    /* int64 */{
      hi: 1004254249,
      lo: 2020291989
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1818789533,
      lo: 1878451246
    },
    /* int64 */{
      hi: 247021097,
      lo: 1999625579
    },
    /* int64 */{
      hi: 1434621550,
      lo: 2370451978
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1714915951,
      lo: 2103538455
    },
    /* int64 */{
      hi: 1071186049,
      lo: 45872671
    },
    /* int64 */{
      hi: 911777108,
      lo: 2638787593
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 365880810,
      lo: 3950734524
    },
    /* int64 */{
      hi: 1215123423,
      lo: 1477626470
    },
    /* int64 */{
      hi: 1155052099,
      lo: 1816687658
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1590309406,
      lo: 1713682280
    },
    /* int64 */{
      hi: 1236324221,
      lo: 2330256524
    },
    /* int64 */{
      hi: 871497139,
      lo: 1538765150
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -1335640207,
      lo: 1074580892
    },
    /* int64 */{
      hi: 153491040,
      lo: 2295578284
    },
    /* int64 */{
      hi: 469100620,
      lo: 1001897781
    }
  ]
];

function from_pairs(prefix, pairs) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[2];
                    var a = param[1];
                    var result = param[0];
                    return /* tuple */[
                            Curry._2(Printf.sprintf(/* Format */[
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* Char_literal */Block.__(12, [
                                              /* "_" */95,
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ]),
                                      "%s_%d"
                                    ]), prefix, i),
                            (function (param) {
                                return commutative_mul(result, a, b);
                              })
                          ];
                  }), pairs));
}

var small_pairs = /* array */[
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 121
    },
    /* int64 */{
      hi: 0,
      lo: 11
    },
    /* int64 */{
      hi: 0,
      lo: 11
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 126736
    },
    /* int64 */{
      hi: 0,
      lo: 356
    },
    /* int64 */{
      hi: 0,
      lo: 356
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 137176406
    },
    /* int64 */{
      hi: 0,
      lo: 12346
    },
    /* int64 */{
      hi: 0,
      lo: 11111
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 268435455,
      lo: 4293918720
    },
    /* int64 */{
      hi: 255,
      lo: 4294967295
    },
    /* int64 */{
      hi: 0,
      lo: 1048576
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -268435456,
      lo: 1048576
    },
    /* int64 */{
      hi: -256,
      lo: 1
    },
    /* int64 */{
      hi: 0,
      lo: 1048576
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -639559543,
      lo: 1275262484
    },
    /* int64 */{
      hi: 1209011959,
      lo: 3147693253
    },
    /* int64 */{
      hi: 1831626934,
      lo: 242558724
    }
  ]
];

var to_floats = /* array */[
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 1
    },
    1
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2
    },
    2
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 4
    },
    4
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 8
    },
    8
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 16
    },
    16
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 32
    },
    32
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 64
    },
    64
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 128
    },
    128
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 256
    },
    256
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 512
    },
    512
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 1024
    },
    1024
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2048
    },
    2048
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 4096
    },
    4096
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 8192
    },
    8192
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 16384
    },
    16384
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 32768
    },
    32768
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 65536
    },
    65536
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 131072
    },
    131072
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 262144
    },
    262144
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 524288
    },
    524288
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 1048576
    },
    1048576
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2097152
    },
    2097152
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 4194304
    },
    4194304
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 8388608
    },
    8388608
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 16777216
    },
    16777216
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 33554432
    },
    33554432
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 67108864
    },
    67108864
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 134217728
    },
    134217728
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 268435456
    },
    268435456
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 536870912
    },
    536870912
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 1073741824
    },
    1073741824
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2147483648
    },
    2147483648
  ],
  /* tuple */[
    /* int64 */{
      hi: 1,
      lo: 0
    },
    4294967296
  ],
  /* tuple */[
    /* int64 */{
      hi: 2,
      lo: 0
    },
    8589934592
  ],
  /* tuple */[
    /* int64 */{
      hi: 4,
      lo: 0
    },
    17179869184
  ],
  /* tuple */[
    /* int64 */{
      hi: 8,
      lo: 0
    },
    34359738368
  ],
  /* tuple */[
    /* int64 */{
      hi: 16,
      lo: 0
    },
    68719476736
  ],
  /* tuple */[
    /* int64 */{
      hi: 32,
      lo: 0
    },
    137438953472
  ],
  /* tuple */[
    /* int64 */{
      hi: 64,
      lo: 0
    },
    274877906944
  ],
  /* tuple */[
    /* int64 */{
      hi: 128,
      lo: 0
    },
    549755813888
  ],
  /* tuple */[
    /* int64 */{
      hi: 256,
      lo: 0
    },
    1099511627776
  ],
  /* tuple */[
    /* int64 */{
      hi: 512,
      lo: 0
    },
    2199023255552
  ],
  /* tuple */[
    /* int64 */{
      hi: 1024,
      lo: 0
    },
    4398046511104
  ],
  /* tuple */[
    /* int64 */{
      hi: 2048,
      lo: 0
    },
    8796093022208
  ],
  /* tuple */[
    /* int64 */{
      hi: 4096,
      lo: 0
    },
    17592186044416
  ],
  /* tuple */[
    /* int64 */{
      hi: 8192,
      lo: 0
    },
    35184372088832
  ],
  /* tuple */[
    /* int64 */{
      hi: 16384,
      lo: 0
    },
    70368744177664
  ],
  /* tuple */[
    /* int64 */{
      hi: 32768,
      lo: 0
    },
    140737488355328
  ],
  /* tuple */[
    /* int64 */{
      hi: 65536,
      lo: 0
    },
    281474976710656
  ],
  /* tuple */[
    /* int64 */{
      hi: 131072,
      lo: 0
    },
    562949953421312
  ],
  /* tuple */[
    /* int64 */{
      hi: 262144,
      lo: 0
    },
    1125899906842624
  ],
  /* tuple */[
    /* int64 */{
      hi: 524288,
      lo: 0
    },
    2251799813685248
  ],
  /* tuple */[
    /* int64 */{
      hi: 1048576,
      lo: 0
    },
    4503599627370496
  ],
  /* tuple */[
    /* int64 */{
      hi: 2097152,
      lo: 0
    },
    9007199254740992
  ],
  /* tuple */[
    /* int64 */{
      hi: 4194304,
      lo: 0
    },
    18014398509481984
  ],
  /* tuple */[
    /* int64 */{
      hi: 8388608,
      lo: 0
    },
    36028797018963968
  ],
  /* tuple */[
    /* int64 */{
      hi: 16777216,
      lo: 0
    },
    72057594037927936
  ],
  /* tuple */[
    /* int64 */{
      hi: 33554432,
      lo: 0
    },
    144115188075855872
  ],
  /* tuple */[
    /* int64 */{
      hi: 67108864,
      lo: 0
    },
    288230376151711744
  ],
  /* tuple */[
    /* int64 */{
      hi: 134217728,
      lo: 0
    },
    576460752303423488
  ],
  /* tuple */[
    /* int64 */{
      hi: 268435456,
      lo: 0
    },
    1.15292150460684698e+18
  ],
  /* tuple */[
    /* int64 */{
      hi: 536870912,
      lo: 0
    },
    2.30584300921369395e+18
  ],
  /* tuple */[
    /* int64 */{
      hi: 1073741824,
      lo: 0
    },
    4.6116860184273879e+18
  ],
  /* tuple */[
    /* int64 */{
      hi: -2147483648,
      lo: 0
    },
    -9.22337203685477581e+18
  ]
];

var check_complete_compare = /* array */[
  true,
  true,
  true,
  true,
  true,
  true,
  true,
  true
];

var of_float_pairs = /* array */[
  /* tuple */[
    6853066956871844,
    /* int64 */{
      hi: 1595603,
      lo: 4254472356
    }
  ],
  /* tuple */[
    -8507688874782117,
    /* int64 */{
      hi: -1980851,
      lo: 1388466779
    }
  ],
  /* tuple */[
    4083117349607451,
    /* int64 */{
      hi: 950674,
      lo: 3610449947
    }
  ],
  /* tuple */[
    -4860723193745655,
    /* int64 */{
      hi: -1131726,
      lo: 2964287241
    }
  ],
  /* tuple */[
    7820020192255542,
    /* int64 */{
      hi: 1820740,
      lo: 1437736502
    }
  ],
  /* tuple */[
    -4908619721514532,
    /* int64 */{
      hi: -1142878,
      lo: 3911803356
    }
  ],
  /* tuple */[
    5.67685864687671e+15,
    /* int64 */{
      hi: 1321746,
      lo: 2803257894
    }
  ],
  /* tuple */[
    -703696191048023,
    /* int64 */{
      hi: -163843,
      lo: 4135630505
    }
  ],
  /* tuple */[
    1123586534990153.88,
    /* int64 */{
      hi: 261605,
      lo: 1615520073
    }
  ],
  /* tuple */[
    -4.29886533981922e+15,
    /* int64 */{
      hi: -1000908,
      lo: 1786485548
    }
  ],
  /* tuple */[
    2.43885138012066e+15,
    /* int64 */{
      hi: 567839,
      lo: 1445727316
    }
  ],
  /* tuple */[
    -8011538689499494,
    /* int64 */{
      hi: -1865332,
      lo: 1246682778
    }
  ],
  /* tuple */[
    2710072285421155,
    /* int64 */{
      hi: 630987,
      lo: 3756220003
    }
  ],
  /* tuple */[
    -2541457347159789.5,
    /* int64 */{
      hi: -591730,
      lo: 3650902291
    }
  ],
  /* tuple */[
    5012932793576708,
    /* int64 */{
      hi: 1167164,
      lo: 1584508164
    }
  ],
  /* tuple */[
    -943066847413899.125,
    /* int64 */{
      hi: -219575,
      lo: 596605301
    }
  ],
  /* tuple */[
    5440257518642004,
    /* int64 */{
      hi: 1266658,
      lo: 2833425236
    }
  ],
  /* tuple */[
    -7750676773453898,
    /* int64 */{
      hi: -1804596,
      lo: 4029038518
    }
  ],
  /* tuple */[
    8911999221747713,
    /* int64 */{
      hi: 2074986,
      lo: 2212089857
    }
  ],
  /* tuple */[
    -1443906702582204.25,
    /* int64 */{
      hi: -336186,
      lo: 1172790852
    }
  ],
  /* tuple */[
    659345820712164.875,
    /* int64 */{
      hi: 153515,
      lo: 3916266724
    }
  ],
  /* tuple */[
    -3284023713149006.5,
    /* int64 */{
      hi: -764622,
      lo: 2770653106
    }
  ],
  /* tuple */[
    5062818438579988,
    /* int64 */{
      hi: 1178779,
      lo: 1184368404
    }
  ],
  /* tuple */[
    -8904450004162331,
    /* int64 */{
      hi: -2073229,
      lo: 747956453
    }
  ],
  /* tuple */[
    848261089308786,
    /* int64 */{
      hi: 197501,
      lo: 753381490
    }
  ],
  /* tuple */[
    -6376579516657391,
    /* int64 */{
      hi: -1484664,
      lo: 3808891153
    }
  ],
  /* tuple */[
    1337907592605664.25,
    /* int64 */{
      hi: 311505,
      lo: 3805065184
    }
  ],
  /* tuple */[
    -8.54733738833896e+15,
    /* int64 */{
      hi: -1990083,
      lo: 4012986608
    }
  ],
  /* tuple */[
    2345417644172927,
    /* int64 */{
      hi: 546085,
      lo: 428336767
    }
  ],
  /* tuple */[
    -2587460670129294.5,
    /* int64 */{
      hi: -602441,
      lo: 3722640242
    }
  ],
  /* tuple */[
    4580431718597436,
    /* int64 */{
      hi: 1066464,
      lo: 3716236092
    }
  ],
  /* tuple */[
    -1460576044874256.25,
    /* int64 */{
      hi: -340067,
      lo: 598574576
    }
  ],
  /* tuple */[
    3403657978343579.5,
    /* int64 */{
      hi: 792475,
      lo: 3770445979
    }
  ],
  /* tuple */[
    -7.89068917321888e+15,
    /* int64 */{
      hi: -1837195,
      lo: 3268155840
    }
  ],
  /* tuple */[
    1683098350604788.5,
    /* int64 */{
      hi: 391876,
      lo: 3746517492
    }
  ],
  /* tuple */[
    -3966538891560174.5,
    /* int64 */{
      hi: -923532,
      lo: 845249298
    }
  ],
  /* tuple */[
    6726025288963652,
    /* int64 */{
      hi: 1566024,
      lo: 3424212548
    }
  ],
  /* tuple */[
    -4790410747298403,
    /* int64 */{
      hi: -1115355,
      lo: 2501131677
    }
  ],
  /* tuple */[
    1985858071337706.25,
    /* int64 */{
      hi: 462368,
      lo: 2632620778
    }
  ],
  /* tuple */[
    -5281733497873409,
    /* int64 */{
      hi: -1229750,
      lo: 2534382591
    }
  ]
];

var simple_divs = /* array */[
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 6
    },
    /* int64 */{
      hi: 0,
      lo: 3
    },
    /* int64 */{
      hi: 0,
      lo: 2
    },
    /* int64 */{
      hi: 0,
      lo: 0
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 120
    },
    /* int64 */{
      hi: 0,
      lo: 11
    },
    /* int64 */{
      hi: 0,
      lo: 10
    },
    /* int64 */{
      hi: 0,
      lo: 10
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: -2147483648,
      lo: 0
    },
    /* int64 */{
      hi: 0,
      lo: 2
    },
    /* int64 */{
      hi: -1073741824,
      lo: 0
    },
    /* int64 */{
      hi: 0,
      lo: 0
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1112580415,
      lo: 4131866785
    },
    /* int64 */{
      hi: 2013350321,
      lo: 2605406679
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 1112580415,
      lo: 4131866785
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 983582600,
      lo: 1414064366
    },
    /* int64 */{
      hi: 1027627185,
      lo: 720592487
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 983582600,
      lo: 1414064366
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 707587463,
      lo: 4050792578
    },
    /* int64 */{
      hi: 457824592,
      lo: 2852982217
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 249762871,
      lo: 1197810361
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 3696312,
      lo: 3842956494
    },
    /* int64 */{
      hi: 303263066,
      lo: 1932508180
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 3696312,
      lo: 3842956494
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1998955230,
      lo: 530108890
    },
    /* int64 */{
      hi: 1876081138,
      lo: 2994715702
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 122874091,
      lo: 1830360484
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1123314058,
      lo: 2451991450
    },
    /* int64 */{
      hi: 1077511003,
      lo: 2658013162
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 45803054,
      lo: 4088945584
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 772515434,
      lo: 3820835012
    },
    /* int64 */{
      hi: 1485983210,
      lo: 435807891
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 772515434,
      lo: 3820835012
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1437309318,
      lo: 1357533220
    },
    /* int64 */{
      hi: 1141241105,
      lo: 541080542
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 296068213,
      lo: 816452678
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1559319564,
      lo: 407118687
    },
    /* int64 */{
      hi: 211092740,
      lo: 4014353660
    },
    /* int64 */{
      hi: 0,
      lo: 7
    },
    /* int64 */{
      hi: 81670377,
      lo: 2371414139
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 897058469,
      lo: 1054256000
    },
    /* int64 */{
      hi: 57853316,
      lo: 661312616
    },
    /* int64 */{
      hi: 0,
      lo: 15
    },
    /* int64 */{
      hi: 29258726,
      lo: 4019468648
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1771820319,
      lo: 3029842884
    },
    /* int64 */{
      hi: 1113086871,
      lo: 222584391
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 658733448,
      lo: 2807258493
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1699471447,
      lo: 2730544778
    },
    /* int64 */{
      hi: 1090632987,
      lo: 3896573910
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 608838459,
      lo: 3128938164
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1300122432,
      lo: 837406327
    },
    /* int64 */{
      hi: 349961722,
      lo: 3861260410
    },
    /* int64 */{
      hi: 0,
      lo: 3
    },
    /* int64 */{
      hi: 250237263,
      lo: 2138526985
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1844919629,
      lo: 681013979
    },
    /* int64 */{
      hi: 141654602,
      lo: 3894038038
    },
    /* int64 */{
      hi: 0,
      lo: 13
    },
    /* int64 */{
      hi: 3409791,
      lo: 1598127037
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 876561860,
      lo: 3227349399
    },
    /* int64 */{
      hi: 1635137811,
      lo: 1118648885
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 876561860,
      lo: 3227349399
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 1444234022,
      lo: 1188873919
    },
    /* int64 */{
      hi: 1506775353,
      lo: 2449062589
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 1444234022,
      lo: 1188873919
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 216713133,
      lo: 2955981804
    },
    /* int64 */{
      hi: 840978153,
      lo: 478170254
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 216713133,
      lo: 2955981804
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 311131250,
      lo: 2343706662
    },
    /* int64 */{
      hi: 1432032601,
      lo: 1397060596
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 311131250,
      lo: 2343706662
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 702252958,
      lo: 2254774319
    },
    /* int64 */{
      hi: 333531300,
      lo: 1983179783
    },
    /* int64 */{
      hi: 0,
      lo: 2
    },
    /* int64 */{
      hi: 35190357,
      lo: 2583382049
    }
  ],
  /* tuple */[
    /* int64 */{
      hi: 222120797,
      lo: 1357843095
    },
    /* int64 */{
      hi: 1409050141,
      lo: 2550433032
    },
    /* int64 */{
      hi: 0,
      lo: 0
    },
    /* int64 */{
      hi: 222120797,
      lo: 1357843095
    }
  ]
];

function from(xs) {
  return List.mapi((function (i, param) {
                var d = param[3];
                var c = param[2];
                var b = param[1];
                var a = param[0];
                return /* tuple */[
                        Curry._1(Printf.sprintf(/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "small_divs ",
                                      /* Scan_get_counter */Block.__(21, [
                                          /* Token_counter */2,
                                          /* End_of_format */0
                                        ])
                                    ]),
                                  "small_divs %L"
                                ]), i),
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      /* tuple */[
                                        c,
                                        d
                                      ],
                                      /* tuple */[
                                        Caml_int64.div(a, b),
                                        Caml_int64.mod_(a, b)
                                      ]
                                    ]);
                          })
                      ];
              }), $$Array.to_list(xs));
}

var to_string = /* array */[/* tuple */[
    /* int64 */{
      hi: 0,
      lo: 0
    },
    "0"
  ]];

var int64_compare_tests = /* array */[
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 1
    },
    /* int64 */{
      hi: 0,
      lo: 2
    },
    -1
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    1
  ],
  /* tuple */[
    /* int64 */{
      hi: 0,
      lo: 2
    },
    /* int64 */{
      hi: 0,
      lo: 1
    },
    1
  ]
];

function from_compare(xs) {
  return List.mapi((function (i, param) {
                var c = param[2];
                var b = param[1];
                var a = param[0];
                return /* tuple */[
                        Curry._1(Printf.sprintf(/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "int64_compare ",
                                      /* Scan_get_counter */Block.__(21, [
                                          /* Token_counter */2,
                                          /* End_of_format */0
                                        ])
                                    ]),
                                  "int64_compare %L"
                                ]), i),
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      c,
                                      Caml_int64.compare(a, b)
                                    ]);
                          })
                      ];
              }), $$Array.to_list(xs));
}

function from_to_string(xs) {
  return List.mapi((function (i, param) {
                var str_a = param[1];
                var a = param[0];
                return /* tuple */[
                        Curry._1(Printf.sprintf(/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "to_string ",
                                      /* Scan_get_counter */Block.__(21, [
                                          /* Token_counter */2,
                                          /* End_of_format */0
                                        ])
                                    ]),
                                  "to_string %L"
                                ]), i),
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      str_a,
                                      Caml_format.caml_int64_format("%d", a)
                                    ]);
                          })
                      ];
              }), $$Array.to_list(xs));
}

Mt.from_pair_suites("Int64_mul_div_test", Pervasives.$at(from_pairs("random", pairs), Pervasives.$at(from_pairs("small", small_pairs), Pervasives.$at(List.mapi((function (i, param) {
                        var f = param[1];
                        var i64 = param[0];
                        return /* tuple */[
                                Curry._1(Printf.sprintf(/* Format */[
                                          /* String_literal */Block.__(11, [
                                              "to_float_",
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ]),
                                          "to_float_%d"
                                        ]), i),
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              Caml_int64.to_float(i64),
                                              f
                                            ]);
                                  })
                              ];
                      }), $$Array.to_list(to_floats)), Pervasives.$at(List.mapi((function (i, param) {
                            var i64 = param[1];
                            var f = param[0];
                            return /* tuple */[
                                    Curry._1(Printf.sprintf(/* Format */[
                                              /* String_literal */Block.__(11, [
                                                  "of_float_",
                                                  /* Int */Block.__(4, [
                                                      /* Int_d */0,
                                                      /* No_padding */0,
                                                      /* No_precision */0,
                                                      /* End_of_format */0
                                                    ])
                                                ]),
                                              "of_float_%d"
                                            ]), i),
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  Caml_int64.of_float(f),
                                                  i64
                                                ]);
                                      })
                                  ];
                          }), $$Array.to_list(of_float_pairs)), Pervasives.$at(/* :: */[
                          /* tuple */[
                            "compare_check_complete",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          $$Array.map((function (param) {
                                                  return true;
                                                }), check_complete_compare),
                                          check_complete_compare
                                        ]);
                              })
                          ],
                          /* [] */0
                        ], Pervasives.$at(from(simple_divs), Pervasives.$at(from_compare(int64_compare_tests), /* :: */[
                                  /* tuple */[
                                    "div_rem_0",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  /* int64 */{
                                                    hi: 0,
                                                    lo: 0
                                                  },
                                                  /* int64 */{
                                                    hi: 0,
                                                    lo: 0
                                                  }
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "div_rem_1",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    /* int64 */{
                                                      hi: -1,
                                                      lo: 4294967295
                                                    },
                                                    /* int64 */{
                                                      hi: -1,
                                                      lo: 4294967295
                                                    }
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "File \"int64_mul_div_test.ml\", line 214, characters 5-12",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      Caml_int64.to_float(Int64.max_int),
                                                      9.22337203685477581e+18
                                                    ]);
                                          })
                                      ],
                                      /* [] */0
                                    ]
                                  ]
                                ]))))))));

exports.commutative_mul = commutative_mul;
exports.pairs = pairs;
exports.from_pairs = from_pairs;
exports.small_pairs = small_pairs;
exports.to_floats = to_floats;
exports.check_complete_compare = check_complete_compare;
exports.of_float_pairs = of_float_pairs;
exports.simple_divs = simple_divs;
exports.from = from;
exports.to_string = to_string;
exports.int64_compare_tests = int64_compare_tests;
exports.from_compare = from_compare;
exports.from_to_string = from_to_string;
/*  Not a pure module */
