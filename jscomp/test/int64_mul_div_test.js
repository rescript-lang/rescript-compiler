// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64  = require("../runtime/caml_int64");
var Caml_obj    = require("../runtime/caml_obj");
var Pervasives  = require("../stdlib/pervasives");
var Caml_format = require("../runtime/caml_format");
var Mt          = require("./mt");
var Block       = require("../runtime/block");
var Curry       = require("../runtime/curry");
var Printf      = require("../stdlib/printf");
var $$Array     = require("../stdlib/array");
var List        = require("../stdlib/list");

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

var pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          -1482940033,
          2541785568
        ],
        /* int64 */[
          1831202545,
          525340320
        ],
        /* int64 */[
          165328154,
          634982515
        ]
      ],
      /* tuple */[
        /* int64 */[
          2086642202,
          4249709544
        ],
        /* int64 */[
          1756378018,
          3051843660
        ],
        /* int64 */[
          1129387921,
          4132619358
        ]
      ],
      /* tuple */[
        /* int64 */[
          -2133087767,
          1358520104
        ],
        /* int64 */[
          209351581,
          3480263932
        ],
        /* int64 */[
          2047885301,
          2699186102
        ]
      ],
      /* tuple */[
        /* int64 */[
          781938191,
          3733162556
        ],
        /* int64 */[
          1243995318,
          3630533041
        ],
        /* int64 */[
          965315102,
          2542797052
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1526298485,
          3121533040
        ],
        /* int64 */[
          806837349,
          4053728232
        ],
        /* int64 */[
          973509509,
          2421939110
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1053250751,
          2477426938
        ],
        /* int64 */[
          297218479,
          3467715454
        ],
        /* int64 */[
          1353404045,
          1639039171
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1614556777,
          1100802137
        ],
        /* int64 */[
          1812737275,
          732603925
        ],
        /* int64 */[
          1686007471,
          1587408437
        ]
      ],
      /* tuple */[
        /* int64 */[
          166521361,
          3339161296
        ],
        /* int64 */[
          585926665,
          2485045435
        ],
        /* int64 */[
          1193284387,
          2883446640
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1556851713,
          2199030589
        ],
        /* int64 */[
          563693579,
          3317959669
        ],
        /* int64 */[
          1423006973,
          700349737
        ]
      ],
      /* tuple */[
        /* int64 */[
          2096650716,
          2212704850
        ],
        /* int64 */[
          1413202597,
          4019257153
        ],
        /* int64 */[
          495794945,
          3451719634
        ]
      ],
      /* tuple */[
        /* int64 */[
          1149398987,
          1313884544
        ],
        /* int64 */[
          556147957,
          255480485
        ],
        /* int64 */[
          1711350082,
          929852288
        ]
      ],
      /* tuple */[
        /* int64 */[
          908394614,
          2507458775
        ],
        /* int64 */[
          1053216964,
          725956947
        ],
        /* int64 */[
          2145390454,
          3984887277
        ]
      ],
      /* tuple */[
        /* int64 */[
          962684198,
          1441406688
        ],
        /* int64 */[
          1528894622,
          468792198
        ],
        /* int64 */[
          1893431833,
          3919858640
        ]
      ],
      /* tuple */[
        /* int64 */[
          -603570361,
          3328354720
        ],
        /* int64 */[
          1842921977,
          1999781536
        ],
        /* int64 */[
          1854314037,
          3255494393
        ]
      ],
      /* tuple */[
        /* int64 */[
          1827458543,
          1187405920
        ],
        /* int64 */[
          1066436782,
          2505967328
        ],
        /* int64 */[
          1004254249,
          2020291989
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1818789533,
          1878451246
        ],
        /* int64 */[
          247021097,
          1999625579
        ],
        /* int64 */[
          1434621550,
          2370451978
        ]
      ],
      /* tuple */[
        /* int64 */[
          1714915951,
          2103538455
        ],
        /* int64 */[
          1071186049,
          45872671
        ],
        /* int64 */[
          911777108,
          2638787593
        ]
      ],
      /* tuple */[
        /* int64 */[
          365880810,
          3950734524
        ],
        /* int64 */[
          1215123423,
          1477626470
        ],
        /* int64 */[
          1155052099,
          1816687658
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1590309406,
          1713682280
        ],
        /* int64 */[
          1236324221,
          2330256524
        ],
        /* int64 */[
          871497139,
          1538765150
        ]
      ],
      /* tuple */[
        /* int64 */[
          -1335640207,
          1074580892
        ],
        /* int64 */[
          153491040,
          2295578284
        ],
        /* int64 */[
          469100620,
          1001897781
        ]
      ]
    ]);

function from_pairs(prefix, pairs) {
  return $$Array.to_list($$Array.mapi(function (i, param) {
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
                          function () {
                            return commutative_mul(result, a, b);
                          }
                        ];
                }, pairs));
}

var small_pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          0,
          121
        ],
        /* int64 */[
          0,
          11
        ],
        /* int64 */[
          0,
          11
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          126736
        ],
        /* int64 */[
          0,
          356
        ],
        /* int64 */[
          0,
          356
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          137176406
        ],
        /* int64 */[
          0,
          12346
        ],
        /* int64 */[
          0,
          11111
        ]
      ],
      /* tuple */[
        /* int64 */[
          268435455,
          4293918720
        ],
        /* int64 */[
          255,
          4294967295
        ],
        /* int64 */[
          0,
          1048576
        ]
      ],
      /* tuple */[
        /* int64 */[
          -268435456,
          1048576
        ],
        /* int64 */[
          -256,
          1
        ],
        /* int64 */[
          0,
          1048576
        ]
      ],
      /* tuple */[
        /* int64 */[
          -639559543,
          1275262484
        ],
        /* int64 */[
          1209011959,
          3147693253
        ],
        /* int64 */[
          1831626934,
          242558724
        ]
      ]
    ]);

var to_floats = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          0,
          1
        ],
        1
      ],
      /* tuple */[
        /* int64 */[
          0,
          2
        ],
        2
      ],
      /* tuple */[
        /* int64 */[
          0,
          4
        ],
        4
      ],
      /* tuple */[
        /* int64 */[
          0,
          8
        ],
        8
      ],
      /* tuple */[
        /* int64 */[
          0,
          16
        ],
        16
      ],
      /* tuple */[
        /* int64 */[
          0,
          32
        ],
        32
      ],
      /* tuple */[
        /* int64 */[
          0,
          64
        ],
        64
      ],
      /* tuple */[
        /* int64 */[
          0,
          128
        ],
        128
      ],
      /* tuple */[
        /* int64 */[
          0,
          256
        ],
        256
      ],
      /* tuple */[
        /* int64 */[
          0,
          512
        ],
        512
      ],
      /* tuple */[
        /* int64 */[
          0,
          1024
        ],
        1024
      ],
      /* tuple */[
        /* int64 */[
          0,
          2048
        ],
        2048
      ],
      /* tuple */[
        /* int64 */[
          0,
          4096
        ],
        4096
      ],
      /* tuple */[
        /* int64 */[
          0,
          8192
        ],
        8192
      ],
      /* tuple */[
        /* int64 */[
          0,
          16384
        ],
        16384
      ],
      /* tuple */[
        /* int64 */[
          0,
          32768
        ],
        32768
      ],
      /* tuple */[
        /* int64 */[
          0,
          65536
        ],
        65536
      ],
      /* tuple */[
        /* int64 */[
          0,
          131072
        ],
        131072
      ],
      /* tuple */[
        /* int64 */[
          0,
          262144
        ],
        262144
      ],
      /* tuple */[
        /* int64 */[
          0,
          524288
        ],
        524288
      ],
      /* tuple */[
        /* int64 */[
          0,
          1048576
        ],
        1048576
      ],
      /* tuple */[
        /* int64 */[
          0,
          2097152
        ],
        2097152
      ],
      /* tuple */[
        /* int64 */[
          0,
          4194304
        ],
        4194304
      ],
      /* tuple */[
        /* int64 */[
          0,
          8388608
        ],
        8388608
      ],
      /* tuple */[
        /* int64 */[
          0,
          16777216
        ],
        16777216
      ],
      /* tuple */[
        /* int64 */[
          0,
          33554432
        ],
        33554432
      ],
      /* tuple */[
        /* int64 */[
          0,
          67108864
        ],
        67108864
      ],
      /* tuple */[
        /* int64 */[
          0,
          134217728
        ],
        134217728
      ],
      /* tuple */[
        /* int64 */[
          0,
          268435456
        ],
        268435456
      ],
      /* tuple */[
        /* int64 */[
          0,
          536870912
        ],
        536870912
      ],
      /* tuple */[
        /* int64 */[
          0,
          1073741824
        ],
        1073741824
      ],
      /* tuple */[
        /* int64 */[
          0,
          2147483648
        ],
        2147483648
      ],
      /* tuple */[
        /* int64 */[
          1,
          0
        ],
        4294967296
      ],
      /* tuple */[
        /* int64 */[
          2,
          0
        ],
        8589934592
      ],
      /* tuple */[
        /* int64 */[
          4,
          0
        ],
        17179869184
      ],
      /* tuple */[
        /* int64 */[
          8,
          0
        ],
        34359738368
      ],
      /* tuple */[
        /* int64 */[
          16,
          0
        ],
        68719476736
      ],
      /* tuple */[
        /* int64 */[
          32,
          0
        ],
        137438953472
      ],
      /* tuple */[
        /* int64 */[
          64,
          0
        ],
        274877906944
      ],
      /* tuple */[
        /* int64 */[
          128,
          0
        ],
        549755813888
      ],
      /* tuple */[
        /* int64 */[
          256,
          0
        ],
        1099511627776
      ],
      /* tuple */[
        /* int64 */[
          512,
          0
        ],
        2199023255552
      ],
      /* tuple */[
        /* int64 */[
          1024,
          0
        ],
        4398046511104
      ],
      /* tuple */[
        /* int64 */[
          2048,
          0
        ],
        8796093022208
      ],
      /* tuple */[
        /* int64 */[
          4096,
          0
        ],
        17592186044416
      ],
      /* tuple */[
        /* int64 */[
          8192,
          0
        ],
        35184372088832
      ],
      /* tuple */[
        /* int64 */[
          16384,
          0
        ],
        70368744177664
      ],
      /* tuple */[
        /* int64 */[
          32768,
          0
        ],
        140737488355328
      ],
      /* tuple */[
        /* int64 */[
          65536,
          0
        ],
        281474976710656
      ],
      /* tuple */[
        /* int64 */[
          131072,
          0
        ],
        562949953421312
      ],
      /* tuple */[
        /* int64 */[
          262144,
          0
        ],
        1125899906842624
      ],
      /* tuple */[
        /* int64 */[
          524288,
          0
        ],
        2251799813685248
      ],
      /* tuple */[
        /* int64 */[
          1048576,
          0
        ],
        4503599627370496
      ],
      /* tuple */[
        /* int64 */[
          2097152,
          0
        ],
        9007199254740992
      ],
      /* tuple */[
        /* int64 */[
          4194304,
          0
        ],
        18014398509481984
      ],
      /* tuple */[
        /* int64 */[
          8388608,
          0
        ],
        36028797018963968
      ],
      /* tuple */[
        /* int64 */[
          16777216,
          0
        ],
        72057594037927936
      ],
      /* tuple */[
        /* int64 */[
          33554432,
          0
        ],
        144115188075855872
      ],
      /* tuple */[
        /* int64 */[
          67108864,
          0
        ],
        288230376151711744
      ],
      /* tuple */[
        /* int64 */[
          134217728,
          0
        ],
        576460752303423488
      ],
      /* tuple */[
        /* int64 */[
          268435456,
          0
        ],
        1.15292150460684698e+18
      ],
      /* tuple */[
        /* int64 */[
          536870912,
          0
        ],
        2.30584300921369395e+18
      ],
      /* tuple */[
        /* int64 */[
          1073741824,
          0
        ],
        4.6116860184273879e+18
      ],
      /* tuple */[
        /* int64 */[
          -2147483648,
          0
        ],
        -9.22337203685477581e+18
      ]
    ]);

var check_complete_compare = /* int array */[
  Caml_int64.ge(/* int64 */[
        0,
        3
      ], /* int64 */[
        0,
        2
      ]),
  Caml_int64.ge(/* int64 */[
        0,
        3
      ], /* int64 */[
        0,
        3
      ]),
  Caml_int64.eq(/* int64 */[
        0,
        3
      ], /* int64 */[
        0,
        3
      ]),
  Caml_int64.eq(/* int64 */[
        0,
        2
      ], /* int64 */[
        0,
        2
      ]),
  Caml_int64.lt(/* int64 */[
        0,
        2
      ], /* int64 */[
        0,
        3
      ]),
  Caml_int64.gt(/* int64 */[
        0,
        3
      ], /* int64 */[
        0,
        2
      ]),
  Caml_int64.le(/* int64 */[
        0,
        2
      ], /* int64 */[
        0,
        3
      ]),
  Caml_int64.le(/* int64 */[
        0,
        3
      ], /* int64 */[
        0,
        3
      ])
];

var of_float_pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        6853066956871844,
        /* int64 */[
          1595603,
          4254472356
        ]
      ],
      /* tuple */[
        -8507688874782117,
        /* int64 */[
          -1980851,
          1388466779
        ]
      ],
      /* tuple */[
        4083117349607451,
        /* int64 */[
          950674,
          3610449947
        ]
      ],
      /* tuple */[
        -4860723193745655,
        /* int64 */[
          -1131726,
          2964287241
        ]
      ],
      /* tuple */[
        7820020192255542,
        /* int64 */[
          1820740,
          1437736502
        ]
      ],
      /* tuple */[
        -4908619721514532,
        /* int64 */[
          -1142878,
          3911803356
        ]
      ],
      /* tuple */[
        5.67685864687671e+15,
        /* int64 */[
          1321746,
          2803257894
        ]
      ],
      /* tuple */[
        -703696191048023,
        /* int64 */[
          -163843,
          4135630505
        ]
      ],
      /* tuple */[
        1123586534990153.88,
        /* int64 */[
          261605,
          1615520073
        ]
      ],
      /* tuple */[
        -4.29886533981922e+15,
        /* int64 */[
          -1000908,
          1786485548
        ]
      ],
      /* tuple */[
        2.43885138012066e+15,
        /* int64 */[
          567839,
          1445727316
        ]
      ],
      /* tuple */[
        -8011538689499494,
        /* int64 */[
          -1865332,
          1246682778
        ]
      ],
      /* tuple */[
        2710072285421155,
        /* int64 */[
          630987,
          3756220003
        ]
      ],
      /* tuple */[
        -2541457347159789.5,
        /* int64 */[
          -591730,
          3650902291
        ]
      ],
      /* tuple */[
        5012932793576708,
        /* int64 */[
          1167164,
          1584508164
        ]
      ],
      /* tuple */[
        -943066847413899.125,
        /* int64 */[
          -219575,
          596605301
        ]
      ],
      /* tuple */[
        5440257518642004,
        /* int64 */[
          1266658,
          2833425236
        ]
      ],
      /* tuple */[
        -7750676773453898,
        /* int64 */[
          -1804596,
          4029038518
        ]
      ],
      /* tuple */[
        8911999221747713,
        /* int64 */[
          2074986,
          2212089857
        ]
      ],
      /* tuple */[
        -1443906702582204.25,
        /* int64 */[
          -336186,
          1172790852
        ]
      ],
      /* tuple */[
        659345820712164.875,
        /* int64 */[
          153515,
          3916266724
        ]
      ],
      /* tuple */[
        -3284023713149006.5,
        /* int64 */[
          -764622,
          2770653106
        ]
      ],
      /* tuple */[
        5062818438579988,
        /* int64 */[
          1178779,
          1184368404
        ]
      ],
      /* tuple */[
        -8904450004162331,
        /* int64 */[
          -2073229,
          747956453
        ]
      ],
      /* tuple */[
        848261089308786,
        /* int64 */[
          197501,
          753381490
        ]
      ],
      /* tuple */[
        -6376579516657391,
        /* int64 */[
          -1484664,
          3808891153
        ]
      ],
      /* tuple */[
        1337907592605664.25,
        /* int64 */[
          311505,
          3805065184
        ]
      ],
      /* tuple */[
        -8.54733738833896e+15,
        /* int64 */[
          -1990083,
          4012986608
        ]
      ],
      /* tuple */[
        2345417644172927,
        /* int64 */[
          546085,
          428336767
        ]
      ],
      /* tuple */[
        -2587460670129294.5,
        /* int64 */[
          -602441,
          3722640242
        ]
      ],
      /* tuple */[
        4580431718597436,
        /* int64 */[
          1066464,
          3716236092
        ]
      ],
      /* tuple */[
        -1460576044874256.25,
        /* int64 */[
          -340067,
          598574576
        ]
      ],
      /* tuple */[
        3403657978343579.5,
        /* int64 */[
          792475,
          3770445979
        ]
      ],
      /* tuple */[
        -7.89068917321888e+15,
        /* int64 */[
          -1837195,
          3268155840
        ]
      ],
      /* tuple */[
        1683098350604788.5,
        /* int64 */[
          391876,
          3746517492
        ]
      ],
      /* tuple */[
        -3966538891560174.5,
        /* int64 */[
          -923532,
          845249298
        ]
      ],
      /* tuple */[
        6726025288963652,
        /* int64 */[
          1566024,
          3424212548
        ]
      ],
      /* tuple */[
        -4790410747298403,
        /* int64 */[
          -1115355,
          2501131677
        ]
      ],
      /* tuple */[
        1985858071337706.25,
        /* int64 */[
          462368,
          2632620778
        ]
      ],
      /* tuple */[
        -5281733497873409,
        /* int64 */[
          -1229750,
          2534382591
        ]
      ]
    ]);

var simple_divs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          0,
          6
        ],
        /* int64 */[
          0,
          3
        ],
        /* int64 */[
          0,
          2
        ],
        /* int64 */[
          0,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          120
        ],
        /* int64 */[
          0,
          11
        ],
        /* int64 */[
          0,
          10
        ],
        /* int64 */[
          0,
          10
        ]
      ],
      /* tuple */[
        /* int64 */[
          -2147483648,
          0
        ],
        /* int64 */[
          0,
          2
        ],
        /* int64 */[
          -1073741824,
          0
        ],
        /* int64 */[
          0,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          1112580415,
          4131866785
        ],
        /* int64 */[
          2013350321,
          2605406679
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          1112580415,
          4131866785
        ]
      ],
      /* tuple */[
        /* int64 */[
          983582600,
          1414064366
        ],
        /* int64 */[
          1027627185,
          720592487
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          983582600,
          1414064366
        ]
      ],
      /* tuple */[
        /* int64 */[
          707587463,
          4050792578
        ],
        /* int64 */[
          457824592,
          2852982217
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          249762871,
          1197810361
        ]
      ],
      /* tuple */[
        /* int64 */[
          3696312,
          3842956494
        ],
        /* int64 */[
          303263066,
          1932508180
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          3696312,
          3842956494
        ]
      ],
      /* tuple */[
        /* int64 */[
          1998955230,
          530108890
        ],
        /* int64 */[
          1876081138,
          2994715702
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          122874091,
          1830360484
        ]
      ],
      /* tuple */[
        /* int64 */[
          1123314058,
          2451991450
        ],
        /* int64 */[
          1077511003,
          2658013162
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          45803054,
          4088945584
        ]
      ],
      /* tuple */[
        /* int64 */[
          772515434,
          3820835012
        ],
        /* int64 */[
          1485983210,
          435807891
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          772515434,
          3820835012
        ]
      ],
      /* tuple */[
        /* int64 */[
          1437309318,
          1357533220
        ],
        /* int64 */[
          1141241105,
          541080542
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          296068213,
          816452678
        ]
      ],
      /* tuple */[
        /* int64 */[
          1559319564,
          407118687
        ],
        /* int64 */[
          211092740,
          4014353660
        ],
        /* int64 */[
          0,
          7
        ],
        /* int64 */[
          81670377,
          2371414139
        ]
      ],
      /* tuple */[
        /* int64 */[
          897058469,
          1054256000
        ],
        /* int64 */[
          57853316,
          661312616
        ],
        /* int64 */[
          0,
          15
        ],
        /* int64 */[
          29258726,
          4019468648
        ]
      ],
      /* tuple */[
        /* int64 */[
          1771820319,
          3029842884
        ],
        /* int64 */[
          1113086871,
          222584391
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          658733448,
          2807258493
        ]
      ],
      /* tuple */[
        /* int64 */[
          1699471447,
          2730544778
        ],
        /* int64 */[
          1090632987,
          3896573910
        ],
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          608838459,
          3128938164
        ]
      ],
      /* tuple */[
        /* int64 */[
          1300122432,
          837406327
        ],
        /* int64 */[
          349961722,
          3861260410
        ],
        /* int64 */[
          0,
          3
        ],
        /* int64 */[
          250237263,
          2138526985
        ]
      ],
      /* tuple */[
        /* int64 */[
          1844919629,
          681013979
        ],
        /* int64 */[
          141654602,
          3894038038
        ],
        /* int64 */[
          0,
          13
        ],
        /* int64 */[
          3409791,
          1598127037
        ]
      ],
      /* tuple */[
        /* int64 */[
          876561860,
          3227349399
        ],
        /* int64 */[
          1635137811,
          1118648885
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          876561860,
          3227349399
        ]
      ],
      /* tuple */[
        /* int64 */[
          1444234022,
          1188873919
        ],
        /* int64 */[
          1506775353,
          2449062589
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          1444234022,
          1188873919
        ]
      ],
      /* tuple */[
        /* int64 */[
          216713133,
          2955981804
        ],
        /* int64 */[
          840978153,
          478170254
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          216713133,
          2955981804
        ]
      ],
      /* tuple */[
        /* int64 */[
          311131250,
          2343706662
        ],
        /* int64 */[
          1432032601,
          1397060596
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          311131250,
          2343706662
        ]
      ],
      /* tuple */[
        /* int64 */[
          702252958,
          2254774319
        ],
        /* int64 */[
          333531300,
          1983179783
        ],
        /* int64 */[
          0,
          2
        ],
        /* int64 */[
          35190357,
          2583382049
        ]
      ],
      /* tuple */[
        /* int64 */[
          222120797,
          1357843095
        ],
        /* int64 */[
          1409050141,
          2550433032
        ],
        /* int64 */[
          0,
          0
        ],
        /* int64 */[
          222120797,
          1357843095
        ]
      ]
    ]);

function from(xs) {
  return List.mapi(function (i, param) {
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
                      function () {
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
                      }
                    ];
            }, $$Array.to_list(xs));
}

var to_string = /* array */[/* tuple */[
    /* int64 */[
      0,
      0
    ],
    "0"
  ]];

var int64_compare_tests = /* array */[
  /* tuple */[
    /* int64 */[
      0,
      1
    ],
    /* int64 */[
      0,
      2
    ],
    -1
  ],
  /* tuple */[
    /* int64 */[
      0,
      2
    ],
    /* int64 */[
      0,
      1
    ],
    1
  ],
  /* tuple */[
    /* int64 */[
      0,
      2
    ],
    /* int64 */[
      0,
      1
    ],
    1
  ]
];

function from_compare(xs) {
  return List.mapi(function (i, param) {
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
                      function () {
                        return /* Eq */Block.__(0, [
                                  c,
                                  Caml_int64.compare(a, b)
                                ]);
                      }
                    ];
            }, $$Array.to_list(xs));
}

function from_to_string(xs) {
  return List.mapi(function (i, param) {
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
                      function () {
                        return /* Eq */Block.__(0, [
                                  str_a,
                                  Caml_format.caml_int64_format("%d", a)
                                ]);
                      }
                    ];
            }, $$Array.to_list(xs));
}

Mt.from_pair_suites("int64_mul_div_test.ml", Pervasives.$at(from_pairs("random", pairs), Pervasives.$at(from_pairs("small", small_pairs), Pervasives.$at(List.mapi(function (i, param) {
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
                              function () {
                                return /* Eq */Block.__(0, [
                                          Caml_int64.to_float(i64),
                                          f
                                        ]);
                              }
                            ];
                    }, $$Array.to_list(to_floats)), Pervasives.$at(List.mapi(function (i, param) {
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
                                  function () {
                                    return /* Eq */Block.__(0, [
                                              Caml_int64.of_float(f),
                                              i64
                                            ]);
                                  }
                                ];
                        }, $$Array.to_list(of_float_pairs)), Pervasives.$at(/* :: */[
                          /* tuple */[
                            "compare_check_complete",
                            function () {
                              return /* Eq */Block.__(0, [
                                        $$Array.map(function () {
                                              return /* true */1;
                                            }, check_complete_compare),
                                        check_complete_compare
                                      ]);
                            }
                          ],
                          /* [] */0
                        ], Pervasives.$at(from(simple_divs), Pervasives.$at(from_compare(int64_compare_tests), /* :: */[
                                  /* tuple */[
                                    "div_rem_0",
                                    function () {
                                      return /* Eq */Block.__(0, [
                                                Caml_int64.div(/* int64 */[
                                                      -1,
                                                      4294967295
                                                    ], /* int64 */[
                                                      0,
                                                      16
                                                    ]),
                                                /* int64 */[
                                                  0,
                                                  0
                                                ]
                                              ]);
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "div_rem_1",
                                      function () {
                                        return /* Eq */Block.__(0, [
                                                  Caml_int64.mod_(/* int64 */[
                                                        -1,
                                                        4294967295
                                                      ], /* int64 */[
                                                        0,
                                                        16
                                                      ]),
                                                  /* int64 */[
                                                    -1,
                                                    4294967295
                                                  ]
                                                ]);
                                      }
                                    ],
                                    /* [] */0
                                  ]
                                ]))))))));

exports.commutative_mul        = commutative_mul;
exports.pairs                  = pairs;
exports.from_pairs             = from_pairs;
exports.small_pairs            = small_pairs;
exports.to_floats              = to_floats;
exports.check_complete_compare = check_complete_compare;
exports.of_float_pairs         = of_float_pairs;
exports.simple_divs            = simple_divs;
exports.from                   = from;
exports.to_string              = to_string;
exports.int64_compare_tests    = int64_compare_tests;
exports.from_compare           = from_compare;
exports.from_to_string         = from_to_string;
/*  Not a pure module */
