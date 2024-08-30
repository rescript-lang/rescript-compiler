// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let List = require("../../lib/js/list.js");
let $$Array = require("../../lib/js/array.js");
let Int64 = require("../../lib/js/int64.js");
let Caml_int64 = require("../../lib/js/caml_int64.js");
let Pervasives = require("../../lib/js/pervasives.js");

function commutative_mul(result, a, b) {
  return {
    TAG: "Eq",
    _0: [
      result,
      result
    ],
    _1: [
      Caml_int64.mul(a, b),
      Caml_int64.mul(b, a)
    ]
  };
}

let pairs = [
  [
    [
      -1482940033,
      2541785568
    ],
    [
      1831202545,
      525340320
    ],
    [
      165328154,
      634982515
    ]
  ],
  [
    [
      2086642202,
      4249709544
    ],
    [
      1756378018,
      3051843660
    ],
    [
      1129387921,
      4132619358
    ]
  ],
  [
    [
      -2133087767,
      1358520104
    ],
    [
      209351581,
      3480263932
    ],
    [
      2047885301,
      2699186102
    ]
  ],
  [
    [
      781938191,
      3733162556
    ],
    [
      1243995318,
      3630533041
    ],
    [
      965315102,
      2542797052
    ]
  ],
  [
    [
      -1526298485,
      3121533040
    ],
    [
      806837349,
      4053728232
    ],
    [
      973509509,
      2421939110
    ]
  ],
  [
    [
      -1053250751,
      2477426938
    ],
    [
      297218479,
      3467715454
    ],
    [
      1353404045,
      1639039171
    ]
  ],
  [
    [
      -1614556777,
      1100802137
    ],
    [
      1812737275,
      732603925
    ],
    [
      1686007471,
      1587408437
    ]
  ],
  [
    [
      166521361,
      3339161296
    ],
    [
      585926665,
      2485045435
    ],
    [
      1193284387,
      2883446640
    ]
  ],
  [
    [
      -1556851713,
      2199030589
    ],
    [
      563693579,
      3317959669
    ],
    [
      1423006973,
      700349737
    ]
  ],
  [
    [
      2096650716,
      2212704850
    ],
    [
      1413202597,
      4019257153
    ],
    [
      495794945,
      3451719634
    ]
  ],
  [
    [
      1149398987,
      1313884544
    ],
    [
      556147957,
      255480485
    ],
    [
      1711350082,
      929852288
    ]
  ],
  [
    [
      908394614,
      2507458775
    ],
    [
      1053216964,
      725956947
    ],
    [
      2145390454,
      3984887277
    ]
  ],
  [
    [
      962684198,
      1441406688
    ],
    [
      1528894622,
      468792198
    ],
    [
      1893431833,
      3919858640
    ]
  ],
  [
    [
      -603570361,
      3328354720
    ],
    [
      1842921977,
      1999781536
    ],
    [
      1854314037,
      3255494393
    ]
  ],
  [
    [
      1827458543,
      1187405920
    ],
    [
      1066436782,
      2505967328
    ],
    [
      1004254249,
      2020291989
    ]
  ],
  [
    [
      -1818789533,
      1878451246
    ],
    [
      247021097,
      1999625579
    ],
    [
      1434621550,
      2370451978
    ]
  ],
  [
    [
      1714915951,
      2103538455
    ],
    [
      1071186049,
      45872671
    ],
    [
      911777108,
      2638787593
    ]
  ],
  [
    [
      365880810,
      3950734524
    ],
    [
      1215123423,
      1477626470
    ],
    [
      1155052099,
      1816687658
    ]
  ],
  [
    [
      -1590309406,
      1713682280
    ],
    [
      1236324221,
      2330256524
    ],
    [
      871497139,
      1538765150
    ]
  ],
  [
    [
      -1335640207,
      1074580892
    ],
    [
      153491040,
      2295578284
    ],
    [
      469100620,
      1001897781
    ]
  ]
];

function from_pairs(prefix, pairs) {
  return $$Array.to_list($$Array.mapi((i, param) => {
    let b = param[2];
    let a = param[1];
    let result = param[0];
    return [
      prefix + "_" + i,
      param => commutative_mul(result, a, b)
    ];
  }, pairs));
}

let small_pairs = [
  [
    [
      0,
      121
    ],
    [
      0,
      11
    ],
    [
      0,
      11
    ]
  ],
  [
    [
      0,
      126736
    ],
    [
      0,
      356
    ],
    [
      0,
      356
    ]
  ],
  [
    [
      0,
      137176406
    ],
    [
      0,
      12346
    ],
    [
      0,
      11111
    ]
  ],
  [
    [
      268435455,
      4293918720
    ],
    [
      255,
      4294967295
    ],
    [
      0,
      1048576
    ]
  ],
  [
    [
      -268435456,
      1048576
    ],
    [
      -256,
      1
    ],
    [
      0,
      1048576
    ]
  ],
  [
    [
      -639559543,
      1275262484
    ],
    [
      1209011959,
      3147693253
    ],
    [
      1831626934,
      242558724
    ]
  ]
];

let to_floats = [
  [
    Caml_int64.one,
    1
  ],
  [
    [
      0,
      2
    ],
    2
  ],
  [
    [
      0,
      4
    ],
    4
  ],
  [
    [
      0,
      8
    ],
    8
  ],
  [
    [
      0,
      16
    ],
    16
  ],
  [
    [
      0,
      32
    ],
    32
  ],
  [
    [
      0,
      64
    ],
    64
  ],
  [
    [
      0,
      128
    ],
    128
  ],
  [
    [
      0,
      256
    ],
    256
  ],
  [
    [
      0,
      512
    ],
    512
  ],
  [
    [
      0,
      1024
    ],
    1024
  ],
  [
    [
      0,
      2048
    ],
    2048
  ],
  [
    [
      0,
      4096
    ],
    4096
  ],
  [
    [
      0,
      8192
    ],
    8192
  ],
  [
    [
      0,
      16384
    ],
    16384
  ],
  [
    [
      0,
      32768
    ],
    32768
  ],
  [
    [
      0,
      65536
    ],
    65536
  ],
  [
    [
      0,
      131072
    ],
    131072
  ],
  [
    [
      0,
      262144
    ],
    262144
  ],
  [
    [
      0,
      524288
    ],
    524288
  ],
  [
    [
      0,
      1048576
    ],
    1048576
  ],
  [
    [
      0,
      2097152
    ],
    2097152
  ],
  [
    [
      0,
      4194304
    ],
    4194304
  ],
  [
    [
      0,
      8388608
    ],
    8388608
  ],
  [
    [
      0,
      16777216
    ],
    16777216
  ],
  [
    [
      0,
      33554432
    ],
    33554432
  ],
  [
    [
      0,
      67108864
    ],
    67108864
  ],
  [
    [
      0,
      134217728
    ],
    134217728
  ],
  [
    [
      0,
      268435456
    ],
    268435456
  ],
  [
    [
      0,
      536870912
    ],
    536870912
  ],
  [
    [
      0,
      1073741824
    ],
    1073741824
  ],
  [
    [
      0,
      2147483648
    ],
    2147483648
  ],
  [
    [
      1,
      0
    ],
    4294967296
  ],
  [
    [
      2,
      0
    ],
    8589934592
  ],
  [
    [
      4,
      0
    ],
    17179869184
  ],
  [
    [
      8,
      0
    ],
    34359738368
  ],
  [
    [
      16,
      0
    ],
    68719476736
  ],
  [
    [
      32,
      0
    ],
    137438953472
  ],
  [
    [
      64,
      0
    ],
    274877906944
  ],
  [
    [
      128,
      0
    ],
    549755813888
  ],
  [
    [
      256,
      0
    ],
    1099511627776
  ],
  [
    [
      512,
      0
    ],
    2199023255552
  ],
  [
    [
      1024,
      0
    ],
    4398046511104
  ],
  [
    [
      2048,
      0
    ],
    8796093022208
  ],
  [
    [
      4096,
      0
    ],
    17592186044416
  ],
  [
    [
      8192,
      0
    ],
    35184372088832
  ],
  [
    [
      16384,
      0
    ],
    70368744177664
  ],
  [
    [
      32768,
      0
    ],
    140737488355328
  ],
  [
    [
      65536,
      0
    ],
    281474976710656
  ],
  [
    [
      131072,
      0
    ],
    562949953421312
  ],
  [
    [
      262144,
      0
    ],
    1125899906842624
  ],
  [
    [
      524288,
      0
    ],
    2251799813685248
  ],
  [
    [
      1048576,
      0
    ],
    4503599627370496
  ],
  [
    [
      2097152,
      0
    ],
    9007199254740992
  ],
  [
    [
      4194304,
      0
    ],
    18014398509481984
  ],
  [
    [
      8388608,
      0
    ],
    36028797018963968
  ],
  [
    [
      16777216,
      0
    ],
    72057594037927936
  ],
  [
    [
      33554432,
      0
    ],
    144115188075855872
  ],
  [
    [
      67108864,
      0
    ],
    288230376151711744
  ],
  [
    [
      134217728,
      0
    ],
    576460752303423488
  ],
  [
    [
      268435456,
      0
    ],
    1.15292150460684698e+18
  ],
  [
    [
      536870912,
      0
    ],
    2.30584300921369395e+18
  ],
  [
    [
      1073741824,
      0
    ],
    4.6116860184273879e+18
  ],
  [
    Caml_int64.min_int,
    -9.22337203685477581e+18
  ]
];

let check_complete_compare = [
  true,
  true,
  true,
  true,
  true,
  true,
  true,
  true
];

let of_float_pairs = [
  [
    6853066956871844,
    [
      1595603,
      4254472356
    ]
  ],
  [
    -8507688874782117,
    [
      -1980851,
      1388466779
    ]
  ],
  [
    4083117349607451,
    [
      950674,
      3610449947
    ]
  ],
  [
    -4860723193745655,
    [
      -1131726,
      2964287241
    ]
  ],
  [
    7820020192255542,
    [
      1820740,
      1437736502
    ]
  ],
  [
    -4908619721514532,
    [
      -1142878,
      3911803356
    ]
  ],
  [
    5.67685864687671e+15,
    [
      1321746,
      2803257894
    ]
  ],
  [
    -703696191048023,
    [
      -163843,
      4135630505
    ]
  ],
  [
    1123586534990153.88,
    [
      261605,
      1615520073
    ]
  ],
  [
    -4.29886533981922e+15,
    [
      -1000908,
      1786485548
    ]
  ],
  [
    2.43885138012066e+15,
    [
      567839,
      1445727316
    ]
  ],
  [
    -8011538689499494,
    [
      -1865332,
      1246682778
    ]
  ],
  [
    2710072285421155,
    [
      630987,
      3756220003
    ]
  ],
  [
    -2541457347159789.5,
    [
      -591730,
      3650902291
    ]
  ],
  [
    5012932793576708,
    [
      1167164,
      1584508164
    ]
  ],
  [
    -943066847413899.125,
    [
      -219575,
      596605301
    ]
  ],
  [
    5440257518642004,
    [
      1266658,
      2833425236
    ]
  ],
  [
    -7750676773453898,
    [
      -1804596,
      4029038518
    ]
  ],
  [
    8911999221747713,
    [
      2074986,
      2212089857
    ]
  ],
  [
    -1443906702582204.25,
    [
      -336186,
      1172790852
    ]
  ],
  [
    659345820712164.875,
    [
      153515,
      3916266724
    ]
  ],
  [
    -3284023713149006.5,
    [
      -764622,
      2770653106
    ]
  ],
  [
    5062818438579988,
    [
      1178779,
      1184368404
    ]
  ],
  [
    -8904450004162331,
    [
      -2073229,
      747956453
    ]
  ],
  [
    848261089308786,
    [
      197501,
      753381490
    ]
  ],
  [
    -6376579516657391,
    [
      -1484664,
      3808891153
    ]
  ],
  [
    1337907592605664.25,
    [
      311505,
      3805065184
    ]
  ],
  [
    -8.54733738833896e+15,
    [
      -1990083,
      4012986608
    ]
  ],
  [
    2345417644172927,
    [
      546085,
      428336767
    ]
  ],
  [
    -2587460670129294.5,
    [
      -602441,
      3722640242
    ]
  ],
  [
    4580431718597436,
    [
      1066464,
      3716236092
    ]
  ],
  [
    -1460576044874256.25,
    [
      -340067,
      598574576
    ]
  ],
  [
    3403657978343579.5,
    [
      792475,
      3770445979
    ]
  ],
  [
    -7.89068917321888e+15,
    [
      -1837195,
      3268155840
    ]
  ],
  [
    1683098350604788.5,
    [
      391876,
      3746517492
    ]
  ],
  [
    -3966538891560174.5,
    [
      -923532,
      845249298
    ]
  ],
  [
    6726025288963652,
    [
      1566024,
      3424212548
    ]
  ],
  [
    -4790410747298403,
    [
      -1115355,
      2501131677
    ]
  ],
  [
    1985858071337706.25,
    [
      462368,
      2632620778
    ]
  ],
  [
    -5281733497873409,
    [
      -1229750,
      2534382591
    ]
  ]
];

let simple_divs = [
  [
    [
      0,
      6
    ],
    [
      0,
      3
    ],
    [
      0,
      2
    ],
    Caml_int64.zero
  ],
  [
    [
      0,
      120
    ],
    [
      0,
      11
    ],
    [
      0,
      10
    ],
    [
      0,
      10
    ]
  ],
  [
    Caml_int64.min_int,
    [
      0,
      2
    ],
    [
      -1073741824,
      0
    ],
    Caml_int64.zero
  ],
  [
    [
      1112580415,
      4131866785
    ],
    [
      2013350321,
      2605406679
    ],
    Caml_int64.zero,
    [
      1112580415,
      4131866785
    ]
  ],
  [
    [
      983582600,
      1414064366
    ],
    [
      1027627185,
      720592487
    ],
    Caml_int64.zero,
    [
      983582600,
      1414064366
    ]
  ],
  [
    [
      707587463,
      4050792578
    ],
    [
      457824592,
      2852982217
    ],
    Caml_int64.one,
    [
      249762871,
      1197810361
    ]
  ],
  [
    [
      3696312,
      3842956494
    ],
    [
      303263066,
      1932508180
    ],
    Caml_int64.zero,
    [
      3696312,
      3842956494
    ]
  ],
  [
    [
      1998955230,
      530108890
    ],
    [
      1876081138,
      2994715702
    ],
    Caml_int64.one,
    [
      122874091,
      1830360484
    ]
  ],
  [
    [
      1123314058,
      2451991450
    ],
    [
      1077511003,
      2658013162
    ],
    Caml_int64.one,
    [
      45803054,
      4088945584
    ]
  ],
  [
    [
      772515434,
      3820835012
    ],
    [
      1485983210,
      435807891
    ],
    Caml_int64.zero,
    [
      772515434,
      3820835012
    ]
  ],
  [
    [
      1437309318,
      1357533220
    ],
    [
      1141241105,
      541080542
    ],
    Caml_int64.one,
    [
      296068213,
      816452678
    ]
  ],
  [
    [
      1559319564,
      407118687
    ],
    [
      211092740,
      4014353660
    ],
    [
      0,
      7
    ],
    [
      81670377,
      2371414139
    ]
  ],
  [
    [
      897058469,
      1054256000
    ],
    [
      57853316,
      661312616
    ],
    [
      0,
      15
    ],
    [
      29258726,
      4019468648
    ]
  ],
  [
    [
      1771820319,
      3029842884
    ],
    [
      1113086871,
      222584391
    ],
    Caml_int64.one,
    [
      658733448,
      2807258493
    ]
  ],
  [
    [
      1699471447,
      2730544778
    ],
    [
      1090632987,
      3896573910
    ],
    Caml_int64.one,
    [
      608838459,
      3128938164
    ]
  ],
  [
    [
      1300122432,
      837406327
    ],
    [
      349961722,
      3861260410
    ],
    [
      0,
      3
    ],
    [
      250237263,
      2138526985
    ]
  ],
  [
    [
      1844919629,
      681013979
    ],
    [
      141654602,
      3894038038
    ],
    [
      0,
      13
    ],
    [
      3409791,
      1598127037
    ]
  ],
  [
    [
      876561860,
      3227349399
    ],
    [
      1635137811,
      1118648885
    ],
    Caml_int64.zero,
    [
      876561860,
      3227349399
    ]
  ],
  [
    [
      1444234022,
      1188873919
    ],
    [
      1506775353,
      2449062589
    ],
    Caml_int64.zero,
    [
      1444234022,
      1188873919
    ]
  ],
  [
    [
      216713133,
      2955981804
    ],
    [
      840978153,
      478170254
    ],
    Caml_int64.zero,
    [
      216713133,
      2955981804
    ]
  ],
  [
    [
      311131250,
      2343706662
    ],
    [
      1432032601,
      1397060596
    ],
    Caml_int64.zero,
    [
      311131250,
      2343706662
    ]
  ],
  [
    [
      702252958,
      2254774319
    ],
    [
      333531300,
      1983179783
    ],
    [
      0,
      2
    ],
    [
      35190357,
      2583382049
    ]
  ],
  [
    [
      222120797,
      1357843095
    ],
    [
      1409050141,
      2550433032
    ],
    Caml_int64.zero,
    [
      222120797,
      1357843095
    ]
  ]
];

function from(xs) {
  return List.mapi((i, param) => {
    let d = param[3];
    let c = param[2];
    let b = param[1];
    let a = param[0];
    return [
      "small_divs " + i,
      param => ({
        TAG: "Eq",
        _0: [
          c,
          d
        ],
        _1: [
          Caml_int64.div(a, b),
          Caml_int64.mod_(a, b)
        ]
      })
    ];
  }, $$Array.to_list(xs));
}

let to_string = [[
    Caml_int64.zero,
    "0"
  ]];

let int64_compare_tests = [
  [
    Caml_int64.one,
    [
      0,
      2
    ],
    -1
  ],
  [
    [
      0,
      2
    ],
    Caml_int64.one,
    1
  ],
  [
    [
      0,
      2
    ],
    Caml_int64.one,
    1
  ]
];

function from_compare(xs) {
  return List.mapi((i, param) => {
    let c = param[2];
    let b = param[1];
    let a = param[0];
    return [
      "int64_compare " + i,
      param => ({
        TAG: "Eq",
        _0: c,
        _1: Caml_int64.compare(a, b)
      })
    ];
  }, $$Array.to_list(xs));
}

function from_to_string(xs) {
  return List.mapi((i, param) => {
    let str_a = param[1];
    let a = param[0];
    return [
      "to_string " + i,
      param => ({
        TAG: "Eq",
        _0: str_a,
        _1: Caml_int64.to_string(a)
      })
    ];
  }, $$Array.to_list(xs));
}

Mt.from_pair_suites("Int64_mul_div_test", Pervasives.$at(from_pairs("random", pairs), Pervasives.$at(from_pairs("small", small_pairs), Pervasives.$at(List.mapi((i, param) => {
  let f = param[1];
  let i64 = param[0];
  return [
    "to_float_" + i,
    () => ({
      TAG: "Eq",
      _0: Caml_int64.to_float(i64),
      _1: f
    })
  ];
}, $$Array.to_list(to_floats)), Pervasives.$at(List.mapi((i, param) => {
  let i64 = param[1];
  let f = param[0];
  return [
    "of_float_" + i,
    () => ({
      TAG: "Eq",
      _0: Caml_int64.of_float(f),
      _1: i64
    })
  ];
}, $$Array.to_list(of_float_pairs)), Pervasives.$at({
  hd: [
    "compare_check_complete",
    () => ({
      TAG: "Eq",
      _0: $$Array.map(param => true, check_complete_compare),
      _1: check_complete_compare
    })
  ],
  tl: /* [] */0
}, Pervasives.$at(from(simple_divs), Pervasives.$at(from_compare(int64_compare_tests), {
  hd: [
    "div_rem_0",
    () => ({
      TAG: "Eq",
      _0: Caml_int64.zero,
      _1: Caml_int64.zero
    })
  ],
  tl: {
    hd: [
      "div_rem_1",
      () => ({
        TAG: "Eq",
        _0: Caml_int64.neg_one,
        _1: Caml_int64.neg_one
      })
    ],
    tl: {
      hd: [
        "File \"int64_mul_div_test.res\", line 256, characters 19-26",
        () => ({
          TAG: "Eq",
          _0: Caml_int64.to_float(Int64.max_int),
          _1: 9.22337203685477581e+18
        })
      ],
      tl: /* [] */0
    }
  }
}))))))));

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
