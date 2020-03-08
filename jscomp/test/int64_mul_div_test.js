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

var pairs = [
  /* tuple */[
    Caml_int64.mk(-1753181728, -1482940033),
    Caml_int64.mk(525340320, 1831202545),
    Caml_int64.mk(634982515, 165328154)
  ],
  /* tuple */[
    Caml_int64.mk(-45257752, 2086642202),
    Caml_int64.mk(-1243123636, 1756378018),
    Caml_int64.mk(-162347938, 1129387921)
  ],
  /* tuple */[
    Caml_int64.mk(1358520104, -2133087767),
    Caml_int64.mk(-814703364, 209351581),
    Caml_int64.mk(-1595781194, 2047885301)
  ],
  /* tuple */[
    Caml_int64.mk(-561804740, 781938191),
    Caml_int64.mk(-664434255, 1243995318),
    Caml_int64.mk(-1752170244, 965315102)
  ],
  /* tuple */[
    Caml_int64.mk(-1173434256, -1526298485),
    Caml_int64.mk(-241239064, 806837349),
    Caml_int64.mk(-1873028186, 973509509)
  ],
  /* tuple */[
    Caml_int64.mk(-1817540358, -1053250751),
    Caml_int64.mk(-827251842, 297218479),
    Caml_int64.mk(1639039171, 1353404045)
  ],
  /* tuple */[
    Caml_int64.mk(1100802137, -1614556777),
    Caml_int64.mk(732603925, 1812737275),
    Caml_int64.mk(1587408437, 1686007471)
  ],
  /* tuple */[
    Caml_int64.mk(-955806000, 166521361),
    Caml_int64.mk(-1809921861, 585926665),
    Caml_int64.mk(-1411520656, 1193284387)
  ],
  /* tuple */[
    Caml_int64.mk(-2095936707, -1556851713),
    Caml_int64.mk(-977007627, 563693579),
    Caml_int64.mk(700349737, 1423006973)
  ],
  /* tuple */[
    Caml_int64.mk(-2082262446, 2096650716),
    Caml_int64.mk(-275710143, 1413202597),
    Caml_int64.mk(-843247662, 495794945)
  ],
  /* tuple */[
    Caml_int64.mk(1313884544, 1149398987),
    Caml_int64.mk(255480485, 556147957),
    Caml_int64.mk(929852288, 1711350082)
  ],
  /* tuple */[
    Caml_int64.mk(-1787508521, 908394614),
    Caml_int64.mk(725956947, 1053216964),
    Caml_int64.mk(-310080019, 2145390454)
  ],
  /* tuple */[
    Caml_int64.mk(1441406688, 962684198),
    Caml_int64.mk(468792198, 1528894622),
    Caml_int64.mk(-375108656, 1893431833)
  ],
  /* tuple */[
    Caml_int64.mk(-966612576, -603570361),
    Caml_int64.mk(1999781536, 1842921977),
    Caml_int64.mk(-1039472903, 1854314037)
  ],
  /* tuple */[
    Caml_int64.mk(1187405920, 1827458543),
    Caml_int64.mk(-1788999968, 1066436782),
    Caml_int64.mk(2020291989, 1004254249)
  ],
  /* tuple */[
    Caml_int64.mk(1878451246, -1818789533),
    Caml_int64.mk(1999625579, 247021097),
    Caml_int64.mk(-1924515318, 1434621550)
  ],
  /* tuple */[
    Caml_int64.mk(2103538455, 1714915951),
    Caml_int64.mk(45872671, 1071186049),
    Caml_int64.mk(-1656179703, 911777108)
  ],
  /* tuple */[
    Caml_int64.mk(-344232772, 365880810),
    Caml_int64.mk(1477626470, 1215123423),
    Caml_int64.mk(1816687658, 1155052099)
  ],
  /* tuple */[
    Caml_int64.mk(1713682280, -1590309406),
    Caml_int64.mk(-1964710772, 1236324221),
    Caml_int64.mk(1538765150, 871497139)
  ],
  /* tuple */[
    Caml_int64.mk(1074580892, -1335640207),
    Caml_int64.mk(-1999389012, 153491040),
    Caml_int64.mk(1001897781, 469100620)
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

var small_pairs = [
  /* tuple */[
    Caml_int64.mk(121, 0),
    Caml_int64.mk(11, 0),
    Caml_int64.mk(11, 0)
  ],
  /* tuple */[
    Caml_int64.mk(126736, 0),
    Caml_int64.mk(356, 0),
    Caml_int64.mk(356, 0)
  ],
  /* tuple */[
    Caml_int64.mk(137176406, 0),
    Caml_int64.mk(12346, 0),
    Caml_int64.mk(11111, 0)
  ],
  /* tuple */[
    Caml_int64.mk(-1048576, 268435455),
    Caml_int64.mk(-1, 255),
    Caml_int64.mk(1048576, 0)
  ],
  /* tuple */[
    Caml_int64.mk(1048576, -268435456),
    Caml_int64.mk(1, -256),
    Caml_int64.mk(1048576, 0)
  ],
  /* tuple */[
    Caml_int64.mk(1275262484, -639559543),
    Caml_int64.mk(-1147274043, 1209011959),
    Caml_int64.mk(242558724, 1831626934)
  ]
];

var to_floats = [
  /* tuple */[
    Caml_int64.one,
    1
  ],
  /* tuple */[
    Caml_int64.mk(2, 0),
    2
  ],
  /* tuple */[
    Caml_int64.mk(4, 0),
    4
  ],
  /* tuple */[
    Caml_int64.mk(8, 0),
    8
  ],
  /* tuple */[
    Caml_int64.mk(16, 0),
    16
  ],
  /* tuple */[
    Caml_int64.mk(32, 0),
    32
  ],
  /* tuple */[
    Caml_int64.mk(64, 0),
    64
  ],
  /* tuple */[
    Caml_int64.mk(128, 0),
    128
  ],
  /* tuple */[
    Caml_int64.mk(256, 0),
    256
  ],
  /* tuple */[
    Caml_int64.mk(512, 0),
    512
  ],
  /* tuple */[
    Caml_int64.mk(1024, 0),
    1024
  ],
  /* tuple */[
    Caml_int64.mk(2048, 0),
    2048
  ],
  /* tuple */[
    Caml_int64.mk(4096, 0),
    4096
  ],
  /* tuple */[
    Caml_int64.mk(8192, 0),
    8192
  ],
  /* tuple */[
    Caml_int64.mk(16384, 0),
    16384
  ],
  /* tuple */[
    Caml_int64.mk(32768, 0),
    32768
  ],
  /* tuple */[
    Caml_int64.mk(65536, 0),
    65536
  ],
  /* tuple */[
    Caml_int64.mk(131072, 0),
    131072
  ],
  /* tuple */[
    Caml_int64.mk(262144, 0),
    262144
  ],
  /* tuple */[
    Caml_int64.mk(524288, 0),
    524288
  ],
  /* tuple */[
    Caml_int64.mk(1048576, 0),
    1048576
  ],
  /* tuple */[
    Caml_int64.mk(2097152, 0),
    2097152
  ],
  /* tuple */[
    Caml_int64.mk(4194304, 0),
    4194304
  ],
  /* tuple */[
    Caml_int64.mk(8388608, 0),
    8388608
  ],
  /* tuple */[
    Caml_int64.mk(16777216, 0),
    16777216
  ],
  /* tuple */[
    Caml_int64.mk(33554432, 0),
    33554432
  ],
  /* tuple */[
    Caml_int64.mk(67108864, 0),
    67108864
  ],
  /* tuple */[
    Caml_int64.mk(134217728, 0),
    134217728
  ],
  /* tuple */[
    Caml_int64.mk(268435456, 0),
    268435456
  ],
  /* tuple */[
    Caml_int64.mk(536870912, 0),
    536870912
  ],
  /* tuple */[
    Caml_int64.mk(1073741824, 0),
    1073741824
  ],
  /* tuple */[
    Caml_int64.mk(-2147483648, 0),
    2147483648
  ],
  /* tuple */[
    Caml_int64.mk(0, 1),
    4294967296
  ],
  /* tuple */[
    Caml_int64.mk(0, 2),
    8589934592
  ],
  /* tuple */[
    Caml_int64.mk(0, 4),
    17179869184
  ],
  /* tuple */[
    Caml_int64.mk(0, 8),
    34359738368
  ],
  /* tuple */[
    Caml_int64.mk(0, 16),
    68719476736
  ],
  /* tuple */[
    Caml_int64.mk(0, 32),
    137438953472
  ],
  /* tuple */[
    Caml_int64.mk(0, 64),
    274877906944
  ],
  /* tuple */[
    Caml_int64.mk(0, 128),
    549755813888
  ],
  /* tuple */[
    Caml_int64.mk(0, 256),
    1099511627776
  ],
  /* tuple */[
    Caml_int64.mk(0, 512),
    2199023255552
  ],
  /* tuple */[
    Caml_int64.mk(0, 1024),
    4398046511104
  ],
  /* tuple */[
    Caml_int64.mk(0, 2048),
    8796093022208
  ],
  /* tuple */[
    Caml_int64.mk(0, 4096),
    17592186044416
  ],
  /* tuple */[
    Caml_int64.mk(0, 8192),
    35184372088832
  ],
  /* tuple */[
    Caml_int64.mk(0, 16384),
    70368744177664
  ],
  /* tuple */[
    Caml_int64.mk(0, 32768),
    140737488355328
  ],
  /* tuple */[
    Caml_int64.mk(0, 65536),
    281474976710656
  ],
  /* tuple */[
    Caml_int64.mk(0, 131072),
    562949953421312
  ],
  /* tuple */[
    Caml_int64.mk(0, 262144),
    1125899906842624
  ],
  /* tuple */[
    Caml_int64.mk(0, 524288),
    2251799813685248
  ],
  /* tuple */[
    Caml_int64.mk(0, 1048576),
    4503599627370496
  ],
  /* tuple */[
    Caml_int64.mk(0, 2097152),
    9007199254740992
  ],
  /* tuple */[
    Caml_int64.mk(0, 4194304),
    18014398509481984
  ],
  /* tuple */[
    Caml_int64.mk(0, 8388608),
    36028797018963968
  ],
  /* tuple */[
    Caml_int64.mk(0, 16777216),
    72057594037927936
  ],
  /* tuple */[
    Caml_int64.mk(0, 33554432),
    144115188075855872
  ],
  /* tuple */[
    Caml_int64.mk(0, 67108864),
    288230376151711744
  ],
  /* tuple */[
    Caml_int64.mk(0, 134217728),
    576460752303423488
  ],
  /* tuple */[
    Caml_int64.mk(0, 268435456),
    1.15292150460684698e+18
  ],
  /* tuple */[
    Caml_int64.mk(0, 536870912),
    2.30584300921369395e+18
  ],
  /* tuple */[
    Caml_int64.mk(0, 1073741824),
    4.6116860184273879e+18
  ],
  /* tuple */[
    Caml_int64.min_int,
    -9.22337203685477581e+18
  ]
];

var check_complete_compare = [
  true,
  true,
  true,
  true,
  true,
  true,
  true,
  true
];

var of_float_pairs = [
  /* tuple */[
    6853066956871844,
    Caml_int64.mk(-40494940, 1595603)
  ],
  /* tuple */[
    -8507688874782117,
    Caml_int64.mk(1388466779, -1980851)
  ],
  /* tuple */[
    4083117349607451,
    Caml_int64.mk(-684517349, 950674)
  ],
  /* tuple */[
    -4860723193745655,
    Caml_int64.mk(-1330680055, -1131726)
  ],
  /* tuple */[
    7820020192255542,
    Caml_int64.mk(1437736502, 1820740)
  ],
  /* tuple */[
    -4908619721514532,
    Caml_int64.mk(-383163940, -1142878)
  ],
  /* tuple */[
    5.67685864687671e+15,
    Caml_int64.mk(-1491709402, 1321746)
  ],
  /* tuple */[
    -703696191048023,
    Caml_int64.mk(-159336791, -163843)
  ],
  /* tuple */[
    1123586534990153.88,
    Caml_int64.mk(1615520073, 261605)
  ],
  /* tuple */[
    -4.29886533981922e+15,
    Caml_int64.mk(1786485548, -1000908)
  ],
  /* tuple */[
    2.43885138012066e+15,
    Caml_int64.mk(1445727316, 567839)
  ],
  /* tuple */[
    -8011538689499494,
    Caml_int64.mk(1246682778, -1865332)
  ],
  /* tuple */[
    2710072285421155,
    Caml_int64.mk(-538747293, 630987)
  ],
  /* tuple */[
    -2541457347159789.5,
    Caml_int64.mk(-644065005, -591730)
  ],
  /* tuple */[
    5012932793576708,
    Caml_int64.mk(1584508164, 1167164)
  ],
  /* tuple */[
    -943066847413899.125,
    Caml_int64.mk(596605301, -219575)
  ],
  /* tuple */[
    5440257518642004,
    Caml_int64.mk(-1461542060, 1266658)
  ],
  /* tuple */[
    -7750676773453898,
    Caml_int64.mk(-265928778, -1804596)
  ],
  /* tuple */[
    8911999221747713,
    Caml_int64.mk(-2082877439, 2074986)
  ],
  /* tuple */[
    -1443906702582204.25,
    Caml_int64.mk(1172790852, -336186)
  ],
  /* tuple */[
    659345820712164.875,
    Caml_int64.mk(-378700572, 153515)
  ],
  /* tuple */[
    -3284023713149006.5,
    Caml_int64.mk(-1524314190, -764622)
  ],
  /* tuple */[
    5062818438579988,
    Caml_int64.mk(1184368404, 1178779)
  ],
  /* tuple */[
    -8904450004162331,
    Caml_int64.mk(747956453, -2073229)
  ],
  /* tuple */[
    848261089308786,
    Caml_int64.mk(753381490, 197501)
  ],
  /* tuple */[
    -6376579516657391,
    Caml_int64.mk(-486076143, -1484664)
  ],
  /* tuple */[
    1337907592605664.25,
    Caml_int64.mk(-489902112, 311505)
  ],
  /* tuple */[
    -8.54733738833896e+15,
    Caml_int64.mk(-281980688, -1990083)
  ],
  /* tuple */[
    2345417644172927,
    Caml_int64.mk(428336767, 546085)
  ],
  /* tuple */[
    -2587460670129294.5,
    Caml_int64.mk(-572327054, -602441)
  ],
  /* tuple */[
    4580431718597436,
    Caml_int64.mk(-578731204, 1066464)
  ],
  /* tuple */[
    -1460576044874256.25,
    Caml_int64.mk(598574576, -340067)
  ],
  /* tuple */[
    3403657978343579.5,
    Caml_int64.mk(-524521317, 792475)
  ],
  /* tuple */[
    -7.89068917321888e+15,
    Caml_int64.mk(-1026811456, -1837195)
  ],
  /* tuple */[
    1683098350604788.5,
    Caml_int64.mk(-548449804, 391876)
  ],
  /* tuple */[
    -3966538891560174.5,
    Caml_int64.mk(845249298, -923532)
  ],
  /* tuple */[
    6726025288963652,
    Caml_int64.mk(-870754748, 1566024)
  ],
  /* tuple */[
    -4790410747298403,
    Caml_int64.mk(-1793835619, -1115355)
  ],
  /* tuple */[
    1985858071337706.25,
    Caml_int64.mk(-1662346518, 462368)
  ],
  /* tuple */[
    -5281733497873409,
    Caml_int64.mk(-1760584705, -1229750)
  ]
];

var simple_divs = [
  /* tuple */[
    Caml_int64.mk(6, 0),
    Caml_int64.mk(3, 0),
    Caml_int64.mk(2, 0),
    Caml_int64.zero
  ],
  /* tuple */[
    Caml_int64.mk(120, 0),
    Caml_int64.mk(11, 0),
    Caml_int64.mk(10, 0),
    Caml_int64.mk(10, 0)
  ],
  /* tuple */[
    Caml_int64.min_int,
    Caml_int64.mk(2, 0),
    Caml_int64.mk(0, -1073741824),
    Caml_int64.zero
  ],
  /* tuple */[
    Caml_int64.mk(-163100511, 1112580415),
    Caml_int64.mk(-1689560617, 2013350321),
    Caml_int64.zero,
    Caml_int64.mk(-163100511, 1112580415)
  ],
  /* tuple */[
    Caml_int64.mk(1414064366, 983582600),
    Caml_int64.mk(720592487, 1027627185),
    Caml_int64.zero,
    Caml_int64.mk(1414064366, 983582600)
  ],
  /* tuple */[
    Caml_int64.mk(-244174718, 707587463),
    Caml_int64.mk(-1441985079, 457824592),
    Caml_int64.one,
    Caml_int64.mk(1197810361, 249762871)
  ],
  /* tuple */[
    Caml_int64.mk(-452010802, 3696312),
    Caml_int64.mk(1932508180, 303263066),
    Caml_int64.zero,
    Caml_int64.mk(-452010802, 3696312)
  ],
  /* tuple */[
    Caml_int64.mk(530108890, 1998955230),
    Caml_int64.mk(-1300251594, 1876081138),
    Caml_int64.one,
    Caml_int64.mk(1830360484, 122874091)
  ],
  /* tuple */[
    Caml_int64.mk(-1842975846, 1123314058),
    Caml_int64.mk(-1636954134, 1077511003),
    Caml_int64.one,
    Caml_int64.mk(-206021712, 45803054)
  ],
  /* tuple */[
    Caml_int64.mk(-474132284, 772515434),
    Caml_int64.mk(435807891, 1485983210),
    Caml_int64.zero,
    Caml_int64.mk(-474132284, 772515434)
  ],
  /* tuple */[
    Caml_int64.mk(1357533220, 1437309318),
    Caml_int64.mk(541080542, 1141241105),
    Caml_int64.one,
    Caml_int64.mk(816452678, 296068213)
  ],
  /* tuple */[
    Caml_int64.mk(407118687, 1559319564),
    Caml_int64.mk(-280613636, 211092740),
    Caml_int64.mk(7, 0),
    Caml_int64.mk(-1923553157, 81670377)
  ],
  /* tuple */[
    Caml_int64.mk(1054256000, 897058469),
    Caml_int64.mk(661312616, 57853316),
    Caml_int64.mk(15, 0),
    Caml_int64.mk(-275498648, 29258726)
  ],
  /* tuple */[
    Caml_int64.mk(-1265124412, 1771820319),
    Caml_int64.mk(222584391, 1113086871),
    Caml_int64.one,
    Caml_int64.mk(-1487708803, 658733448)
  ],
  /* tuple */[
    Caml_int64.mk(-1564422518, 1699471447),
    Caml_int64.mk(-398393386, 1090632987),
    Caml_int64.one,
    Caml_int64.mk(-1166029132, 608838459)
  ],
  /* tuple */[
    Caml_int64.mk(837406327, 1300122432),
    Caml_int64.mk(-433706886, 349961722),
    Caml_int64.mk(3, 0),
    Caml_int64.mk(2138526985, 250237263)
  ],
  /* tuple */[
    Caml_int64.mk(681013979, 1844919629),
    Caml_int64.mk(-400929258, 141654602),
    Caml_int64.mk(13, 0),
    Caml_int64.mk(1598127037, 3409791)
  ],
  /* tuple */[
    Caml_int64.mk(-1067617897, 876561860),
    Caml_int64.mk(1118648885, 1635137811),
    Caml_int64.zero,
    Caml_int64.mk(-1067617897, 876561860)
  ],
  /* tuple */[
    Caml_int64.mk(1188873919, 1444234022),
    Caml_int64.mk(-1845904707, 1506775353),
    Caml_int64.zero,
    Caml_int64.mk(1188873919, 1444234022)
  ],
  /* tuple */[
    Caml_int64.mk(-1338985492, 216713133),
    Caml_int64.mk(478170254, 840978153),
    Caml_int64.zero,
    Caml_int64.mk(-1338985492, 216713133)
  ],
  /* tuple */[
    Caml_int64.mk(-1951260634, 311131250),
    Caml_int64.mk(1397060596, 1432032601),
    Caml_int64.zero,
    Caml_int64.mk(-1951260634, 311131250)
  ],
  /* tuple */[
    Caml_int64.mk(-2040192977, 702252958),
    Caml_int64.mk(1983179783, 333531300),
    Caml_int64.mk(2, 0),
    Caml_int64.mk(-1711585247, 35190357)
  ],
  /* tuple */[
    Caml_int64.mk(1357843095, 222120797),
    Caml_int64.mk(-1744534264, 1409050141),
    Caml_int64.zero,
    Caml_int64.mk(1357843095, 222120797)
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

var to_string = [/* tuple */[
    Caml_int64.zero,
    "0"
  ]];

var int64_compare_tests = [
  /* tuple */[
    Caml_int64.one,
    Caml_int64.mk(2, 0),
    -1
  ],
  /* tuple */[
    Caml_int64.mk(2, 0),
    Caml_int64.one,
    1
  ],
  /* tuple */[
    Caml_int64.mk(2, 0),
    Caml_int64.one,
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
                                      Caml_int64.to_string(a)
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
                                                  Caml_int64.zero,
                                                  Caml_int64.zero
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "div_rem_1",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    Caml_int64.neg_one,
                                                    Caml_int64.neg_one
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
