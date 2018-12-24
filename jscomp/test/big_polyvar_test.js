'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var jsMapperConstantArray = /* array */[
  /* tuple */[
    -384420853,
    "variant0"
  ],
  /* tuple */[
    -384420852,
    "variant1"
  ],
  /* tuple */[
    -384420851,
    "variant2"
  ],
  /* tuple */[
    -384420850,
    "variant3"
  ],
  /* tuple */[
    -384420849,
    "variant4"
  ],
  /* tuple */[
    -384420848,
    "variant5"
  ],
  /* tuple */[
    -384420847,
    "variant6"
  ],
  /* tuple */[
    -384420846,
    "variant7"
  ],
  /* tuple */[
    -384420845,
    "variant8"
  ],
  /* tuple */[
    -384420844,
    "variant9"
  ],
  /* tuple */[
    34896140,
    "variant100"
  ],
  /* tuple */[
    34896141,
    "variant101"
  ],
  /* tuple */[
    34896142,
    "variant102"
  ],
  /* tuple */[
    34896143,
    "variant103"
  ],
  /* tuple */[
    34896144,
    "variant104"
  ],
  /* tuple */[
    34896145,
    "variant105"
  ],
  /* tuple */[
    34896146,
    "variant106"
  ],
  /* tuple */[
    34896147,
    "variant107"
  ],
  /* tuple */[
    34896148,
    "variant108"
  ],
  /* tuple */[
    34896149,
    "variant109"
  ],
  /* tuple */[
    34896363,
    "variant110"
  ],
  /* tuple */[
    34896364,
    "variant111"
  ],
  /* tuple */[
    34896365,
    "variant112"
  ],
  /* tuple */[
    34896366,
    "variant113"
  ],
  /* tuple */[
    34896367,
    "variant114"
  ],
  /* tuple */[
    34896368,
    "variant115"
  ],
  /* tuple */[
    34896369,
    "variant116"
  ],
  /* tuple */[
    34896370,
    "variant117"
  ],
  /* tuple */[
    34896371,
    "variant118"
  ],
  /* tuple */[
    34896372,
    "variant119"
  ],
  /* tuple */[
    34896586,
    "variant120"
  ],
  /* tuple */[
    34896587,
    "variant121"
  ],
  /* tuple */[
    34896588,
    "variant122"
  ],
  /* tuple */[
    34896589,
    "variant123"
  ],
  /* tuple */[
    34896590,
    "variant124"
  ],
  /* tuple */[
    34896591,
    "variant125"
  ],
  /* tuple */[
    34896592,
    "variant126"
  ],
  /* tuple */[
    34896593,
    "variant127"
  ],
  /* tuple */[
    34896594,
    "variant128"
  ],
  /* tuple */[
    34896595,
    "variant129"
  ],
  /* tuple */[
    34896809,
    "variant130"
  ],
  /* tuple */[
    34896810,
    "variant131"
  ],
  /* tuple */[
    34896811,
    "variant132"
  ],
  /* tuple */[
    34896812,
    "variant133"
  ],
  /* tuple */[
    34896813,
    "variant134"
  ],
  /* tuple */[
    34896814,
    "variant135"
  ],
  /* tuple */[
    34896815,
    "variant136"
  ],
  /* tuple */[
    34896816,
    "variant137"
  ],
  /* tuple */[
    34896817,
    "variant138"
  ],
  /* tuple */[
    34896818,
    "variant139"
  ],
  /* tuple */[
    34897032,
    "variant140"
  ],
  /* tuple */[
    34897033,
    "variant141"
  ],
  /* tuple */[
    34897034,
    "variant142"
  ],
  /* tuple */[
    34897035,
    "variant143"
  ],
  /* tuple */[
    34897036,
    "variant144"
  ],
  /* tuple */[
    34897037,
    "variant145"
  ],
  /* tuple */[
    34897038,
    "variant146"
  ],
  /* tuple */[
    34897039,
    "variant147"
  ],
  /* tuple */[
    34897040,
    "variant148"
  ],
  /* tuple */[
    34897041,
    "variant149"
  ],
  /* tuple */[
    34897255,
    "variant150"
  ],
  /* tuple */[
    34897256,
    "variant151"
  ],
  /* tuple */[
    34897257,
    "variant152"
  ],
  /* tuple */[
    34897258,
    "variant153"
  ],
  /* tuple */[
    34897259,
    "variant154"
  ],
  /* tuple */[
    34897260,
    "variant155"
  ],
  /* tuple */[
    34897261,
    "variant156"
  ],
  /* tuple */[
    34897262,
    "variant157"
  ],
  /* tuple */[
    34897263,
    "variant158"
  ],
  /* tuple */[
    34897264,
    "variant159"
  ],
  /* tuple */[
    34897478,
    "variant160"
  ],
  /* tuple */[
    34897479,
    "variant161"
  ],
  /* tuple */[
    34897480,
    "variant162"
  ],
  /* tuple */[
    34897481,
    "variant163"
  ],
  /* tuple */[
    34897482,
    "variant164"
  ],
  /* tuple */[
    34897483,
    "variant165"
  ],
  /* tuple */[
    34897484,
    "variant166"
  ],
  /* tuple */[
    34897485,
    "variant167"
  ],
  /* tuple */[
    34897486,
    "variant168"
  ],
  /* tuple */[
    34897487,
    "variant169"
  ],
  /* tuple */[
    34897701,
    "variant170"
  ],
  /* tuple */[
    34897702,
    "variant171"
  ],
  /* tuple */[
    34897703,
    "variant172"
  ],
  /* tuple */[
    34897704,
    "variant173"
  ],
  /* tuple */[
    34897705,
    "variant174"
  ],
  /* tuple */[
    34897706,
    "variant175"
  ],
  /* tuple */[
    34897707,
    "variant176"
  ],
  /* tuple */[
    34897708,
    "variant177"
  ],
  /* tuple */[
    34897709,
    "variant178"
  ],
  /* tuple */[
    34897710,
    "variant179"
  ],
  /* tuple */[
    34897924,
    "variant180"
  ],
  /* tuple */[
    34897925,
    "variant181"
  ],
  /* tuple */[
    34897926,
    "variant182"
  ],
  /* tuple */[
    34897927,
    "variant183"
  ],
  /* tuple */[
    34897928,
    "variant184"
  ],
  /* tuple */[
    34897929,
    "variant185"
  ],
  /* tuple */[
    34897930,
    "variant186"
  ],
  /* tuple */[
    34897931,
    "variant187"
  ],
  /* tuple */[
    34897932,
    "variant188"
  ],
  /* tuple */[
    34897933,
    "variant189"
  ],
  /* tuple */[
    34898147,
    "variant190"
  ],
  /* tuple */[
    34898148,
    "variant191"
  ],
  /* tuple */[
    34898149,
    "variant192"
  ],
  /* tuple */[
    34898150,
    "variant193"
  ],
  /* tuple */[
    34898151,
    "variant194"
  ],
  /* tuple */[
    34898152,
    "variant195"
  ],
  /* tuple */[
    34898153,
    "variant196"
  ],
  /* tuple */[
    34898154,
    "variant197"
  ],
  /* tuple */[
    34898155,
    "variant198"
  ],
  /* tuple */[
    34898156,
    "variant199"
  ],
  /* tuple */[
    34945869,
    "variant200"
  ],
  /* tuple */[
    34945870,
    "variant201"
  ],
  /* tuple */[
    34945871,
    "variant202"
  ],
  /* tuple */[
    34945872,
    "variant203"
  ],
  /* tuple */[
    34945873,
    "variant204"
  ],
  /* tuple */[
    34945874,
    "variant205"
  ],
  /* tuple */[
    34945875,
    "variant206"
  ],
  /* tuple */[
    34945876,
    "variant207"
  ],
  /* tuple */[
    34945877,
    "variant208"
  ],
  /* tuple */[
    34945878,
    "variant209"
  ],
  /* tuple */[
    34946092,
    "variant210"
  ],
  /* tuple */[
    34946093,
    "variant211"
  ],
  /* tuple */[
    34946094,
    "variant212"
  ],
  /* tuple */[
    34946095,
    "variant213"
  ],
  /* tuple */[
    34946096,
    "variant214"
  ],
  /* tuple */[
    34946097,
    "variant215"
  ],
  /* tuple */[
    34946098,
    "variant216"
  ],
  /* tuple */[
    34946099,
    "variant217"
  ],
  /* tuple */[
    34946100,
    "variant218"
  ],
  /* tuple */[
    34946101,
    "variant219"
  ],
  /* tuple */[
    34946315,
    "variant220"
  ],
  /* tuple */[
    34946316,
    "variant221"
  ],
  /* tuple */[
    34946317,
    "variant222"
  ],
  /* tuple */[
    34946318,
    "variant223"
  ],
  /* tuple */[
    34946319,
    "variant224"
  ],
  /* tuple */[
    34946320,
    "variant225"
  ],
  /* tuple */[
    34946321,
    "variant226"
  ],
  /* tuple */[
    34946322,
    "variant227"
  ],
  /* tuple */[
    34946323,
    "variant228"
  ],
  /* tuple */[
    34946324,
    "variant229"
  ],
  /* tuple */[
    34946538,
    "variant230"
  ],
  /* tuple */[
    34946539,
    "variant231"
  ],
  /* tuple */[
    34946540,
    "variant232"
  ],
  /* tuple */[
    34946541,
    "variant233"
  ],
  /* tuple */[
    34946542,
    "variant234"
  ],
  /* tuple */[
    34946543,
    "variant235"
  ],
  /* tuple */[
    34946544,
    "variant236"
  ],
  /* tuple */[
    34946545,
    "variant237"
  ],
  /* tuple */[
    34946546,
    "variant238"
  ],
  /* tuple */[
    34946547,
    "variant239"
  ],
  /* tuple */[
    34946761,
    "variant240"
  ],
  /* tuple */[
    34946762,
    "variant241"
  ],
  /* tuple */[
    34946763,
    "variant242"
  ],
  /* tuple */[
    34946764,
    "variant243"
  ],
  /* tuple */[
    34946765,
    "variant244"
  ],
  /* tuple */[
    34946766,
    "variant245"
  ],
  /* tuple */[
    34946767,
    "variant246"
  ],
  /* tuple */[
    34946768,
    "variant247"
  ],
  /* tuple */[
    34946769,
    "variant248"
  ],
  /* tuple */[
    34946770,
    "variant249"
  ],
  /* tuple */[
    34946984,
    "variant250"
  ],
  /* tuple */[
    34946985,
    "variant251"
  ],
  /* tuple */[
    34946986,
    "variant252"
  ],
  /* tuple */[
    34946987,
    "variant253"
  ],
  /* tuple */[
    34946988,
    "variant254"
  ],
  /* tuple */[
    34946989,
    "variant255"
  ],
  /* tuple */[
    34946990,
    "variant256"
  ],
  /* tuple */[
    34946991,
    "variant257"
  ],
  /* tuple */[
    34946992,
    "variant258"
  ],
  /* tuple */[
    34946993,
    "variant259"
  ],
  /* tuple */[
    34947207,
    "variant260"
  ],
  /* tuple */[
    34947208,
    "variant261"
  ],
  /* tuple */[
    34947209,
    "variant262"
  ],
  /* tuple */[
    34947210,
    "variant263"
  ],
  /* tuple */[
    34947211,
    "variant264"
  ],
  /* tuple */[
    34947212,
    "variant265"
  ],
  /* tuple */[
    34947213,
    "variant266"
  ],
  /* tuple */[
    34947214,
    "variant267"
  ],
  /* tuple */[
    34947215,
    "variant268"
  ],
  /* tuple */[
    34947216,
    "variant269"
  ],
  /* tuple */[
    34947430,
    "variant270"
  ],
  /* tuple */[
    34947431,
    "variant271"
  ],
  /* tuple */[
    34947432,
    "variant272"
  ],
  /* tuple */[
    34947433,
    "variant273"
  ],
  /* tuple */[
    34947434,
    "variant274"
  ],
  /* tuple */[
    34947435,
    "variant275"
  ],
  /* tuple */[
    34947436,
    "variant276"
  ],
  /* tuple */[
    34947437,
    "variant277"
  ],
  /* tuple */[
    34947438,
    "variant278"
  ],
  /* tuple */[
    34947439,
    "variant279"
  ],
  /* tuple */[
    34947653,
    "variant280"
  ],
  /* tuple */[
    34947654,
    "variant281"
  ],
  /* tuple */[
    34947655,
    "variant282"
  ],
  /* tuple */[
    34947656,
    "variant283"
  ],
  /* tuple */[
    34947657,
    "variant284"
  ],
  /* tuple */[
    34947658,
    "variant285"
  ],
  /* tuple */[
    34947659,
    "variant286"
  ],
  /* tuple */[
    34947660,
    "variant287"
  ],
  /* tuple */[
    34947661,
    "variant288"
  ],
  /* tuple */[
    34947662,
    "variant289"
  ],
  /* tuple */[
    34947876,
    "variant290"
  ],
  /* tuple */[
    34947877,
    "variant291"
  ],
  /* tuple */[
    34947878,
    "variant292"
  ],
  /* tuple */[
    34947879,
    "variant293"
  ],
  /* tuple */[
    34947880,
    "variant294"
  ],
  /* tuple */[
    34947881,
    "variant295"
  ],
  /* tuple */[
    34947882,
    "variant296"
  ],
  /* tuple */[
    34947883,
    "variant297"
  ],
  /* tuple */[
    34947884,
    "variant298"
  ],
  /* tuple */[
    34947885,
    "variant299"
  ],
  /* tuple */[
    173495972,
    "variant10"
  ],
  /* tuple */[
    173495973,
    "variant11"
  ],
  /* tuple */[
    173495974,
    "variant12"
  ],
  /* tuple */[
    173495975,
    "variant13"
  ],
  /* tuple */[
    173495976,
    "variant14"
  ],
  /* tuple */[
    173495977,
    "variant15"
  ],
  /* tuple */[
    173495978,
    "variant16"
  ],
  /* tuple */[
    173495979,
    "variant17"
  ],
  /* tuple */[
    173495980,
    "variant18"
  ],
  /* tuple */[
    173495981,
    "variant19"
  ],
  /* tuple */[
    173496195,
    "variant20"
  ],
  /* tuple */[
    173496196,
    "variant21"
  ],
  /* tuple */[
    173496197,
    "variant22"
  ],
  /* tuple */[
    173496198,
    "variant23"
  ],
  /* tuple */[
    173496199,
    "variant24"
  ],
  /* tuple */[
    173496200,
    "variant25"
  ],
  /* tuple */[
    173496201,
    "variant26"
  ],
  /* tuple */[
    173496202,
    "variant27"
  ],
  /* tuple */[
    173496203,
    "variant28"
  ],
  /* tuple */[
    173496204,
    "variant29"
  ],
  /* tuple */[
    173496418,
    "variant30"
  ],
  /* tuple */[
    173496419,
    "variant31"
  ],
  /* tuple */[
    173496420,
    "variant32"
  ],
  /* tuple */[
    173496421,
    "variant33"
  ],
  /* tuple */[
    173496422,
    "variant34"
  ],
  /* tuple */[
    173496423,
    "variant35"
  ],
  /* tuple */[
    173496424,
    "variant36"
  ],
  /* tuple */[
    173496425,
    "variant37"
  ],
  /* tuple */[
    173496426,
    "variant38"
  ],
  /* tuple */[
    173496427,
    "variant39"
  ],
  /* tuple */[
    173496641,
    "variant40"
  ],
  /* tuple */[
    173496642,
    "variant41"
  ],
  /* tuple */[
    173496643,
    "variant42"
  ],
  /* tuple */[
    173496644,
    "variant43"
  ],
  /* tuple */[
    173496645,
    "variant44"
  ],
  /* tuple */[
    173496646,
    "variant45"
  ],
  /* tuple */[
    173496647,
    "variant46"
  ],
  /* tuple */[
    173496648,
    "variant47"
  ],
  /* tuple */[
    173496649,
    "variant48"
  ],
  /* tuple */[
    173496650,
    "variant49"
  ],
  /* tuple */[
    173496864,
    "variant50"
  ],
  /* tuple */[
    173496865,
    "variant51"
  ],
  /* tuple */[
    173496866,
    "variant52"
  ],
  /* tuple */[
    173496867,
    "variant53"
  ],
  /* tuple */[
    173496868,
    "variant54"
  ],
  /* tuple */[
    173496869,
    "variant55"
  ],
  /* tuple */[
    173496870,
    "variant56"
  ],
  /* tuple */[
    173496871,
    "variant57"
  ],
  /* tuple */[
    173496872,
    "variant58"
  ],
  /* tuple */[
    173496873,
    "variant59"
  ],
  /* tuple */[
    173497087,
    "variant60"
  ],
  /* tuple */[
    173497088,
    "variant61"
  ],
  /* tuple */[
    173497089,
    "variant62"
  ],
  /* tuple */[
    173497090,
    "variant63"
  ],
  /* tuple */[
    173497091,
    "variant64"
  ],
  /* tuple */[
    173497092,
    "variant65"
  ],
  /* tuple */[
    173497093,
    "variant66"
  ],
  /* tuple */[
    173497094,
    "variant67"
  ],
  /* tuple */[
    173497095,
    "variant68"
  ],
  /* tuple */[
    173497096,
    "variant69"
  ],
  /* tuple */[
    173497310,
    "variant70"
  ],
  /* tuple */[
    173497311,
    "variant71"
  ],
  /* tuple */[
    173497312,
    "variant72"
  ],
  /* tuple */[
    173497313,
    "variant73"
  ],
  /* tuple */[
    173497314,
    "variant74"
  ],
  /* tuple */[
    173497315,
    "variant75"
  ],
  /* tuple */[
    173497316,
    "variant76"
  ],
  /* tuple */[
    173497317,
    "variant77"
  ],
  /* tuple */[
    173497318,
    "variant78"
  ],
  /* tuple */[
    173497319,
    "variant79"
  ],
  /* tuple */[
    173497533,
    "variant80"
  ],
  /* tuple */[
    173497534,
    "variant81"
  ],
  /* tuple */[
    173497535,
    "variant82"
  ],
  /* tuple */[
    173497536,
    "variant83"
  ],
  /* tuple */[
    173497537,
    "variant84"
  ],
  /* tuple */[
    173497538,
    "variant85"
  ],
  /* tuple */[
    173497539,
    "variant86"
  ],
  /* tuple */[
    173497540,
    "variant87"
  ],
  /* tuple */[
    173497541,
    "variant88"
  ],
  /* tuple */[
    173497542,
    "variant89"
  ],
  /* tuple */[
    173497756,
    "variant90"
  ],
  /* tuple */[
    173497757,
    "variant91"
  ],
  /* tuple */[
    173497758,
    "variant92"
  ],
  /* tuple */[
    173497759,
    "variant93"
  ],
  /* tuple */[
    173497760,
    "variant94"
  ],
  /* tuple */[
    173497761,
    "variant95"
  ],
  /* tuple */[
    173497762,
    "variant96"
  ],
  /* tuple */[
    173497763,
    "variant97"
  ],
  /* tuple */[
    173497764,
    "variant98"
  ],
  /* tuple */[
    173497765,
    "variant99"
  ]
];

function tToJs(param) {
  return Js_mapperRt.binarySearch(300, param, jsMapperConstantArray);
}

function tFromJs(param) {
  return Js_mapperRt.revSearch(300, jsMapperConstantArray, param);
}

function eq(x, y) {
  if (x !== undefined) {
    if (y !== undefined) {
      return x === y;
    } else {
      return false;
    }
  } else {
    return y === undefined;
  }
}

if (tToJs(/* variant0 */-384420853) !== "variant0") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          310,
          2
        ]
      ];
}

if (tToJs(/* variant1 */-384420852) !== "variant1") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          311,
          2
        ]
      ];
}

if (tToJs(/* variant2 */-384420851) !== "variant2") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          312,
          2
        ]
      ];
}

if (tToJs(/* variant3 */-384420850) !== "variant3") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          313,
          2
        ]
      ];
}

if (tToJs(/* variant4 */-384420849) !== "variant4") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          314,
          2
        ]
      ];
}

if (tToJs(/* variant5 */-384420848) !== "variant5") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          315,
          2
        ]
      ];
}

if (tToJs(/* variant6 */-384420847) !== "variant6") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          316,
          2
        ]
      ];
}

if (tToJs(/* variant7 */-384420846) !== "variant7") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          317,
          2
        ]
      ];
}

if (tToJs(/* variant8 */-384420845) !== "variant8") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          318,
          2
        ]
      ];
}

if (tToJs(/* variant9 */-384420844) !== "variant9") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          319,
          2
        ]
      ];
}

if (tToJs(/* variant10 */173495972) !== "variant10") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          320,
          2
        ]
      ];
}

if (tToJs(/* variant11 */173495973) !== "variant11") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          321,
          2
        ]
      ];
}

if (tToJs(/* variant12 */173495974) !== "variant12") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          322,
          2
        ]
      ];
}

if (tToJs(/* variant13 */173495975) !== "variant13") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          323,
          2
        ]
      ];
}

if (tToJs(/* variant14 */173495976) !== "variant14") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          324,
          2
        ]
      ];
}

if (tToJs(/* variant15 */173495977) !== "variant15") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          325,
          2
        ]
      ];
}

if (tToJs(/* variant16 */173495978) !== "variant16") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          326,
          2
        ]
      ];
}

if (tToJs(/* variant17 */173495979) !== "variant17") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          327,
          2
        ]
      ];
}

if (tToJs(/* variant18 */173495980) !== "variant18") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          328,
          2
        ]
      ];
}

if (tToJs(/* variant19 */173495981) !== "variant19") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          329,
          2
        ]
      ];
}

if (tToJs(/* variant20 */173496195) !== "variant20") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          330,
          2
        ]
      ];
}

if (tToJs(/* variant21 */173496196) !== "variant21") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          331,
          2
        ]
      ];
}

if (tToJs(/* variant22 */173496197) !== "variant22") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          332,
          2
        ]
      ];
}

if (tToJs(/* variant23 */173496198) !== "variant23") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          333,
          2
        ]
      ];
}

if (tToJs(/* variant24 */173496199) !== "variant24") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          334,
          2
        ]
      ];
}

if (tToJs(/* variant25 */173496200) !== "variant25") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          335,
          2
        ]
      ];
}

if (tToJs(/* variant26 */173496201) !== "variant26") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          336,
          2
        ]
      ];
}

if (tToJs(/* variant27 */173496202) !== "variant27") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          337,
          2
        ]
      ];
}

if (tToJs(/* variant28 */173496203) !== "variant28") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          338,
          2
        ]
      ];
}

if (tToJs(/* variant29 */173496204) !== "variant29") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          339,
          2
        ]
      ];
}

if (tToJs(/* variant30 */173496418) !== "variant30") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          340,
          2
        ]
      ];
}

if (tToJs(/* variant31 */173496419) !== "variant31") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          341,
          2
        ]
      ];
}

if (tToJs(/* variant32 */173496420) !== "variant32") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          342,
          2
        ]
      ];
}

if (tToJs(/* variant33 */173496421) !== "variant33") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          343,
          2
        ]
      ];
}

if (tToJs(/* variant34 */173496422) !== "variant34") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          344,
          2
        ]
      ];
}

if (tToJs(/* variant35 */173496423) !== "variant35") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          345,
          2
        ]
      ];
}

if (tToJs(/* variant36 */173496424) !== "variant36") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          346,
          2
        ]
      ];
}

if (tToJs(/* variant37 */173496425) !== "variant37") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          347,
          2
        ]
      ];
}

if (tToJs(/* variant38 */173496426) !== "variant38") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          348,
          2
        ]
      ];
}

if (tToJs(/* variant39 */173496427) !== "variant39") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          349,
          2
        ]
      ];
}

if (tToJs(/* variant40 */173496641) !== "variant40") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          350,
          2
        ]
      ];
}

if (tToJs(/* variant41 */173496642) !== "variant41") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          351,
          2
        ]
      ];
}

if (tToJs(/* variant42 */173496643) !== "variant42") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          352,
          2
        ]
      ];
}

if (tToJs(/* variant43 */173496644) !== "variant43") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          353,
          2
        ]
      ];
}

if (tToJs(/* variant44 */173496645) !== "variant44") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          354,
          2
        ]
      ];
}

if (tToJs(/* variant45 */173496646) !== "variant45") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          355,
          2
        ]
      ];
}

if (tToJs(/* variant46 */173496647) !== "variant46") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          356,
          2
        ]
      ];
}

if (tToJs(/* variant47 */173496648) !== "variant47") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          357,
          2
        ]
      ];
}

if (tToJs(/* variant48 */173496649) !== "variant48") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          358,
          2
        ]
      ];
}

if (tToJs(/* variant49 */173496650) !== "variant49") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          359,
          2
        ]
      ];
}

if (tToJs(/* variant50 */173496864) !== "variant50") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          360,
          2
        ]
      ];
}

if (tToJs(/* variant51 */173496865) !== "variant51") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          361,
          2
        ]
      ];
}

if (tToJs(/* variant52 */173496866) !== "variant52") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          362,
          2
        ]
      ];
}

if (tToJs(/* variant53 */173496867) !== "variant53") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          363,
          2
        ]
      ];
}

if (tToJs(/* variant54 */173496868) !== "variant54") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          364,
          2
        ]
      ];
}

if (tToJs(/* variant55 */173496869) !== "variant55") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          365,
          2
        ]
      ];
}

if (tToJs(/* variant56 */173496870) !== "variant56") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          366,
          2
        ]
      ];
}

if (tToJs(/* variant57 */173496871) !== "variant57") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          367,
          2
        ]
      ];
}

if (tToJs(/* variant58 */173496872) !== "variant58") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          368,
          2
        ]
      ];
}

if (tToJs(/* variant59 */173496873) !== "variant59") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          369,
          2
        ]
      ];
}

if (tToJs(/* variant60 */173497087) !== "variant60") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          370,
          2
        ]
      ];
}

if (tToJs(/* variant61 */173497088) !== "variant61") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          371,
          2
        ]
      ];
}

if (tToJs(/* variant62 */173497089) !== "variant62") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          372,
          2
        ]
      ];
}

if (tToJs(/* variant63 */173497090) !== "variant63") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          373,
          2
        ]
      ];
}

if (tToJs(/* variant64 */173497091) !== "variant64") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          374,
          2
        ]
      ];
}

if (tToJs(/* variant65 */173497092) !== "variant65") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          375,
          2
        ]
      ];
}

if (tToJs(/* variant66 */173497093) !== "variant66") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          376,
          2
        ]
      ];
}

if (tToJs(/* variant67 */173497094) !== "variant67") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          377,
          2
        ]
      ];
}

if (tToJs(/* variant68 */173497095) !== "variant68") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          378,
          2
        ]
      ];
}

if (tToJs(/* variant69 */173497096) !== "variant69") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          379,
          2
        ]
      ];
}

if (tToJs(/* variant70 */173497310) !== "variant70") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          380,
          2
        ]
      ];
}

if (tToJs(/* variant71 */173497311) !== "variant71") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          381,
          2
        ]
      ];
}

if (tToJs(/* variant72 */173497312) !== "variant72") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          382,
          2
        ]
      ];
}

if (tToJs(/* variant73 */173497313) !== "variant73") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          383,
          2
        ]
      ];
}

if (tToJs(/* variant74 */173497314) !== "variant74") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          384,
          2
        ]
      ];
}

if (tToJs(/* variant75 */173497315) !== "variant75") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          385,
          2
        ]
      ];
}

if (tToJs(/* variant76 */173497316) !== "variant76") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          386,
          2
        ]
      ];
}

if (tToJs(/* variant77 */173497317) !== "variant77") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          387,
          2
        ]
      ];
}

if (tToJs(/* variant78 */173497318) !== "variant78") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          388,
          2
        ]
      ];
}

if (tToJs(/* variant79 */173497319) !== "variant79") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          389,
          2
        ]
      ];
}

if (tToJs(/* variant80 */173497533) !== "variant80") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          390,
          2
        ]
      ];
}

if (tToJs(/* variant81 */173497534) !== "variant81") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          391,
          2
        ]
      ];
}

if (tToJs(/* variant82 */173497535) !== "variant82") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          392,
          2
        ]
      ];
}

if (tToJs(/* variant83 */173497536) !== "variant83") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          393,
          2
        ]
      ];
}

if (tToJs(/* variant84 */173497537) !== "variant84") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          394,
          2
        ]
      ];
}

if (tToJs(/* variant85 */173497538) !== "variant85") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          395,
          2
        ]
      ];
}

if (tToJs(/* variant86 */173497539) !== "variant86") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          396,
          2
        ]
      ];
}

if (tToJs(/* variant87 */173497540) !== "variant87") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          397,
          2
        ]
      ];
}

if (tToJs(/* variant88 */173497541) !== "variant88") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          398,
          2
        ]
      ];
}

if (tToJs(/* variant89 */173497542) !== "variant89") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          399,
          2
        ]
      ];
}

if (tToJs(/* variant90 */173497756) !== "variant90") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          400,
          2
        ]
      ];
}

if (tToJs(/* variant91 */173497757) !== "variant91") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          401,
          2
        ]
      ];
}

if (tToJs(/* variant92 */173497758) !== "variant92") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          402,
          2
        ]
      ];
}

if (tToJs(/* variant93 */173497759) !== "variant93") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          403,
          2
        ]
      ];
}

if (tToJs(/* variant94 */173497760) !== "variant94") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          404,
          2
        ]
      ];
}

if (tToJs(/* variant95 */173497761) !== "variant95") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          405,
          2
        ]
      ];
}

if (tToJs(/* variant96 */173497762) !== "variant96") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          406,
          2
        ]
      ];
}

if (tToJs(/* variant97 */173497763) !== "variant97") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          407,
          2
        ]
      ];
}

if (tToJs(/* variant98 */173497764) !== "variant98") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          408,
          2
        ]
      ];
}

if (tToJs(/* variant99 */173497765) !== "variant99") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          409,
          2
        ]
      ];
}

if (tToJs(/* variant100 */34896140) !== "variant100") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          410,
          2
        ]
      ];
}

if (tToJs(/* variant101 */34896141) !== "variant101") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          411,
          2
        ]
      ];
}

if (tToJs(/* variant102 */34896142) !== "variant102") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          412,
          2
        ]
      ];
}

if (tToJs(/* variant103 */34896143) !== "variant103") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          413,
          2
        ]
      ];
}

if (tToJs(/* variant104 */34896144) !== "variant104") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          414,
          2
        ]
      ];
}

if (tToJs(/* variant105 */34896145) !== "variant105") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          415,
          2
        ]
      ];
}

if (tToJs(/* variant106 */34896146) !== "variant106") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          416,
          2
        ]
      ];
}

if (tToJs(/* variant107 */34896147) !== "variant107") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          417,
          2
        ]
      ];
}

if (tToJs(/* variant108 */34896148) !== "variant108") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          418,
          2
        ]
      ];
}

if (tToJs(/* variant109 */34896149) !== "variant109") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          419,
          2
        ]
      ];
}

if (tToJs(/* variant110 */34896363) !== "variant110") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          420,
          2
        ]
      ];
}

if (tToJs(/* variant111 */34896364) !== "variant111") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          421,
          2
        ]
      ];
}

if (tToJs(/* variant112 */34896365) !== "variant112") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          422,
          2
        ]
      ];
}

if (tToJs(/* variant113 */34896366) !== "variant113") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          423,
          2
        ]
      ];
}

if (tToJs(/* variant114 */34896367) !== "variant114") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          424,
          2
        ]
      ];
}

if (tToJs(/* variant115 */34896368) !== "variant115") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          425,
          2
        ]
      ];
}

if (tToJs(/* variant116 */34896369) !== "variant116") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          426,
          2
        ]
      ];
}

if (tToJs(/* variant117 */34896370) !== "variant117") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          427,
          2
        ]
      ];
}

if (tToJs(/* variant118 */34896371) !== "variant118") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          428,
          2
        ]
      ];
}

if (tToJs(/* variant119 */34896372) !== "variant119") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          429,
          2
        ]
      ];
}

if (tToJs(/* variant120 */34896586) !== "variant120") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          430,
          2
        ]
      ];
}

if (tToJs(/* variant121 */34896587) !== "variant121") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          431,
          2
        ]
      ];
}

if (tToJs(/* variant122 */34896588) !== "variant122") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          432,
          2
        ]
      ];
}

if (tToJs(/* variant123 */34896589) !== "variant123") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          433,
          2
        ]
      ];
}

if (tToJs(/* variant124 */34896590) !== "variant124") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          434,
          2
        ]
      ];
}

if (tToJs(/* variant125 */34896591) !== "variant125") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          435,
          2
        ]
      ];
}

if (tToJs(/* variant126 */34896592) !== "variant126") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          436,
          2
        ]
      ];
}

if (tToJs(/* variant127 */34896593) !== "variant127") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          437,
          2
        ]
      ];
}

if (tToJs(/* variant128 */34896594) !== "variant128") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          438,
          2
        ]
      ];
}

if (tToJs(/* variant129 */34896595) !== "variant129") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          439,
          2
        ]
      ];
}

if (tToJs(/* variant130 */34896809) !== "variant130") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          440,
          2
        ]
      ];
}

if (tToJs(/* variant131 */34896810) !== "variant131") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          441,
          2
        ]
      ];
}

if (tToJs(/* variant132 */34896811) !== "variant132") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          442,
          2
        ]
      ];
}

if (tToJs(/* variant133 */34896812) !== "variant133") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          443,
          2
        ]
      ];
}

if (tToJs(/* variant134 */34896813) !== "variant134") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          444,
          2
        ]
      ];
}

if (tToJs(/* variant135 */34896814) !== "variant135") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          445,
          2
        ]
      ];
}

if (tToJs(/* variant136 */34896815) !== "variant136") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          446,
          2
        ]
      ];
}

if (tToJs(/* variant137 */34896816) !== "variant137") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          447,
          2
        ]
      ];
}

if (tToJs(/* variant138 */34896817) !== "variant138") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          448,
          2
        ]
      ];
}

if (tToJs(/* variant139 */34896818) !== "variant139") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          449,
          2
        ]
      ];
}

if (tToJs(/* variant140 */34897032) !== "variant140") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          450,
          2
        ]
      ];
}

if (tToJs(/* variant141 */34897033) !== "variant141") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          451,
          2
        ]
      ];
}

if (tToJs(/* variant142 */34897034) !== "variant142") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          452,
          2
        ]
      ];
}

if (tToJs(/* variant143 */34897035) !== "variant143") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          453,
          2
        ]
      ];
}

if (tToJs(/* variant144 */34897036) !== "variant144") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          454,
          2
        ]
      ];
}

if (tToJs(/* variant145 */34897037) !== "variant145") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          455,
          2
        ]
      ];
}

if (tToJs(/* variant146 */34897038) !== "variant146") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          456,
          2
        ]
      ];
}

if (tToJs(/* variant147 */34897039) !== "variant147") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          457,
          2
        ]
      ];
}

if (tToJs(/* variant148 */34897040) !== "variant148") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          458,
          2
        ]
      ];
}

if (tToJs(/* variant149 */34897041) !== "variant149") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          459,
          2
        ]
      ];
}

if (tToJs(/* variant150 */34897255) !== "variant150") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          460,
          2
        ]
      ];
}

if (tToJs(/* variant151 */34897256) !== "variant151") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          461,
          2
        ]
      ];
}

if (tToJs(/* variant152 */34897257) !== "variant152") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          462,
          2
        ]
      ];
}

if (tToJs(/* variant153 */34897258) !== "variant153") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          463,
          2
        ]
      ];
}

if (tToJs(/* variant154 */34897259) !== "variant154") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          464,
          2
        ]
      ];
}

if (tToJs(/* variant155 */34897260) !== "variant155") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          465,
          2
        ]
      ];
}

if (tToJs(/* variant156 */34897261) !== "variant156") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          466,
          2
        ]
      ];
}

if (tToJs(/* variant157 */34897262) !== "variant157") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          467,
          2
        ]
      ];
}

if (tToJs(/* variant158 */34897263) !== "variant158") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          468,
          2
        ]
      ];
}

if (tToJs(/* variant159 */34897264) !== "variant159") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          469,
          2
        ]
      ];
}

if (tToJs(/* variant160 */34897478) !== "variant160") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          470,
          2
        ]
      ];
}

if (tToJs(/* variant161 */34897479) !== "variant161") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          471,
          2
        ]
      ];
}

if (tToJs(/* variant162 */34897480) !== "variant162") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          472,
          2
        ]
      ];
}

if (tToJs(/* variant163 */34897481) !== "variant163") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          473,
          2
        ]
      ];
}

if (tToJs(/* variant164 */34897482) !== "variant164") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          474,
          2
        ]
      ];
}

if (tToJs(/* variant165 */34897483) !== "variant165") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          475,
          2
        ]
      ];
}

if (tToJs(/* variant166 */34897484) !== "variant166") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          476,
          2
        ]
      ];
}

if (tToJs(/* variant167 */34897485) !== "variant167") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          477,
          2
        ]
      ];
}

if (tToJs(/* variant168 */34897486) !== "variant168") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          478,
          2
        ]
      ];
}

if (tToJs(/* variant169 */34897487) !== "variant169") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          479,
          2
        ]
      ];
}

if (tToJs(/* variant170 */34897701) !== "variant170") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          480,
          2
        ]
      ];
}

if (tToJs(/* variant171 */34897702) !== "variant171") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          481,
          2
        ]
      ];
}

if (tToJs(/* variant172 */34897703) !== "variant172") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          482,
          2
        ]
      ];
}

if (tToJs(/* variant173 */34897704) !== "variant173") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          483,
          2
        ]
      ];
}

if (tToJs(/* variant174 */34897705) !== "variant174") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          484,
          2
        ]
      ];
}

if (tToJs(/* variant175 */34897706) !== "variant175") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          485,
          2
        ]
      ];
}

if (tToJs(/* variant176 */34897707) !== "variant176") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          486,
          2
        ]
      ];
}

if (tToJs(/* variant177 */34897708) !== "variant177") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          487,
          2
        ]
      ];
}

if (tToJs(/* variant178 */34897709) !== "variant178") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          488,
          2
        ]
      ];
}

if (tToJs(/* variant179 */34897710) !== "variant179") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          489,
          2
        ]
      ];
}

if (tToJs(/* variant180 */34897924) !== "variant180") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          490,
          2
        ]
      ];
}

if (tToJs(/* variant181 */34897925) !== "variant181") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          491,
          2
        ]
      ];
}

if (tToJs(/* variant182 */34897926) !== "variant182") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          492,
          2
        ]
      ];
}

if (tToJs(/* variant183 */34897927) !== "variant183") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          493,
          2
        ]
      ];
}

if (tToJs(/* variant184 */34897928) !== "variant184") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          494,
          2
        ]
      ];
}

if (tToJs(/* variant185 */34897929) !== "variant185") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          495,
          2
        ]
      ];
}

if (tToJs(/* variant186 */34897930) !== "variant186") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          496,
          2
        ]
      ];
}

if (tToJs(/* variant187 */34897931) !== "variant187") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          497,
          2
        ]
      ];
}

if (tToJs(/* variant188 */34897932) !== "variant188") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          498,
          2
        ]
      ];
}

if (tToJs(/* variant189 */34897933) !== "variant189") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          499,
          2
        ]
      ];
}

if (tToJs(/* variant190 */34898147) !== "variant190") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          500,
          2
        ]
      ];
}

if (tToJs(/* variant191 */34898148) !== "variant191") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          501,
          2
        ]
      ];
}

if (tToJs(/* variant192 */34898149) !== "variant192") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          502,
          2
        ]
      ];
}

if (tToJs(/* variant193 */34898150) !== "variant193") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          503,
          2
        ]
      ];
}

if (tToJs(/* variant194 */34898151) !== "variant194") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          504,
          2
        ]
      ];
}

if (tToJs(/* variant195 */34898152) !== "variant195") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          505,
          2
        ]
      ];
}

if (tToJs(/* variant196 */34898153) !== "variant196") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          506,
          2
        ]
      ];
}

if (tToJs(/* variant197 */34898154) !== "variant197") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          507,
          2
        ]
      ];
}

if (tToJs(/* variant198 */34898155) !== "variant198") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          508,
          2
        ]
      ];
}

if (tToJs(/* variant199 */34898156) !== "variant199") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          509,
          2
        ]
      ];
}

if (tToJs(/* variant200 */34945869) !== "variant200") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          510,
          2
        ]
      ];
}

if (tToJs(/* variant201 */34945870) !== "variant201") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          511,
          2
        ]
      ];
}

if (tToJs(/* variant202 */34945871) !== "variant202") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          512,
          2
        ]
      ];
}

if (tToJs(/* variant203 */34945872) !== "variant203") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          513,
          2
        ]
      ];
}

if (tToJs(/* variant204 */34945873) !== "variant204") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          514,
          2
        ]
      ];
}

if (tToJs(/* variant205 */34945874) !== "variant205") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          515,
          2
        ]
      ];
}

if (tToJs(/* variant206 */34945875) !== "variant206") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          516,
          2
        ]
      ];
}

if (tToJs(/* variant207 */34945876) !== "variant207") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          517,
          2
        ]
      ];
}

if (tToJs(/* variant208 */34945877) !== "variant208") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          518,
          2
        ]
      ];
}

if (tToJs(/* variant209 */34945878) !== "variant209") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          519,
          2
        ]
      ];
}

if (tToJs(/* variant210 */34946092) !== "variant210") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          520,
          2
        ]
      ];
}

if (tToJs(/* variant211 */34946093) !== "variant211") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          521,
          2
        ]
      ];
}

if (tToJs(/* variant212 */34946094) !== "variant212") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          522,
          2
        ]
      ];
}

if (tToJs(/* variant213 */34946095) !== "variant213") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          523,
          2
        ]
      ];
}

if (tToJs(/* variant214 */34946096) !== "variant214") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          524,
          2
        ]
      ];
}

if (tToJs(/* variant215 */34946097) !== "variant215") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          525,
          2
        ]
      ];
}

if (tToJs(/* variant216 */34946098) !== "variant216") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          526,
          2
        ]
      ];
}

if (tToJs(/* variant217 */34946099) !== "variant217") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          527,
          2
        ]
      ];
}

if (tToJs(/* variant218 */34946100) !== "variant218") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          528,
          2
        ]
      ];
}

if (tToJs(/* variant219 */34946101) !== "variant219") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          529,
          2
        ]
      ];
}

if (tToJs(/* variant220 */34946315) !== "variant220") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          530,
          2
        ]
      ];
}

if (tToJs(/* variant221 */34946316) !== "variant221") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          531,
          2
        ]
      ];
}

if (tToJs(/* variant222 */34946317) !== "variant222") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          532,
          2
        ]
      ];
}

if (tToJs(/* variant223 */34946318) !== "variant223") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          533,
          2
        ]
      ];
}

if (tToJs(/* variant224 */34946319) !== "variant224") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          534,
          2
        ]
      ];
}

if (tToJs(/* variant225 */34946320) !== "variant225") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          535,
          2
        ]
      ];
}

if (tToJs(/* variant226 */34946321) !== "variant226") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          536,
          2
        ]
      ];
}

if (tToJs(/* variant227 */34946322) !== "variant227") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          537,
          2
        ]
      ];
}

if (tToJs(/* variant228 */34946323) !== "variant228") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          538,
          2
        ]
      ];
}

if (tToJs(/* variant229 */34946324) !== "variant229") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          539,
          2
        ]
      ];
}

if (tToJs(/* variant230 */34946538) !== "variant230") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          540,
          2
        ]
      ];
}

if (tToJs(/* variant231 */34946539) !== "variant231") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          541,
          2
        ]
      ];
}

if (tToJs(/* variant232 */34946540) !== "variant232") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          542,
          2
        ]
      ];
}

if (tToJs(/* variant233 */34946541) !== "variant233") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          543,
          2
        ]
      ];
}

if (tToJs(/* variant234 */34946542) !== "variant234") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          544,
          2
        ]
      ];
}

if (tToJs(/* variant235 */34946543) !== "variant235") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          545,
          2
        ]
      ];
}

if (tToJs(/* variant236 */34946544) !== "variant236") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          546,
          2
        ]
      ];
}

if (tToJs(/* variant237 */34946545) !== "variant237") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          547,
          2
        ]
      ];
}

if (tToJs(/* variant238 */34946546) !== "variant238") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          548,
          2
        ]
      ];
}

if (tToJs(/* variant239 */34946547) !== "variant239") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          549,
          2
        ]
      ];
}

if (tToJs(/* variant240 */34946761) !== "variant240") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          550,
          2
        ]
      ];
}

if (tToJs(/* variant241 */34946762) !== "variant241") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          551,
          2
        ]
      ];
}

if (tToJs(/* variant242 */34946763) !== "variant242") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          552,
          2
        ]
      ];
}

if (tToJs(/* variant243 */34946764) !== "variant243") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          553,
          2
        ]
      ];
}

if (tToJs(/* variant244 */34946765) !== "variant244") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          554,
          2
        ]
      ];
}

if (tToJs(/* variant245 */34946766) !== "variant245") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          555,
          2
        ]
      ];
}

if (tToJs(/* variant246 */34946767) !== "variant246") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          556,
          2
        ]
      ];
}

if (tToJs(/* variant247 */34946768) !== "variant247") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          557,
          2
        ]
      ];
}

if (tToJs(/* variant248 */34946769) !== "variant248") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          558,
          2
        ]
      ];
}

if (tToJs(/* variant249 */34946770) !== "variant249") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          559,
          2
        ]
      ];
}

if (tToJs(/* variant250 */34946984) !== "variant250") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          560,
          2
        ]
      ];
}

if (tToJs(/* variant251 */34946985) !== "variant251") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          561,
          2
        ]
      ];
}

if (tToJs(/* variant252 */34946986) !== "variant252") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          562,
          2
        ]
      ];
}

if (tToJs(/* variant253 */34946987) !== "variant253") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          563,
          2
        ]
      ];
}

if (tToJs(/* variant254 */34946988) !== "variant254") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          564,
          2
        ]
      ];
}

if (tToJs(/* variant255 */34946989) !== "variant255") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          565,
          2
        ]
      ];
}

if (tToJs(/* variant256 */34946990) !== "variant256") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          566,
          2
        ]
      ];
}

if (tToJs(/* variant257 */34946991) !== "variant257") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          567,
          2
        ]
      ];
}

if (tToJs(/* variant258 */34946992) !== "variant258") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          568,
          2
        ]
      ];
}

if (tToJs(/* variant259 */34946993) !== "variant259") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          569,
          2
        ]
      ];
}

if (tToJs(/* variant260 */34947207) !== "variant260") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          570,
          2
        ]
      ];
}

if (tToJs(/* variant261 */34947208) !== "variant261") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          571,
          2
        ]
      ];
}

if (tToJs(/* variant262 */34947209) !== "variant262") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          572,
          2
        ]
      ];
}

if (tToJs(/* variant263 */34947210) !== "variant263") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          573,
          2
        ]
      ];
}

if (tToJs(/* variant264 */34947211) !== "variant264") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          574,
          2
        ]
      ];
}

if (tToJs(/* variant265 */34947212) !== "variant265") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          575,
          2
        ]
      ];
}

if (tToJs(/* variant266 */34947213) !== "variant266") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          576,
          2
        ]
      ];
}

if (tToJs(/* variant267 */34947214) !== "variant267") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          577,
          2
        ]
      ];
}

if (tToJs(/* variant268 */34947215) !== "variant268") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          578,
          2
        ]
      ];
}

if (tToJs(/* variant269 */34947216) !== "variant269") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          579,
          2
        ]
      ];
}

if (tToJs(/* variant270 */34947430) !== "variant270") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          580,
          2
        ]
      ];
}

if (tToJs(/* variant271 */34947431) !== "variant271") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          581,
          2
        ]
      ];
}

if (tToJs(/* variant272 */34947432) !== "variant272") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          582,
          2
        ]
      ];
}

if (tToJs(/* variant273 */34947433) !== "variant273") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          583,
          2
        ]
      ];
}

if (tToJs(/* variant274 */34947434) !== "variant274") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          584,
          2
        ]
      ];
}

if (tToJs(/* variant275 */34947435) !== "variant275") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          585,
          2
        ]
      ];
}

if (tToJs(/* variant276 */34947436) !== "variant276") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          586,
          2
        ]
      ];
}

if (tToJs(/* variant277 */34947437) !== "variant277") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          587,
          2
        ]
      ];
}

if (tToJs(/* variant278 */34947438) !== "variant278") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          588,
          2
        ]
      ];
}

if (tToJs(/* variant279 */34947439) !== "variant279") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          589,
          2
        ]
      ];
}

if (tToJs(/* variant280 */34947653) !== "variant280") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          590,
          2
        ]
      ];
}

if (tToJs(/* variant281 */34947654) !== "variant281") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          591,
          2
        ]
      ];
}

if (tToJs(/* variant282 */34947655) !== "variant282") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          592,
          2
        ]
      ];
}

if (tToJs(/* variant283 */34947656) !== "variant283") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          593,
          2
        ]
      ];
}

if (tToJs(/* variant284 */34947657) !== "variant284") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          594,
          2
        ]
      ];
}

if (tToJs(/* variant285 */34947658) !== "variant285") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          595,
          2
        ]
      ];
}

if (tToJs(/* variant286 */34947659) !== "variant286") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          596,
          2
        ]
      ];
}

if (tToJs(/* variant287 */34947660) !== "variant287") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          597,
          2
        ]
      ];
}

if (tToJs(/* variant288 */34947661) !== "variant288") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          598,
          2
        ]
      ];
}

if (tToJs(/* variant289 */34947662) !== "variant289") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          599,
          2
        ]
      ];
}

if (tToJs(/* variant290 */34947876) !== "variant290") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          600,
          2
        ]
      ];
}

if (tToJs(/* variant291 */34947877) !== "variant291") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          601,
          2
        ]
      ];
}

if (tToJs(/* variant292 */34947878) !== "variant292") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          602,
          2
        ]
      ];
}

if (tToJs(/* variant293 */34947879) !== "variant293") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          603,
          2
        ]
      ];
}

if (tToJs(/* variant294 */34947880) !== "variant294") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          604,
          2
        ]
      ];
}

if (tToJs(/* variant295 */34947881) !== "variant295") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          605,
          2
        ]
      ];
}

if (tToJs(/* variant296 */34947882) !== "variant296") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          606,
          2
        ]
      ];
}

if (tToJs(/* variant297 */34947883) !== "variant297") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          607,
          2
        ]
      ];
}

if (tToJs(/* variant298 */34947884) !== "variant298") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          608,
          2
        ]
      ];
}

if (tToJs(/* variant299 */34947885) !== "variant299") {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          609,
          2
        ]
      ];
}

if (!eq(tFromJs("variant0"), /* variant0 */-384420853)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          610,
          2
        ]
      ];
}

if (!eq(tFromJs("variant1"), /* variant1 */-384420852)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          611,
          2
        ]
      ];
}

if (!eq(tFromJs("variant2"), /* variant2 */-384420851)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          612,
          2
        ]
      ];
}

if (!eq(tFromJs("variant3"), /* variant3 */-384420850)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          613,
          2
        ]
      ];
}

if (!eq(tFromJs("variant4"), /* variant4 */-384420849)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          614,
          2
        ]
      ];
}

if (!eq(tFromJs("variant5"), /* variant5 */-384420848)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          615,
          2
        ]
      ];
}

if (!eq(tFromJs("variant6"), /* variant6 */-384420847)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          616,
          2
        ]
      ];
}

if (!eq(tFromJs("variant7"), /* variant7 */-384420846)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          617,
          2
        ]
      ];
}

if (!eq(tFromJs("variant8"), /* variant8 */-384420845)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          618,
          2
        ]
      ];
}

if (!eq(tFromJs("variant9"), /* variant9 */-384420844)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          619,
          2
        ]
      ];
}

if (!eq(tFromJs("variant10"), /* variant10 */173495972)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          620,
          2
        ]
      ];
}

if (!eq(tFromJs("variant11"), /* variant11 */173495973)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          621,
          2
        ]
      ];
}

if (!eq(tFromJs("variant12"), /* variant12 */173495974)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          622,
          2
        ]
      ];
}

if (!eq(tFromJs("variant13"), /* variant13 */173495975)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          623,
          2
        ]
      ];
}

if (!eq(tFromJs("variant14"), /* variant14 */173495976)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          624,
          2
        ]
      ];
}

if (!eq(tFromJs("variant15"), /* variant15 */173495977)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          625,
          2
        ]
      ];
}

if (!eq(tFromJs("variant16"), /* variant16 */173495978)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          626,
          2
        ]
      ];
}

if (!eq(tFromJs("variant17"), /* variant17 */173495979)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          627,
          2
        ]
      ];
}

if (!eq(tFromJs("variant18"), /* variant18 */173495980)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          628,
          2
        ]
      ];
}

if (!eq(tFromJs("variant19"), /* variant19 */173495981)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          629,
          2
        ]
      ];
}

if (!eq(tFromJs("variant20"), /* variant20 */173496195)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          630,
          2
        ]
      ];
}

if (!eq(tFromJs("variant21"), /* variant21 */173496196)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          631,
          2
        ]
      ];
}

if (!eq(tFromJs("variant22"), /* variant22 */173496197)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          632,
          2
        ]
      ];
}

if (!eq(tFromJs("variant23"), /* variant23 */173496198)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          633,
          2
        ]
      ];
}

if (!eq(tFromJs("variant24"), /* variant24 */173496199)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          634,
          2
        ]
      ];
}

if (!eq(tFromJs("variant25"), /* variant25 */173496200)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          635,
          2
        ]
      ];
}

if (!eq(tFromJs("variant26"), /* variant26 */173496201)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          636,
          2
        ]
      ];
}

if (!eq(tFromJs("variant27"), /* variant27 */173496202)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          637,
          2
        ]
      ];
}

if (!eq(tFromJs("variant28"), /* variant28 */173496203)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          638,
          2
        ]
      ];
}

if (!eq(tFromJs("variant29"), /* variant29 */173496204)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          639,
          2
        ]
      ];
}

if (!eq(tFromJs("variant30"), /* variant30 */173496418)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          640,
          2
        ]
      ];
}

if (!eq(tFromJs("variant31"), /* variant31 */173496419)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          641,
          2
        ]
      ];
}

if (!eq(tFromJs("variant32"), /* variant32 */173496420)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          642,
          2
        ]
      ];
}

if (!eq(tFromJs("variant33"), /* variant33 */173496421)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          643,
          2
        ]
      ];
}

if (!eq(tFromJs("variant34"), /* variant34 */173496422)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          644,
          2
        ]
      ];
}

if (!eq(tFromJs("variant35"), /* variant35 */173496423)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          645,
          2
        ]
      ];
}

if (!eq(tFromJs("variant36"), /* variant36 */173496424)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          646,
          2
        ]
      ];
}

if (!eq(tFromJs("variant37"), /* variant37 */173496425)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          647,
          2
        ]
      ];
}

if (!eq(tFromJs("variant38"), /* variant38 */173496426)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          648,
          2
        ]
      ];
}

if (!eq(tFromJs("variant39"), /* variant39 */173496427)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          649,
          2
        ]
      ];
}

if (!eq(tFromJs("variant40"), /* variant40 */173496641)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          650,
          2
        ]
      ];
}

if (!eq(tFromJs("variant41"), /* variant41 */173496642)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          651,
          2
        ]
      ];
}

if (!eq(tFromJs("variant42"), /* variant42 */173496643)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          652,
          2
        ]
      ];
}

if (!eq(tFromJs("variant43"), /* variant43 */173496644)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          653,
          2
        ]
      ];
}

if (!eq(tFromJs("variant44"), /* variant44 */173496645)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          654,
          2
        ]
      ];
}

if (!eq(tFromJs("variant45"), /* variant45 */173496646)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          655,
          2
        ]
      ];
}

if (!eq(tFromJs("variant46"), /* variant46 */173496647)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          656,
          2
        ]
      ];
}

if (!eq(tFromJs("variant47"), /* variant47 */173496648)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          657,
          2
        ]
      ];
}

if (!eq(tFromJs("variant48"), /* variant48 */173496649)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          658,
          2
        ]
      ];
}

if (!eq(tFromJs("variant49"), /* variant49 */173496650)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          659,
          2
        ]
      ];
}

if (!eq(tFromJs("variant50"), /* variant50 */173496864)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          660,
          2
        ]
      ];
}

if (!eq(tFromJs("variant51"), /* variant51 */173496865)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          661,
          2
        ]
      ];
}

if (!eq(tFromJs("variant52"), /* variant52 */173496866)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          662,
          2
        ]
      ];
}

if (!eq(tFromJs("variant53"), /* variant53 */173496867)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          663,
          2
        ]
      ];
}

if (!eq(tFromJs("variant54"), /* variant54 */173496868)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          664,
          2
        ]
      ];
}

if (!eq(tFromJs("variant55"), /* variant55 */173496869)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          665,
          2
        ]
      ];
}

if (!eq(tFromJs("variant56"), /* variant56 */173496870)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          666,
          2
        ]
      ];
}

if (!eq(tFromJs("variant57"), /* variant57 */173496871)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          667,
          2
        ]
      ];
}

if (!eq(tFromJs("variant58"), /* variant58 */173496872)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          668,
          2
        ]
      ];
}

if (!eq(tFromJs("variant59"), /* variant59 */173496873)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          669,
          2
        ]
      ];
}

if (!eq(tFromJs("variant60"), /* variant60 */173497087)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          670,
          2
        ]
      ];
}

if (!eq(tFromJs("variant61"), /* variant61 */173497088)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          671,
          2
        ]
      ];
}

if (!eq(tFromJs("variant62"), /* variant62 */173497089)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          672,
          2
        ]
      ];
}

if (!eq(tFromJs("variant63"), /* variant63 */173497090)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          673,
          2
        ]
      ];
}

if (!eq(tFromJs("variant64"), /* variant64 */173497091)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          674,
          2
        ]
      ];
}

if (!eq(tFromJs("variant65"), /* variant65 */173497092)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          675,
          2
        ]
      ];
}

if (!eq(tFromJs("variant66"), /* variant66 */173497093)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          676,
          2
        ]
      ];
}

if (!eq(tFromJs("variant67"), /* variant67 */173497094)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          677,
          2
        ]
      ];
}

if (!eq(tFromJs("variant68"), /* variant68 */173497095)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          678,
          2
        ]
      ];
}

if (!eq(tFromJs("variant69"), /* variant69 */173497096)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          679,
          2
        ]
      ];
}

if (!eq(tFromJs("variant70"), /* variant70 */173497310)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          680,
          2
        ]
      ];
}

if (!eq(tFromJs("variant71"), /* variant71 */173497311)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          681,
          2
        ]
      ];
}

if (!eq(tFromJs("variant72"), /* variant72 */173497312)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          682,
          2
        ]
      ];
}

if (!eq(tFromJs("variant73"), /* variant73 */173497313)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          683,
          2
        ]
      ];
}

if (!eq(tFromJs("variant74"), /* variant74 */173497314)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          684,
          2
        ]
      ];
}

if (!eq(tFromJs("variant75"), /* variant75 */173497315)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          685,
          2
        ]
      ];
}

if (!eq(tFromJs("variant76"), /* variant76 */173497316)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          686,
          2
        ]
      ];
}

if (!eq(tFromJs("variant77"), /* variant77 */173497317)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          687,
          2
        ]
      ];
}

if (!eq(tFromJs("variant78"), /* variant78 */173497318)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          688,
          2
        ]
      ];
}

if (!eq(tFromJs("variant79"), /* variant79 */173497319)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          689,
          2
        ]
      ];
}

if (!eq(tFromJs("variant80"), /* variant80 */173497533)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          690,
          2
        ]
      ];
}

if (!eq(tFromJs("variant81"), /* variant81 */173497534)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          691,
          2
        ]
      ];
}

if (!eq(tFromJs("variant82"), /* variant82 */173497535)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          692,
          2
        ]
      ];
}

if (!eq(tFromJs("variant83"), /* variant83 */173497536)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          693,
          2
        ]
      ];
}

if (!eq(tFromJs("variant84"), /* variant84 */173497537)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          694,
          2
        ]
      ];
}

if (!eq(tFromJs("variant85"), /* variant85 */173497538)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          695,
          2
        ]
      ];
}

if (!eq(tFromJs("variant86"), /* variant86 */173497539)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          696,
          2
        ]
      ];
}

if (!eq(tFromJs("variant87"), /* variant87 */173497540)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          697,
          2
        ]
      ];
}

if (!eq(tFromJs("variant88"), /* variant88 */173497541)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          698,
          2
        ]
      ];
}

if (!eq(tFromJs("variant89"), /* variant89 */173497542)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          699,
          2
        ]
      ];
}

if (!eq(tFromJs("variant90"), /* variant90 */173497756)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          700,
          2
        ]
      ];
}

if (!eq(tFromJs("variant91"), /* variant91 */173497757)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          701,
          2
        ]
      ];
}

if (!eq(tFromJs("variant92"), /* variant92 */173497758)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          702,
          2
        ]
      ];
}

if (!eq(tFromJs("variant93"), /* variant93 */173497759)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          703,
          2
        ]
      ];
}

if (!eq(tFromJs("variant94"), /* variant94 */173497760)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          704,
          2
        ]
      ];
}

if (!eq(tFromJs("variant95"), /* variant95 */173497761)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          705,
          2
        ]
      ];
}

if (!eq(tFromJs("variant96"), /* variant96 */173497762)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          706,
          2
        ]
      ];
}

if (!eq(tFromJs("variant97"), /* variant97 */173497763)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          707,
          2
        ]
      ];
}

if (!eq(tFromJs("variant98"), /* variant98 */173497764)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          708,
          2
        ]
      ];
}

if (!eq(tFromJs("variant99"), /* variant99 */173497765)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          709,
          2
        ]
      ];
}

if (!eq(tFromJs("variant100"), /* variant100 */34896140)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          710,
          2
        ]
      ];
}

if (!eq(tFromJs("variant101"), /* variant101 */34896141)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          711,
          2
        ]
      ];
}

if (!eq(tFromJs("variant102"), /* variant102 */34896142)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          712,
          2
        ]
      ];
}

if (!eq(tFromJs("variant103"), /* variant103 */34896143)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          713,
          2
        ]
      ];
}

if (!eq(tFromJs("variant104"), /* variant104 */34896144)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          714,
          2
        ]
      ];
}

if (!eq(tFromJs("variant105"), /* variant105 */34896145)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          715,
          2
        ]
      ];
}

if (!eq(tFromJs("variant106"), /* variant106 */34896146)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          716,
          2
        ]
      ];
}

if (!eq(tFromJs("variant107"), /* variant107 */34896147)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          717,
          2
        ]
      ];
}

if (!eq(tFromJs("variant108"), /* variant108 */34896148)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          718,
          2
        ]
      ];
}

if (!eq(tFromJs("variant109"), /* variant109 */34896149)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          719,
          2
        ]
      ];
}

if (!eq(tFromJs("variant110"), /* variant110 */34896363)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          720,
          2
        ]
      ];
}

if (!eq(tFromJs("variant111"), /* variant111 */34896364)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          721,
          2
        ]
      ];
}

if (!eq(tFromJs("variant112"), /* variant112 */34896365)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          722,
          2
        ]
      ];
}

if (!eq(tFromJs("variant113"), /* variant113 */34896366)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          723,
          2
        ]
      ];
}

if (!eq(tFromJs("variant114"), /* variant114 */34896367)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          724,
          2
        ]
      ];
}

if (!eq(tFromJs("variant115"), /* variant115 */34896368)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          725,
          2
        ]
      ];
}

if (!eq(tFromJs("variant116"), /* variant116 */34896369)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          726,
          2
        ]
      ];
}

if (!eq(tFromJs("variant117"), /* variant117 */34896370)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          727,
          2
        ]
      ];
}

if (!eq(tFromJs("variant118"), /* variant118 */34896371)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          728,
          2
        ]
      ];
}

if (!eq(tFromJs("variant119"), /* variant119 */34896372)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          729,
          2
        ]
      ];
}

if (!eq(tFromJs("variant120"), /* variant120 */34896586)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          730,
          2
        ]
      ];
}

if (!eq(tFromJs("variant121"), /* variant121 */34896587)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          731,
          2
        ]
      ];
}

if (!eq(tFromJs("variant122"), /* variant122 */34896588)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          732,
          2
        ]
      ];
}

if (!eq(tFromJs("variant123"), /* variant123 */34896589)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          733,
          2
        ]
      ];
}

if (!eq(tFromJs("variant124"), /* variant124 */34896590)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          734,
          2
        ]
      ];
}

if (!eq(tFromJs("variant125"), /* variant125 */34896591)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          735,
          2
        ]
      ];
}

if (!eq(tFromJs("variant126"), /* variant126 */34896592)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          736,
          2
        ]
      ];
}

if (!eq(tFromJs("variant127"), /* variant127 */34896593)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          737,
          2
        ]
      ];
}

if (!eq(tFromJs("variant128"), /* variant128 */34896594)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          738,
          2
        ]
      ];
}

if (!eq(tFromJs("variant129"), /* variant129 */34896595)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          739,
          2
        ]
      ];
}

if (!eq(tFromJs("variant130"), /* variant130 */34896809)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          740,
          2
        ]
      ];
}

if (!eq(tFromJs("variant131"), /* variant131 */34896810)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          741,
          2
        ]
      ];
}

if (!eq(tFromJs("variant132"), /* variant132 */34896811)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          742,
          2
        ]
      ];
}

if (!eq(tFromJs("variant133"), /* variant133 */34896812)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          743,
          2
        ]
      ];
}

if (!eq(tFromJs("variant134"), /* variant134 */34896813)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          744,
          2
        ]
      ];
}

if (!eq(tFromJs("variant135"), /* variant135 */34896814)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          745,
          2
        ]
      ];
}

if (!eq(tFromJs("variant136"), /* variant136 */34896815)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          746,
          2
        ]
      ];
}

if (!eq(tFromJs("variant137"), /* variant137 */34896816)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          747,
          2
        ]
      ];
}

if (!eq(tFromJs("variant138"), /* variant138 */34896817)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          748,
          2
        ]
      ];
}

if (!eq(tFromJs("variant139"), /* variant139 */34896818)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          749,
          2
        ]
      ];
}

if (!eq(tFromJs("variant140"), /* variant140 */34897032)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          750,
          2
        ]
      ];
}

if (!eq(tFromJs("variant141"), /* variant141 */34897033)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          751,
          2
        ]
      ];
}

if (!eq(tFromJs("variant142"), /* variant142 */34897034)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          752,
          2
        ]
      ];
}

if (!eq(tFromJs("variant143"), /* variant143 */34897035)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          753,
          2
        ]
      ];
}

if (!eq(tFromJs("variant144"), /* variant144 */34897036)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          754,
          2
        ]
      ];
}

if (!eq(tFromJs("variant145"), /* variant145 */34897037)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          755,
          2
        ]
      ];
}

if (!eq(tFromJs("variant146"), /* variant146 */34897038)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          756,
          2
        ]
      ];
}

if (!eq(tFromJs("variant147"), /* variant147 */34897039)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          757,
          2
        ]
      ];
}

if (!eq(tFromJs("variant148"), /* variant148 */34897040)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          758,
          2
        ]
      ];
}

if (!eq(tFromJs("variant149"), /* variant149 */34897041)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          759,
          2
        ]
      ];
}

if (!eq(tFromJs("variant150"), /* variant150 */34897255)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          760,
          2
        ]
      ];
}

if (!eq(tFromJs("variant151"), /* variant151 */34897256)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          761,
          2
        ]
      ];
}

if (!eq(tFromJs("variant152"), /* variant152 */34897257)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          762,
          2
        ]
      ];
}

if (!eq(tFromJs("variant153"), /* variant153 */34897258)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          763,
          2
        ]
      ];
}

if (!eq(tFromJs("variant154"), /* variant154 */34897259)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          764,
          2
        ]
      ];
}

if (!eq(tFromJs("variant155"), /* variant155 */34897260)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          765,
          2
        ]
      ];
}

if (!eq(tFromJs("variant156"), /* variant156 */34897261)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          766,
          2
        ]
      ];
}

if (!eq(tFromJs("variant157"), /* variant157 */34897262)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          767,
          2
        ]
      ];
}

if (!eq(tFromJs("variant158"), /* variant158 */34897263)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          768,
          2
        ]
      ];
}

if (!eq(tFromJs("variant159"), /* variant159 */34897264)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          769,
          2
        ]
      ];
}

if (!eq(tFromJs("variant160"), /* variant160 */34897478)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          770,
          2
        ]
      ];
}

if (!eq(tFromJs("variant161"), /* variant161 */34897479)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          771,
          2
        ]
      ];
}

if (!eq(tFromJs("variant162"), /* variant162 */34897480)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          772,
          2
        ]
      ];
}

if (!eq(tFromJs("variant163"), /* variant163 */34897481)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          773,
          2
        ]
      ];
}

if (!eq(tFromJs("variant164"), /* variant164 */34897482)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          774,
          2
        ]
      ];
}

if (!eq(tFromJs("variant165"), /* variant165 */34897483)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          775,
          2
        ]
      ];
}

if (!eq(tFromJs("variant166"), /* variant166 */34897484)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          776,
          2
        ]
      ];
}

if (!eq(tFromJs("variant167"), /* variant167 */34897485)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          777,
          2
        ]
      ];
}

if (!eq(tFromJs("variant168"), /* variant168 */34897486)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          778,
          2
        ]
      ];
}

if (!eq(tFromJs("variant169"), /* variant169 */34897487)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          779,
          2
        ]
      ];
}

if (!eq(tFromJs("variant170"), /* variant170 */34897701)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          780,
          2
        ]
      ];
}

if (!eq(tFromJs("variant171"), /* variant171 */34897702)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          781,
          2
        ]
      ];
}

if (!eq(tFromJs("variant172"), /* variant172 */34897703)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          782,
          2
        ]
      ];
}

if (!eq(tFromJs("variant173"), /* variant173 */34897704)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          783,
          2
        ]
      ];
}

if (!eq(tFromJs("variant174"), /* variant174 */34897705)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          784,
          2
        ]
      ];
}

if (!eq(tFromJs("variant175"), /* variant175 */34897706)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          785,
          2
        ]
      ];
}

if (!eq(tFromJs("variant176"), /* variant176 */34897707)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          786,
          2
        ]
      ];
}

if (!eq(tFromJs("variant177"), /* variant177 */34897708)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          787,
          2
        ]
      ];
}

if (!eq(tFromJs("variant178"), /* variant178 */34897709)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          788,
          2
        ]
      ];
}

if (!eq(tFromJs("variant179"), /* variant179 */34897710)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          789,
          2
        ]
      ];
}

if (!eq(tFromJs("variant180"), /* variant180 */34897924)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          790,
          2
        ]
      ];
}

if (!eq(tFromJs("variant181"), /* variant181 */34897925)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          791,
          2
        ]
      ];
}

if (!eq(tFromJs("variant182"), /* variant182 */34897926)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          792,
          2
        ]
      ];
}

if (!eq(tFromJs("variant183"), /* variant183 */34897927)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          793,
          2
        ]
      ];
}

if (!eq(tFromJs("variant184"), /* variant184 */34897928)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          794,
          2
        ]
      ];
}

if (!eq(tFromJs("variant185"), /* variant185 */34897929)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          795,
          2
        ]
      ];
}

if (!eq(tFromJs("variant186"), /* variant186 */34897930)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          796,
          2
        ]
      ];
}

if (!eq(tFromJs("variant187"), /* variant187 */34897931)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          797,
          2
        ]
      ];
}

if (!eq(tFromJs("variant188"), /* variant188 */34897932)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          798,
          2
        ]
      ];
}

if (!eq(tFromJs("variant189"), /* variant189 */34897933)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          799,
          2
        ]
      ];
}

if (!eq(tFromJs("variant190"), /* variant190 */34898147)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          800,
          2
        ]
      ];
}

if (!eq(tFromJs("variant191"), /* variant191 */34898148)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          801,
          2
        ]
      ];
}

if (!eq(tFromJs("variant192"), /* variant192 */34898149)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          802,
          2
        ]
      ];
}

if (!eq(tFromJs("variant193"), /* variant193 */34898150)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          803,
          2
        ]
      ];
}

if (!eq(tFromJs("variant194"), /* variant194 */34898151)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          804,
          2
        ]
      ];
}

if (!eq(tFromJs("variant195"), /* variant195 */34898152)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          805,
          2
        ]
      ];
}

if (!eq(tFromJs("variant196"), /* variant196 */34898153)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          806,
          2
        ]
      ];
}

if (!eq(tFromJs("variant197"), /* variant197 */34898154)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          807,
          2
        ]
      ];
}

if (!eq(tFromJs("variant198"), /* variant198 */34898155)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          808,
          2
        ]
      ];
}

if (!eq(tFromJs("variant199"), /* variant199 */34898156)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          809,
          2
        ]
      ];
}

if (!eq(tFromJs("variant200"), /* variant200 */34945869)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          810,
          2
        ]
      ];
}

if (!eq(tFromJs("variant201"), /* variant201 */34945870)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          811,
          2
        ]
      ];
}

if (!eq(tFromJs("variant202"), /* variant202 */34945871)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          812,
          2
        ]
      ];
}

if (!eq(tFromJs("variant203"), /* variant203 */34945872)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          813,
          2
        ]
      ];
}

if (!eq(tFromJs("variant204"), /* variant204 */34945873)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          814,
          2
        ]
      ];
}

if (!eq(tFromJs("variant205"), /* variant205 */34945874)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          815,
          2
        ]
      ];
}

if (!eq(tFromJs("variant206"), /* variant206 */34945875)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          816,
          2
        ]
      ];
}

if (!eq(tFromJs("variant207"), /* variant207 */34945876)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          817,
          2
        ]
      ];
}

if (!eq(tFromJs("variant208"), /* variant208 */34945877)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          818,
          2
        ]
      ];
}

if (!eq(tFromJs("variant209"), /* variant209 */34945878)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          819,
          2
        ]
      ];
}

if (!eq(tFromJs("variant210"), /* variant210 */34946092)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          820,
          2
        ]
      ];
}

if (!eq(tFromJs("variant211"), /* variant211 */34946093)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          821,
          2
        ]
      ];
}

if (!eq(tFromJs("variant212"), /* variant212 */34946094)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          822,
          2
        ]
      ];
}

if (!eq(tFromJs("variant213"), /* variant213 */34946095)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          823,
          2
        ]
      ];
}

if (!eq(tFromJs("variant214"), /* variant214 */34946096)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          824,
          2
        ]
      ];
}

if (!eq(tFromJs("variant215"), /* variant215 */34946097)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          825,
          2
        ]
      ];
}

if (!eq(tFromJs("variant216"), /* variant216 */34946098)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          826,
          2
        ]
      ];
}

if (!eq(tFromJs("variant217"), /* variant217 */34946099)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          827,
          2
        ]
      ];
}

if (!eq(tFromJs("variant218"), /* variant218 */34946100)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          828,
          2
        ]
      ];
}

if (!eq(tFromJs("variant219"), /* variant219 */34946101)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          829,
          2
        ]
      ];
}

if (!eq(tFromJs("variant220"), /* variant220 */34946315)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          830,
          2
        ]
      ];
}

if (!eq(tFromJs("variant221"), /* variant221 */34946316)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          831,
          2
        ]
      ];
}

if (!eq(tFromJs("variant222"), /* variant222 */34946317)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          832,
          2
        ]
      ];
}

if (!eq(tFromJs("variant223"), /* variant223 */34946318)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          833,
          2
        ]
      ];
}

if (!eq(tFromJs("variant224"), /* variant224 */34946319)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          834,
          2
        ]
      ];
}

if (!eq(tFromJs("variant225"), /* variant225 */34946320)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          835,
          2
        ]
      ];
}

if (!eq(tFromJs("variant226"), /* variant226 */34946321)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          836,
          2
        ]
      ];
}

if (!eq(tFromJs("variant227"), /* variant227 */34946322)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          837,
          2
        ]
      ];
}

if (!eq(tFromJs("variant228"), /* variant228 */34946323)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          838,
          2
        ]
      ];
}

if (!eq(tFromJs("variant229"), /* variant229 */34946324)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          839,
          2
        ]
      ];
}

if (!eq(tFromJs("variant230"), /* variant230 */34946538)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          840,
          2
        ]
      ];
}

if (!eq(tFromJs("variant231"), /* variant231 */34946539)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          841,
          2
        ]
      ];
}

if (!eq(tFromJs("variant232"), /* variant232 */34946540)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          842,
          2
        ]
      ];
}

if (!eq(tFromJs("variant233"), /* variant233 */34946541)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          843,
          2
        ]
      ];
}

if (!eq(tFromJs("variant234"), /* variant234 */34946542)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          844,
          2
        ]
      ];
}

if (!eq(tFromJs("variant235"), /* variant235 */34946543)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          845,
          2
        ]
      ];
}

if (!eq(tFromJs("variant236"), /* variant236 */34946544)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          846,
          2
        ]
      ];
}

if (!eq(tFromJs("variant237"), /* variant237 */34946545)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          847,
          2
        ]
      ];
}

if (!eq(tFromJs("variant238"), /* variant238 */34946546)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          848,
          2
        ]
      ];
}

if (!eq(tFromJs("variant239"), /* variant239 */34946547)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          849,
          2
        ]
      ];
}

if (!eq(tFromJs("variant240"), /* variant240 */34946761)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          850,
          2
        ]
      ];
}

if (!eq(tFromJs("variant241"), /* variant241 */34946762)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          851,
          2
        ]
      ];
}

if (!eq(tFromJs("variant242"), /* variant242 */34946763)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          852,
          2
        ]
      ];
}

if (!eq(tFromJs("variant243"), /* variant243 */34946764)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          853,
          2
        ]
      ];
}

if (!eq(tFromJs("variant244"), /* variant244 */34946765)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          854,
          2
        ]
      ];
}

if (!eq(tFromJs("variant245"), /* variant245 */34946766)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          855,
          2
        ]
      ];
}

if (!eq(tFromJs("variant246"), /* variant246 */34946767)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          856,
          2
        ]
      ];
}

if (!eq(tFromJs("variant247"), /* variant247 */34946768)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          857,
          2
        ]
      ];
}

if (!eq(tFromJs("variant248"), /* variant248 */34946769)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          858,
          2
        ]
      ];
}

if (!eq(tFromJs("variant249"), /* variant249 */34946770)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          859,
          2
        ]
      ];
}

if (!eq(tFromJs("variant250"), /* variant250 */34946984)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          860,
          2
        ]
      ];
}

if (!eq(tFromJs("variant251"), /* variant251 */34946985)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          861,
          2
        ]
      ];
}

if (!eq(tFromJs("variant252"), /* variant252 */34946986)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          862,
          2
        ]
      ];
}

if (!eq(tFromJs("variant253"), /* variant253 */34946987)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          863,
          2
        ]
      ];
}

if (!eq(tFromJs("variant254"), /* variant254 */34946988)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          864,
          2
        ]
      ];
}

if (!eq(tFromJs("variant255"), /* variant255 */34946989)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          865,
          2
        ]
      ];
}

if (!eq(tFromJs("variant256"), /* variant256 */34946990)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          866,
          2
        ]
      ];
}

if (!eq(tFromJs("variant257"), /* variant257 */34946991)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          867,
          2
        ]
      ];
}

if (!eq(tFromJs("variant258"), /* variant258 */34946992)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          868,
          2
        ]
      ];
}

if (!eq(tFromJs("variant259"), /* variant259 */34946993)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          869,
          2
        ]
      ];
}

if (!eq(tFromJs("variant260"), /* variant260 */34947207)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          870,
          2
        ]
      ];
}

if (!eq(tFromJs("variant261"), /* variant261 */34947208)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          871,
          2
        ]
      ];
}

if (!eq(tFromJs("variant262"), /* variant262 */34947209)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          872,
          2
        ]
      ];
}

if (!eq(tFromJs("variant263"), /* variant263 */34947210)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          873,
          2
        ]
      ];
}

if (!eq(tFromJs("variant264"), /* variant264 */34947211)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          874,
          2
        ]
      ];
}

if (!eq(tFromJs("variant265"), /* variant265 */34947212)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          875,
          2
        ]
      ];
}

if (!eq(tFromJs("variant266"), /* variant266 */34947213)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          876,
          2
        ]
      ];
}

if (!eq(tFromJs("variant267"), /* variant267 */34947214)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          877,
          2
        ]
      ];
}

if (!eq(tFromJs("variant268"), /* variant268 */34947215)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          878,
          2
        ]
      ];
}

if (!eq(tFromJs("variant269"), /* variant269 */34947216)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          879,
          2
        ]
      ];
}

if (!eq(tFromJs("variant270"), /* variant270 */34947430)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          880,
          2
        ]
      ];
}

if (!eq(tFromJs("variant271"), /* variant271 */34947431)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          881,
          2
        ]
      ];
}

if (!eq(tFromJs("variant272"), /* variant272 */34947432)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          882,
          2
        ]
      ];
}

if (!eq(tFromJs("variant273"), /* variant273 */34947433)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          883,
          2
        ]
      ];
}

if (!eq(tFromJs("variant274"), /* variant274 */34947434)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          884,
          2
        ]
      ];
}

if (!eq(tFromJs("variant275"), /* variant275 */34947435)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          885,
          2
        ]
      ];
}

if (!eq(tFromJs("variant276"), /* variant276 */34947436)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          886,
          2
        ]
      ];
}

if (!eq(tFromJs("variant277"), /* variant277 */34947437)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          887,
          2
        ]
      ];
}

if (!eq(tFromJs("variant278"), /* variant278 */34947438)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          888,
          2
        ]
      ];
}

if (!eq(tFromJs("variant279"), /* variant279 */34947439)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          889,
          2
        ]
      ];
}

if (!eq(tFromJs("variant280"), /* variant280 */34947653)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          890,
          2
        ]
      ];
}

if (!eq(tFromJs("variant281"), /* variant281 */34947654)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          891,
          2
        ]
      ];
}

if (!eq(tFromJs("variant282"), /* variant282 */34947655)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          892,
          2
        ]
      ];
}

if (!eq(tFromJs("variant283"), /* variant283 */34947656)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          893,
          2
        ]
      ];
}

if (!eq(tFromJs("variant284"), /* variant284 */34947657)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          894,
          2
        ]
      ];
}

if (!eq(tFromJs("variant285"), /* variant285 */34947658)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          895,
          2
        ]
      ];
}

if (!eq(tFromJs("variant286"), /* variant286 */34947659)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          896,
          2
        ]
      ];
}

if (!eq(tFromJs("variant287"), /* variant287 */34947660)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          897,
          2
        ]
      ];
}

if (!eq(tFromJs("variant288"), /* variant288 */34947661)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          898,
          2
        ]
      ];
}

if (!eq(tFromJs("variant289"), /* variant289 */34947662)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          899,
          2
        ]
      ];
}

if (!eq(tFromJs("variant290"), /* variant290 */34947876)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          900,
          2
        ]
      ];
}

if (!eq(tFromJs("variant291"), /* variant291 */34947877)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          901,
          2
        ]
      ];
}

if (!eq(tFromJs("variant292"), /* variant292 */34947878)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          902,
          2
        ]
      ];
}

if (!eq(tFromJs("variant293"), /* variant293 */34947879)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          903,
          2
        ]
      ];
}

if (!eq(tFromJs("variant294"), /* variant294 */34947880)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          904,
          2
        ]
      ];
}

if (!eq(tFromJs("variant295"), /* variant295 */34947881)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          905,
          2
        ]
      ];
}

if (!eq(tFromJs("variant296"), /* variant296 */34947882)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          906,
          2
        ]
      ];
}

if (!eq(tFromJs("variant297"), /* variant297 */34947883)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          907,
          2
        ]
      ];
}

if (!eq(tFromJs("variant298"), /* variant298 */34947884)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          908,
          2
        ]
      ];
}

if (!eq(tFromJs("variant299"), /* variant299 */34947885)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          909,
          2
        ]
      ];
}

if (!eq(tFromJs("xx"), undefined)) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test/big_polyvar_test.ml",
          910,
          2
        ]
      ];
}

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.eq = eq;
/*  Not a pure module */
