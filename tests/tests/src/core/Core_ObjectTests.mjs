// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "./Test.mjs";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Core__Option from "../src/Core__Option.mjs";

var eq = Caml_obj.equal;

Test.run([
      [
        "ObjectTests.res",
        12,
        20,
        30
      ],
      "is: ints"
    ], Object.is(25, 25), eq, true);

Test.run([
      [
        "ObjectTests.res",
        14,
        20,
        33
      ],
      "is: strings"
    ], Object.is("abc", "abc"), eq, true);

Test.run([
      [
        "ObjectTests.res",
        15,
        20,
        33
      ],
      "is: strings"
    ], Object.is("abc", "ABC"), eq, false);

Test.run([
      [
        "ObjectTests.res",
        17,
        20,
        44
      ],
      "is: null and undefined"
    ], Object.is(null, undefined), eq, false);

Test.run([
      [
        "ObjectTests.res",
        18,
        20,
        44
      ],
      "is: null and undefined"
    ], Object.is(undefined, undefined), eq, true);

Test.run([
      [
        "ObjectTests.res",
        19,
        20,
        44
      ],
      "is: null and undefined"
    ], Object.is(null, null), eq, true);

var nums = [
  1,
  2,
  3
];

Test.run([
      [
        "ObjectTests.res",
        22,
        20,
        32
      ],
      "is: arrays"
    ], Object.is([
          1,
          2,
          3
        ], [
          1,
          2,
          3
        ]), eq, false);

Test.run([
      [
        "ObjectTests.res",
        23,
        20,
        32
      ],
      "is: arrays"
    ], Object.is(nums, nums), eq, true);

Test.run([
      [
        "ObjectTests.res",
        24,
        20,
        32
      ],
      "is: arrays"
    ], Caml_obj.equal([
          1,
          2,
          3
        ], [
          1,
          2,
          3
        ]), eq, true);

Test.run([
      [
        "ObjectTests.res",
        25,
        20,
        32
      ],
      "is: arrays"
    ], [
      1,
      2,
      3
    ] === [
      1,
      2,
      3
    ], eq, false);

Test.run([
      [
        "ObjectTests.res",
        27,
        20,
        30
      ],
      "is: list"
    ], Object.is({
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }, {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }), eq, false);

Test.run([
      [
        "ObjectTests.res",
        28,
        20,
        30
      ],
      "is: list"
    ], Caml_obj.equal({
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }, {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }), eq, true);

Test.run([
      [
        "ObjectTests.res",
        29,
        20,
        30
      ],
      "is: list"
    ], ({
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: /* [] */0
          }
        }
      }) === ({
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: /* [] */0
          }
        }
      }), eq, false);

var d = new Date(2000, 1);

Test.run([
      [
        "ObjectTests.res",
        33,
        13,
        23
      ],
      "is: date"
    ], Object.is(new Date(2000, 1), new Date(2000, 1)), eq, false);

Test.run([
      [
        "ObjectTests.res",
        38,
        20,
        30
      ],
      "is: date"
    ], Object.is(d, d), eq, true);

var x = {
  a: 1
};

Test.run([
      [
        "ObjectTests.res",
        41,
        20,
        33
      ],
      "is: objects"
    ], Object.is(x, x), eq, true);

Test.run([
      [
        "ObjectTests.res",
        42,
        20,
        33
      ],
      "is: objects"
    ], Object.is({
          a: 1
        }, {
          a: 1
        }), eq, false);

Test.run([
      [
        "ObjectTests.res",
        43,
        20,
        33
      ],
      "is: objects"
    ], Object.is({}, {}), eq, false);

Test.run([
      [
        "ObjectTests.res",
        44,
        20,
        45
      ],
      "is: === and == operator"
    ], x === x, eq, true);

Test.run([
      [
        "ObjectTests.res",
        45,
        20,
        45
      ],
      "is: === and == operator"
    ], Caml_obj.equal(x, x), eq, true);

Test.run([
      [
        "ObjectTests.res",
        46,
        20,
        45
      ],
      "is: === and == operator"
    ], Caml_obj.equal({
          a: 1
        }, {
          a: 1
        }), eq, true);

Test.run([
      [
        "ObjectTests.res",
        48,
        20,
        31
      ],
      "is: zeros"
    ], Object.is(0, 0), eq, true);

Test.run([
      [
        "ObjectTests.res",
        49,
        20,
        31
      ],
      "is: zeros"
    ], Object.is(-0.0, -0.0), eq, true);

Test.run([
      [
        "ObjectTests.res",
        50,
        20,
        31
      ],
      "is: zeros"
    ], Object.is(0.0, -0.0), eq, false);

function mkBig(s) {
  return BigInt(s);
}

Test.run([
      [
        "ObjectTests.res",
        53,
        20,
        32
      ],
      "is: bigint"
    ], Object.is(BigInt("123456789"), BigInt("123456789")), eq, true);

Test.run([
      [
        "ObjectTests.res",
        54,
        20,
        32
      ],
      "is: bigint"
    ], Object.is(BigInt("123489"), BigInt("123456789")), eq, false);

Test.run([
      [
        "ObjectTests.res",
        55,
        20,
        32
      ],
      "is: bigint"
    ], Object.is(BigInt("000000000"), BigInt("0")), eq, true);

Test.run([
      [
        "ObjectTests.res",
        56,
        20,
        32
      ],
      "is: bigint"
    ], BigInt("123") === BigInt("123"), eq, true);

Test.run([
      [
        "ObjectTests.res",
        57,
        20,
        32
      ],
      "is: bigint"
    ], BigInt("123") === BigInt("123"), eq, true);

Test.run([
      [
        "ObjectTests.res",
        62,
        13,
        50
      ],
      "assign copies from source to target"
    ], Object.assign({
          a: 1,
          b: 2
        }, {
          b: 3,
          c: 0
        }), eq, {
      a: 1,
      b: 3,
      c: 0
    });

function assignOverwritesTarget(title, source) {
  var sourceObj = {
    a: source
  };
  Test.run([
        [
          "ObjectTests.res",
          70,
          22,
          39
        ],
        "assign " + title
      ], Object.assign({
            a: 1
          }, sourceObj), eq, sourceObj);
  Test.run([
        [
          "ObjectTests.res",
          71,
          22,
          39
        ],
        "assign " + title
      ], Object.assign({
            a: undefined
          }, sourceObj), eq, sourceObj);
  Test.run([
        [
          "ObjectTests.res",
          72,
          22,
          39
        ],
        "assign " + title
      ], Object.assign({
            a: null
          }, sourceObj), eq, sourceObj);
}

assignOverwritesTarget("when source is undefined", undefined);

assignOverwritesTarget("when source is null", null);

assignOverwritesTarget("when source is a number", 1);

assignOverwritesTarget("when source is a string", "abc");

function runGetTest(i) {
  Test.run([
        [
          "ObjectTests.res",
          90,
          22,
          46
        ],
        "Object.get: " + i.title
      ], i.get(i.source()), eq, i.expected);
}

runGetTest({
      title: "prop exists, return Some",
      source: (function () {
          return {
                  a: 1
                };
        }),
      get: (function (__x) {
          return __x["a"];
        }),
      expected: 1
    });

runGetTest({
      title: "prop NOT exist, return None",
      source: (function () {
          return {
                  a: 1
                };
        }),
      get: (function (i) {
          return i["banana"];
        }),
      expected: undefined
    });

runGetTest({
      title: "prop like toString, return Some",
      source: (function () {
          return {
                  a: 1
                };
        }),
      get: (function (i) {
          return Core__Option.isSome(i["toString"]);
        }),
      expected: true
    });

runGetTest({
      title: "prop exist but explicitly undefined, return None",
      source: (function () {
          return {
                  a: undefined
                };
        }),
      get: (function (i) {
          return i["a"];
        }),
      expected: undefined
    });

runGetTest({
      title: "prop exist but explicitly null, return None",
      source: (function () {
          return {
                  a: null
                };
        }),
      get: (function (i) {
          return i["a"];
        }),
      expected: null
    });

runGetTest({
      title: "prop exists and is an array, can get it",
      source: (function () {
          return {
                  a: [
                    1,
                    2,
                    3
                  ]
                };
        }),
      get: (function (i) {
          return Core__Option.getOr(Core__Option.map(i["a"], (function (i) {
                            return i.concat([
                                        4,
                                        5
                                      ]);
                          })), []);
        }),
      expected: [
        1,
        2,
        3,
        4,
        5
      ]
    });

function getSymbolTestWhenExists() {
  var obj = {};
  var fruit = Symbol("fruit");
  obj[fruit] = "banana";
  var retrieved = obj[fruit];
  Test.run([
        [
          "ObjectTests.res",
          150,
          15,
          63
        ],
        "Object.getSymbol when exists return it as Some"
      ], retrieved, eq, "banana");
}

getSymbolTestWhenExists();

Test.run([
      [
        "ObjectTests.res",
        159,
        13,
        65
      ],
      "Object.getSymbol when not exists return it as None"
    ], ({})[Symbol("fruit")], eq, undefined);

Test.run([
      [
        "ObjectTests.res",
        168,
        13,
        46
      ],
      "Object.create clones properties"
    ], Object.create({
            a: 1
          })["a"], eq, 1);

Test.run([
      [
        "ObjectTests.res",
        175,
        13,
        46
      ],
      "Object.create clones properties"
    ], Object.create({
            a: 1
          })["b"], eq, undefined);

export {
  eq ,
  nums ,
  d ,
  x ,
  mkBig ,
  assignOverwritesTarget ,
  runGetTest ,
  getSymbolTestWhenExists ,
}
/*  Not a pure module */
