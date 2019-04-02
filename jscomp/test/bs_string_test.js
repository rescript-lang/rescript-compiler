'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_Option = require("../../lib/js/belt_Option.js");
var Belt_String = require("../../lib/js/belt_String.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

eq("File \"bs_string_test.ml\", line 11, characters 5-12", "ghso ghso g".split(" ").reduce((function (x, y) {
            return x + ("-" + y);
          }), ""), "-ghso-ghso-g");

eq("File \"bs_string_test.ml\", line 18, characters 5-12", "foo".length, 3);

eq("File \"bs_string_test.ml\", line 19, characters 5-12", "".length, 0);

eq("File \"bs_string_test.ml\", line 22, characters 5-12", "foo"[1], "o");

eq("File \"bs_string_test.ml\", line 23, characters 5-12", "foo"[5], undefined);

eq("File \"bs_string_test.ml\", line 26, characters 5-12", "foo".concat("bar"), "foobar");

eq("File \"bs_string_test.ml\", line 29, characters 5-12", "foo".concat("bar", "baz"), "foobarbaz");

eq("File \"bs_string_test.ml\", line 30, characters 5-12", "".concat(), "");

eq("File \"bs_string_test.ml\", line 33, characters 5-12", "foo".endsWith("oo"), true);

eq("File \"bs_string_test.ml\", line 34, characters 5-12", "foo".endsWith("a"), false);

eq("File \"bs_string_test.ml\", line 37, characters 5-12", Belt_String.indexOf("foo", "oo"), 1);

eq("File \"bs_string_test.ml\", line 38, characters 5-12", Belt_String.indexOf("foo", "a"), undefined);

eq("File \"bs_string_test.ml\", line 41, characters 5-12", "foo".includes("oo"), true);

eq("File \"bs_string_test.ml\", line 42, characters 5-12", "foo".includes("a"), false);

eq("File \"bs_string_test.ml\", line 45, characters 5-12", "a".repeat(3), "aaa");

eq("File \"bs_string_test.ml\", line 46, characters 5-12", "a".repeat(0), "");

eq("File \"bs_string_test.ml\", line 49, characters 5-12", "hello world".replace("world", "you"), "hello you");

eq("File \"bs_string_test.ml\", line 50, characters 5-12", "hello world".replace("foo", "you"), "hello world");

eq("File \"bs_string_test.ml\", line 53, characters 5-12", "hello world".replace((/world/), "you"), "hello you");

eq("File \"bs_string_test.ml\", line 54, characters 5-12", "hello world world".replace((/world/g), "you"), "hello you you");

eq("File \"bs_string_test.ml\", line 55, characters 5-12", "hello world".replace((/foo/g), "you"), "hello world");

eq("File \"bs_string_test.ml\", line 59, characters 5-12", Caml_option.null_to_opt("hello world".match((/world/))) !== undefined, true);

eq("File \"bs_string_test.ml\", line 60, characters 5-12", Belt_Option.map(Caml_option.null_to_opt("hello world".match((/world/))), (function (x) {
            return x.length;
          })), 1);

eq("File \"bs_string_test.ml\", line 61, characters 5-12", Belt_Option.flatMap(Caml_option.null_to_opt("hello world".match((/world/))), (function (x) {
            return Belt_Array.get(x, 0);
          })), "world");

eq("File \"bs_string_test.ml\", line 62, characters 5-12", Caml_option.null_to_opt("hello world".match((/notfound/))), undefined);

eq("File \"bs_string_test.ml\", line 65, characters 5-12", "hello world foo".split(" "), /* array */[
      "hello",
      "world",
      "foo"
    ]);

eq("File \"bs_string_test.ml\", line 68, characters 5-12", "hello world foo".split(" ", 1), /* array */["hello"]);

eq("File \"bs_string_test.ml\", line 71, characters 5-12", "hello world".startsWith("hello"), true);

eq("File \"bs_string_test.ml\", line 72, characters 5-12", "hello world".startsWith("world"), false);

eq("File \"bs_string_test.ml\", line 75, characters 5-12", "hello world".substr(1, 3), "ell");

eq("File \"bs_string_test.ml\", line 78, characters 5-12", "hello world".substr(1), "ello world");

eq("File \"bs_string_test.ml\", line 79, characters 5-12", "hello world".substr(11), "");

eq("File \"bs_string_test.ml\", line 82, characters 5-12", "hello world".slice(1, 3), "el");

eq("File \"bs_string_test.ml\", line 83, characters 5-12", "hello world".slice(11, 12), "");

eq("File \"bs_string_test.ml\", line 86, characters 5-12", "hello world".slice(1), "ello world");

eq("File \"bs_string_test.ml\", line 87, characters 5-12", "hello world".slice(11), "");

eq("File \"bs_string_test.ml\", line 90, characters 5-12", "  hello world   ".trim(), "hello world");

eq("File \"bs_string_test.ml\", line 91, characters 5-12", "\n\r\t hello world\n\r\t ".trim(), "hello world");

eq("File \"bs_string_test.ml\", line 94, characters 5-12", "  hello world   ".trimStart(), "hello world   ");

eq("File \"bs_string_test.ml\", line 95, characters 5-12", "\n\r\t hello world\n\r\t ".trimStart(), "hello world\n\r\t ");

eq("File \"bs_string_test.ml\", line 98, characters 5-12", "  hello world   ".trimEnd(), "  hello world");

eq("File \"bs_string_test.ml\", line 99, characters 5-12", "\n\r\t hello world\n\r\t ".trimEnd(), "\n\r\t hello world");

eq("File \"bs_string_test.ml\", line 102, characters 5-12", "4".padStart(4, "x"), "xxx4");

eq("File \"bs_string_test.ml\", line 103, characters 5-12", "4444".padStart(4, "x"), "4444");

eq("File \"bs_string_test.ml\", line 104, characters 5-12", "4".padStart(4, "xy"), "xyx4");

eq("File \"bs_string_test.ml\", line 107, characters 5-12", "4".padEnd(4, "x"), "4xxx");

eq("File \"bs_string_test.ml\", line 108, characters 5-12", "4444".padEnd(4, "x"), "4444");

eq("File \"bs_string_test.ml\", line 109, characters 5-12", "4".padEnd(4, "xy"), "4xyx");

eq("File \"bs_string_test.ml\", line 112, characters 5-12", "HeLLo WorLd".toLowerCase(), "hello world");

eq("File \"bs_string_test.ml\", line 115, characters 5-12", "HeLLo WorLd".toUpperCase(), "HELLO WORLD");

Mt.from_pair_suites("bs_string_test.ml", suites[0]);

var S = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.S = S;
/*  Not a pure module */
