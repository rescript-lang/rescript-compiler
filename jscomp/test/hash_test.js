// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes       = require("../stdlib/bytes");
var Hashtbl     = require("../stdlib/hashtbl");
var Mt          = require("./mt");
var Char        = require("../stdlib/char");
var Mt_global   = require("./mt_global");
var $$Array     = require("../stdlib/array");
var Caml_string = require("../runtime/caml_string");

var suites = [/* [] */0];

var test_id = [0];

function eq(f) {
  return function (param, param$1) {
    return Mt_global.collect_eq(test_id, suites, f, param, param$1);
  };
}

var test_strings = $$Array.init(32, function (i) {
      var c = Char.chr(i);
      return Caml_string.bytes_to_string(Bytes.make(i, c));
    });

var test_strings_hash_results = /* array */[
  0,
  904391063,
  889600889,
  929588010,
  596566298,
  365199070,
  448044845,
  311625091,
  681445541,
  634941451,
  82108334,
  17482990,
  491949228,
  696194769,
  711728152,
  594966620,
  820561748,
  958901713,
  102794744,
  378848504,
  349314368,
  114167579,
  71240932,
  110067399,
  280623927,
  323523937,
  310683234,
  178511779,
  585018975,
  544388424,
  1043872806,
  831138595
];

function normalize(x) {
  return x & 1073741823;
}

var param = $$Array.map(function (x) {
      return Hashtbl.hash(x) & 1073741823;
    }, test_strings);

Mt_global.collect_eq(test_id, suites, 'File "hash_test.ml", line 17, characters 5-12', param, test_strings_hash_results);

var param$1 = Hashtbl.hash(0) & 1073741823;

Mt_global.collect_eq(test_id, suites, 'File "hash_test.ml", line 23, characters 5-12', param$1, 129913994);

var param$2 = Hashtbl.hash("x") & 1073741823;

Mt_global.collect_eq(test_id, suites, 'File "hash_test.ml", line 26, characters 5-12', param$2, 780510073);

var param$3 = Hashtbl.hash("xy") & 1073741823;

Mt_global.collect_eq(test_id, suites, 'File "hash_test.ml", line 29, characters 5-12', param$3, 194127723);

Mt.from_pair_suites("hash_test.ml", suites[0]);

exports.suites                    = suites;
exports.test_id                   = test_id;
exports.eq                        = eq;
exports.test_strings              = test_strings;
exports.test_strings_hash_results = test_strings_hash_results;
exports.normalize                 = normalize;
/* test_strings Not a pure module */
