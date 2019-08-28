'use strict';

var Mt = require("./mt.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

b("File \"test_is_js.ml\", line 15, characters 2-9", true);

b("File \"test_is_js.ml\", line 23, characters 2-9", true);

b("File \"test_is_js.ml\", line 37, characters 2-9", true);

Mt.from_pair_suites("Test_is_js", suites.contents);

/*  Not a pure module */
