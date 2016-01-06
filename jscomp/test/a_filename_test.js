// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Mt = require("./mt");
var Ext_filename = require("./ext_filename");

var suites_001 = [
  /* tuple */0,
  "basic",
  function () {
    return Mt.assert_equal(Ext_filename.node_relative_path("./a/b.c", "./a/u/g.c"), "./u/g");
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_suites("a_filename_test.ml", suites);

exports.suites = suites;
/*  fail the pure module */
