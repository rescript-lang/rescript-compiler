// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt           = require("./mt");
var Block        = require("../runtime/block");
var Ext_filename = require("./ext_filename");

var suites_000 = /* tuple */[
  "basic",
  function () {
    return /* Eq */Block.__(0, [
              Ext_filename.node_relative_path("./a/b.c", "./a/u/g.c"),
              "./u/g"
            ]);
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("a_filename_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
