// GENERATED CODE BY BUCKLESCRIPT VERSION 0.5.5 , PLEASE EDIT WITH CARE
'use strict';

var Mt           = require("./mt");
var Block        = require("../block");
var Ext_filename = require("./ext_filename");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

eq('File "a_filename_test.ml", line 10, characters 5-12', /* tuple */[
      Ext_filename.combine("/tmp", "subdir/file.txt"),
      Ext_filename.combine("/tmp", "/a/tmp.txt"),
      Ext_filename.combine("/a/tmp.txt", "subdir/file.txt")
    ], /* tuple */[
      "/tmp/subdir/file.txt",
      "/a/tmp.txt",
      "/a/tmp.txt/subdir/file.txt"
    ]);

eq('File "a_filename_test.ml", line 22, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/b.c"
        ], /* `File */[
          781515420,
          "./a/u/g.c"
        ]), "./u/g");

eq('File "a_filename_test.ml", line 27, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/b.c"
        ], /* `File */[
          781515420,
          "xxxghsoghos/ghsoghso/node_modules/buckle-stdlib/list.js"
        ]), "buckle-stdlib/list.js");

eq('File "a_filename_test.ml", line 33, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/b.c"
        ], /* `File */[
          781515420,
          "xxxghsoghos/ghsoghso/node_modules//buckle-stdlib/list.js"
        ]), "buckle-stdlib/list.js");

eq('File "a_filename_test.ml", line 39, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/b.c"
        ], /* `File */[
          781515420,
          "xxxghsoghos/ghsoghso/node_modules/./buckle-stdlib/list.js"
        ]), "buckle-stdlib/list.js");

eq('File "a_filename_test.ml", line 45, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/c.js"
        ], /* `File */[
          781515420,
          "./a/b"
        ]), "./b");

eq('File "a_filename_test.ml", line 50, characters 5-12', Ext_filename.node_relative_path(/* `File */[
          781515420,
          "./a/c"
        ], /* `File */[
          781515420,
          "./a/b.js"
        ]), "./b");

eq('File "a_filename_test.ml", line 55, characters 5-12', Ext_filename.node_relative_path(/* `Dir */[
          3405101,
          "./a/"
        ], /* `File */[
          781515420,
          "./a/b.js"
        ]), "./b");

Mt.from_pair_suites("a_filename_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
