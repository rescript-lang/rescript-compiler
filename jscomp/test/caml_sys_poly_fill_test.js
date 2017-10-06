'use strict';

var Mt                      = require("./mt.js");
var Block                   = require("../../lib/js/block.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_sys                = require("../../lib/js/caml_sys.js");
var Filename                = require("../../lib/js/filename.js");
var Caml_sys_fs             = require("../../lib/js/caml_sys_fs.js");
var Node_process            = require("../../lib/js/node_process.js");
var Caml_sys_system         = require("../../lib/js/caml_sys_system.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
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

Node_process.putEnvVar("caml_sys_poly_fill_test.ml", "X");

var v = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");

eq("File \"caml_sys_poly_fill_test.ml\", line 12, characters 5-12", "X", (Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml"), v));

Node_process.putEnvVar("caml_sys_poly_fill_test.ml", "Y");

var v$1 = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");

eq("File \"caml_sys_poly_fill_test.ml\", line 18, characters 5-12", "Y", (Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml"), v$1));

Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml");

var tmp;

try {
  tmp = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");
}
catch (exn){
  tmp = "Z";
}

eq("File \"caml_sys_poly_fill_test.ml\", line 24, characters 5-12", "Z", tmp);

eq("File \"caml_sys_poly_fill_test.ml\", line 31, characters 5-12", /* true */1, Caml_sys_fs.caml_sys_is_directory("."));

eq("File \"caml_sys_poly_fill_test.ml\", line 34, characters 5-12", /* false */0, Caml_sys_fs.caml_sys_is_directory("Makefile"));

var tmp$1;

try {
  Caml_sys_fs.caml_sys_is_directory("path_that_does_not_exist");
  tmp$1 = "no_error";
}
catch (raw_exn){
  var exn$1 = Js_exn.internalToOCamlException(raw_exn);
  if (exn$1[0] === Caml_builtin_exceptions.sys_error) {
    tmp$1 = "sys_error";
  } else {
    throw exn$1;
  }
}

eq("File \"caml_sys_poly_fill_test.ml\", line 37, characters 5-12", "sys_error", tmp$1);

eq("File \"caml_sys_poly_fill_test.ml\", line 48, characters 5-12", /* true */1, Caml_sys_fs.caml_sys_file_exists("."));

eq("File \"caml_sys_poly_fill_test.ml\", line 51, characters 5-12", /* false */0, Caml_sys_fs.caml_sys_file_exists("path_that_does_not_exist"));

eq("File \"caml_sys_poly_fill_test.ml\", line 56, characters 5-12", 0, Caml_sys_system.caml_sys_system_command("true"));

eq("File \"caml_sys_poly_fill_test.ml\", line 59, characters 5-12", 0, Caml_sys_system.caml_sys_system_command("type true"));

eq("File \"caml_sys_poly_fill_test.ml\", line 63, characters 5-12", 1, Caml_sys_system.caml_sys_system_command("false"));

eq("File \"caml_sys_poly_fill_test.ml\", line 66, characters 5-12", 127, Caml_sys_system.caml_sys_system_command("not_a_real_command"));

eq("File \"caml_sys_poly_fill_test.ml\", line 70, characters 5-12", 0, (Filename.open_temp_file(/* None */0, /* None */0, "pre.", ".txt"), 0));

Mt.from_pair_suites("caml_sys_poly_fill_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
