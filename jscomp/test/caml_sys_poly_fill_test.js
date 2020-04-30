'use strict';

var Mt = require("./mt.js");
var Sys = require("../../lib/js/sys.js");
var Block = require("../../lib/js/block.js");
var Caml_sys = require("../../lib/js/caml_sys.js");
var Node_process = require("../../lib/js/node_process.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

Node_process.putEnvVar("Caml_sys_poly_fill_test", "X");

var v = Caml_sys.caml_sys_getenv("Caml_sys_poly_fill_test");

eq("File \"caml_sys_poly_fill_test.ml\", line 11, characters 5-12", "X", (Node_process.deleteEnvVar("Caml_sys_poly_fill_test"), v));

Node_process.putEnvVar("Caml_sys_poly_fill_test", "Y");

var v$1 = Caml_sys.caml_sys_getenv("Caml_sys_poly_fill_test");

eq("File \"caml_sys_poly_fill_test.ml\", line 17, characters 5-12", "Y", (Node_process.deleteEnvVar("Caml_sys_poly_fill_test"), v$1));

Node_process.deleteEnvVar("Caml_sys_poly_fill_test");

var tmp;

try {
  tmp = Caml_sys.caml_sys_getenv("Caml_sys_poly_fill_test");
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn.ExceptionID === "Not_found") {
    tmp = "Z";
  } else {
    throw exn;
  }
}

eq("File \"caml_sys_poly_fill_test.ml\", line 23, characters 5-12", "Z", tmp);

console.log(/* tuple */[
      Caml_sys.caml_sys_getcwd(undefined),
      Caml_sys.caml_sys_time(undefined),
      Sys.argv,
      Sys.executable_name
    ]);

Mt.from_pair_suites("Caml_sys_poly_fill_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
