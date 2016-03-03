// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

var stdin = undefined;

var stdout = undefined;

var stderr = undefined;

function caml_ml_open_descriptor_in() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_open_descriptor_in not implemented"
      ];
}

function caml_ml_open_descriptor_out() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_open_descriptor_out not implemented"
      ];
}

function caml_ml_output_char(_, _$1) {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_output_char not implemented"
      ];
}

function caml_ml_output(_, _$1, _$2, _$3) {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_output not implemented"
      ];
}

function caml_ml_input(_, _$1, _$2, _$3) {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_input ic not implemented"
      ];
}

function caml_ml_input_char() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_input_char not implemnted"
      ];
}

function caml_ml_out_channels_list() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "caml_io.ml",
          46,
          2
        ]
      ];
}

function caml_ml_flush() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "caml_io.ml",
          49,
          2
        ]
      ];
}

exports.stdin                       = stdin;
exports.stdout                      = stdout;
exports.stderr                      = stderr;
exports.caml_ml_open_descriptor_in  = caml_ml_open_descriptor_in;
exports.caml_ml_open_descriptor_out = caml_ml_open_descriptor_out;
exports.caml_ml_output_char         = caml_ml_output_char;
exports.caml_ml_output              = caml_ml_output;
exports.caml_ml_input               = caml_ml_input;
exports.caml_ml_input_char          = caml_ml_input_char;
exports.caml_ml_out_channels_list   = caml_ml_out_channels_list;
exports.caml_ml_flush               = caml_ml_flush;
/* stdin Not a pure module */
