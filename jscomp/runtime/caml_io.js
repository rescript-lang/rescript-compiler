// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");
var Caml_curry              = require("./caml_curry");

var stdin = undefined;

var stdout = /* record */[
  "",
  function (_, s) {
    var v = s.length - 1;
    if (( process && process.stdout && process.stdout.write)) {
      return ( process.stdout.write )(s);
    }
    else if (s[v] === "\n") {
      console.log(s.slice(0, v));
      return /* () */0;
    }
    else {
      console.log(s);
      return /* () */0;
    }
  }
];

var stderr = /* record */[
  "",
  function (_, s) {
    var v = s.length - 1;
    if (s[v] === "\n") {
      console.log(s.slice(0, v));
      return /* () */0;
    }
    else {
      console.log(s);
      return /* () */0;
    }
  }
];

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

function caml_ml_flush(oc) {
  if (oc[/* buffer */0] !== "") {
    Caml_curry.app2(oc[/* output */1], oc, oc[/* buffer */0]);
    oc[/* buffer */0] = "";
    return /* () */0;
  }
  else {
    return 0;
  }
}

var node_std_output = (function (s){
   return process && process.stdout && (process.stdout.write(s), true);
   }
);

function caml_ml_output(oc, str, offset, len) {
  var str$1 = offset === 0 && len === str.length ? str : str.slice(offset, len);
  if ((process && process.stdout && process.stdout.write ) && oc === stdout) {
    return (process.stdout.write)(str$1);
  }
  else {
    var id = str$1.lastIndexOf("\n");
    if (id < 0) {
      oc[/* buffer */0] = oc[/* buffer */0] + str$1;
      return /* () */0;
    }
    else {
      oc[/* buffer */0] = oc[/* buffer */0] + str$1.slice(0, id + 1);
      caml_ml_flush(oc);
      oc[/* buffer */0] = oc[/* buffer */0] + str$1.slice(id + 1);
      return /* () */0;
    }
  }
}

function caml_ml_output_char(oc, $$char) {
  return function (param, param$1) {
    return caml_ml_output(oc, String.fromCharCode($$char), param, param$1);
  };
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
          108,
          2
        ]
      ];
}

exports.stdin                       = stdin;
exports.stdout                      = stdout;
exports.stderr                      = stderr;
exports.caml_ml_open_descriptor_in  = caml_ml_open_descriptor_in;
exports.caml_ml_open_descriptor_out = caml_ml_open_descriptor_out;
exports.caml_ml_flush               = caml_ml_flush;
exports.node_std_output             = node_std_output;
exports.caml_ml_output              = caml_ml_output;
exports.caml_ml_output_char         = caml_ml_output_char;
exports.caml_ml_input               = caml_ml_input;
exports.caml_ml_input_char          = caml_ml_input_char;
exports.caml_ml_out_channels_list   = caml_ml_out_channels_list;
/* stdin Not a pure module */
