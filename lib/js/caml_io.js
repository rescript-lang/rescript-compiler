'use strict';

var Curry                   = require("./curry.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function $caret(prim, prim$1) {
  return prim + prim$1;
}

var stdin = /* record */[
  /* fd : Some */[0],
  /* offset */0,
  /* buffer : None */0,
  /* curr */0,
  /* max */0
];

var stdout = /* record */[
  /* fd : Some */[1],
  /* buffer */"",
  /* output */(function (_, s) {
      var v = s.length - 1 | 0;
      if (( (typeof process !== "undefined") && process.stdout && process.stdout.write)) {
        return ( process.stdout.write )(s);
      } else if (s[v] === "\n") {
        console.log(s.slice(0, v));
        return /* () */0;
      } else {
        console.log(s);
        return /* () */0;
      }
    })
];

var stderr = /* record */[
  /* fd : Some */[2],
  /* buffer */"",
  /* output */(function (_, s) {
      var v = s.length - 1 | 0;
      if (s[v] === "\n") {
        console.log(s.slice(0, v));
        return /* () */0;
      } else {
        console.log(s);
        return /* () */0;
      }
    })
];

function caml_ml_open_descriptor_in() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_ml_open_descriptor_in not implemented"
      ];
}

function caml_ml_flush(oc) {
  if (oc[/* buffer */1] !== "") {
    Curry._2(oc[/* output */2], oc, oc[/* buffer */1]);
    oc[/* buffer */1] = "";
    return /* () */0;
  } else {
    return 0;
  }
}

function caml_ml_output(oc, str, offset, len) {
  var str$1 = offset === 0 && len === str.length ? str : str.slice(offset, len);
  if (( (typeof process !== "undefined") && process.stdout && process.stdout.write ) && oc === stdout) {
    return ( process.stdout.write )(str$1);
  } else {
    var id = str$1.lastIndexOf("\n");
    if (id < 0) {
      oc[/* buffer */1] = oc[/* buffer */1] + str$1;
      return /* () */0;
    } else {
      oc[/* buffer */1] = oc[/* buffer */1] + str$1.slice(0, id + 1 | 0);
      caml_ml_flush(oc);
      oc[/* buffer */1] = oc[/* buffer */1] + str$1.slice(id + 1 | 0);
      return /* () */0;
    }
  }
}

function caml_ml_output_char(oc, $$char) {
  return caml_ml_output(oc, String.fromCharCode($$char), 0, 1);
}

function caml_ml_out_channels_list() {
  return /* :: */[
          stdout,
          /* :: */[
            stderr,
            /* [] */0
          ]
        ];
}

exports.$caret                     = $caret;
exports.stdin                      = stdin;
exports.stdout                     = stdout;
exports.stderr                     = stderr;
exports.caml_ml_open_descriptor_in = caml_ml_open_descriptor_in;
exports.caml_ml_flush              = caml_ml_flush;
exports.caml_ml_output             = caml_ml_output;
exports.caml_ml_output_char        = caml_ml_output_char;
exports.caml_ml_out_channels_list  = caml_ml_out_channels_list;
/* No side effect */
