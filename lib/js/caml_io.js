'use strict';

var Curry = require("./curry.js");

var stdout = /* record */[
  /* buffer */"",
  /* output */(function (param, s) {
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
  /* buffer */"",
  /* output */(function (param, s) {
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

function caml_ml_flush(oc) {
  if (oc[/* buffer */0] !== "") {
    Curry._2(oc[/* output */1], oc, oc[/* buffer */0]);
    oc[/* buffer */0] = "";
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
      oc[/* buffer */0] = oc[/* buffer */0] + str$1;
      return /* () */0;
    } else {
      oc[/* buffer */0] = oc[/* buffer */0] + str$1.slice(0, id + 1 | 0);
      caml_ml_flush(oc);
      oc[/* buffer */0] = oc[/* buffer */0] + str$1.slice(id + 1 | 0);
      return /* () */0;
    }
  }
}

function caml_ml_output_char(oc, $$char) {
  return caml_ml_output(oc, String.fromCharCode($$char), 0, 1);
}

function caml_ml_out_channels_list(param) {
  return /* :: */[
          stdout,
          /* :: */[
            stderr,
            /* [] */0
          ]
        ];
}

var stdin = undefined;

exports.stdin = stdin;
exports.stdout = stdout;
exports.stderr = stderr;
exports.caml_ml_flush = caml_ml_flush;
exports.caml_ml_output = caml_ml_output;
exports.caml_ml_output_char = caml_ml_output_char;
exports.caml_ml_out_channels_list = caml_ml_out_channels_list;
/* No side effect */
