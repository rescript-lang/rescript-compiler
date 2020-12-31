'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Format = require("../../lib/js/format.js");
var Mt_global = require("./mt_global.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Float_array = require("./float_array.js");

var buf = $$Buffer.create(50);

var fmt = Format.formatter_of_buffer(buf);

function print_float(f) {
  return Curry._1(Format.fprintf(fmt, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%s"
                }), Pervasives.string_of_float(f));
}

function print_newline(param) {
  return Format.fprintf(fmt, /* Format */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* '
' */10,
                _1: /* End_of_format */0
              },
              _1: "\n"
            });
}

var s = {
  f: 1.0
};

print_float(1.0);

print_newline(undefined);

var b = Float_array.small_float_array(12);

var c = Float_array.longer_float_array(34);

function print_array(a) {
  $$Array.iter((function (f) {
          print_float(f);
          return print_newline(undefined);
        }), a);
  return print_newline(undefined);
}

print_array(b[0]);

print_array(c[0]);

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(f, a, b) {
  return Mt_global.collect_eq(test_id, suites, f, a, b);
}

eq("File \"tfloat_record_test.ml\", line 43, characters 5-12", $$Buffer.contents(buf), "1.\n1.\n2.\n3.\n\n1.\n2.\n3.\n4.\n5.\n6.\n7.\n8.\n9.\n0.\n1.\n2.\n3.\n4.\n5.\n6.\n7.\n8.\n9.\n0.\n1.\n2.\n3.\n4.\n5.\n6.\n7.\n8.\n9.\n0.\n1.\n2.\n3.\n4.\n5.\n6.\n7.\n8.\n9.\n0.\n\n");

Mt.from_pair_suites("Tfloat_record_test", suites.contents);

exports.buf = buf;
exports.fmt = fmt;
exports.print_float = print_float;
exports.print_newline = print_newline;
exports.s = s;
exports.b = b;
exports.c = c;
exports.print_array = print_array;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* buf Not a pure module */
