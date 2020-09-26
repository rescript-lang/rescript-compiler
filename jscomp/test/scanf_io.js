'use strict';

var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Digest = require("../../lib/js/digest.js");
var Printf = require("../../lib/js/printf.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

function create_tscanf_data(ob, lines) {
  var add_line = function (param) {
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Caml_string */3,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%S"
                }), param[0]));
    $$Buffer.add_string(ob, " -> ");
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Caml_string */3,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%S"
                }), param[1]));
    return $$Buffer.add_string(ob, ";\n");
  };
  return List.iter(add_line, lines);
}

function write_tscanf_data_file(fname, lines) {
  var oc = Pervasives.open_out(fname);
  var ob = $$Buffer.create(42);
  create_tscanf_data(ob, lines);
  $$Buffer.output_buffer(oc, ob);
  Caml_io.caml_ml_flush(oc);
  return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
}

function add_digest_ib(ob, ib) {
  var scan_line = function (ib, f) {
    return Curry._1(Scanf.bscanf(ib, /* Format */{
                    _0: {
                      TAG: /* Scan_char_set */20,
                      _0: undefined,
                      _1: "\xff\xdb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                      _2: {
                        TAG: /* Char_literal */12,
                        _0: /* "\n" */10,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "%[^\n\r]\n"
                  }), f);
  };
  var output_line_digest = function (s) {
    $$Buffer.add_string(ob, s);
    $$Buffer.add_char(ob, /* "#" */35);
    var s$1 = Digest.to_hex(Digest.string(s));
    $$Buffer.add_string(ob, Caml_bytes.bytes_to_string(Bytes.uppercase(Caml_bytes.bytes_of_string(s$1))));
    return $$Buffer.add_char(ob, /* "\n" */10);
  };
  try {
    while(true) {
      scan_line(ib, output_line_digest);
    };
    return ;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "End_of_file") {
      return ;
    }
    throw exn;
  }
}

var tscanf_data_file = "tscanf_data";

var tscanf_data_file_lines = {
  hd: [
    "Objective",
    "Caml"
  ],
  tl: /* [] */0
};

exports.tscanf_data_file = tscanf_data_file;
exports.tscanf_data_file_lines = tscanf_data_file_lines;
exports.create_tscanf_data = create_tscanf_data;
exports.write_tscanf_data_file = write_tscanf_data_file;
exports.add_digest_ib = add_digest_ib;
/* Scanf Not a pure module */
