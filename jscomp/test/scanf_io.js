'use strict';

var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Digest = require("../../lib/js/digest.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Printf = require("../../lib/js/printf.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_missing_polyfill = require("../../lib/js/caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var tscanf_data_file = "tscanf_data";

var tscanf_data_file_lines = /* :: */[
  /* tuple */[
    "Objective",
    "Caml"
  ],
  /* [] */0
];

function create_tscanf_data(ob, lines) {
  var add_line = function (param) {
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */[
                  /* Caml_string */Block.__(3, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%S"
                ]), param[0]));
    $$Buffer.add_string(ob, " -> ");
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */[
                  /* Caml_string */Block.__(3, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%S"
                ]), param[1]));
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
  return Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
}

function get_lines(fname) {
  var ib = Scanf.Scanning[/* from_file */4](fname);
  var l = [/* [] */0];
  try {
    while(!Scanf.Scanning[/* end_of_input */9](ib)) {
      Curry._1(Scanf.bscanf(ib, /* Format */[
                /* Char_literal */Block.__(12, [
                    /* " " */32,
                    /* Caml_string */Block.__(3, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            " -> ",
                            /* Caml_string */Block.__(3, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    "; ",
                                    /* End_of_format */0
                                  ])
                              ])
                          ])
                      ])
                  ]),
                " %S -> %S; "
              ]), (function (x, y) {
              l[0] = /* :: */[
                /* tuple */[
                  x,
                  y
                ],
                l[0]
              ];
              return /* () */0;
            }));
    };
    return List.rev(l[0]);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Scanf.Scan_failure) {
      var s = Curry._2(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "in file ",
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            ", ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ])
                      ])
                  ]),
                "in file %s, %s"
              ]), fname, exn[1]);
      throw [
            Caml_builtin_exceptions.failure,
            s
          ];
    } else if (exn === Caml_builtin_exceptions.end_of_file) {
      var s$1 = Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "in file ",
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            ", unexpected end of file",
                            /* End_of_format */0
                          ])
                      ])
                  ]),
                "in file %s, unexpected end of file"
              ]), fname);
      throw [
            Caml_builtin_exceptions.failure,
            s$1
          ];
    } else {
      throw exn;
    }
  }
}

function add_digest_ib(ob, ib) {
  var scan_line = function (ib, f) {
    return Curry._1(Scanf.bscanf(ib, /* Format */[
                    /* Scan_char_set */Block.__(20, [
                        /* None */0,
                        "\xff\xdb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                        /* Char_literal */Block.__(12, [
                            /* "\n" */10,
                            /* End_of_format */0
                          ])
                      ]),
                    "%[^\n\r]\n"
                  ]), f);
  };
  var output_line_digest = function (s) {
    $$Buffer.add_string(ob, s);
    $$Buffer.add_char(ob, /* "#" */35);
    var s$1 = Digest.to_hex(Digest.string(s));
    $$Buffer.add_string(ob, Caml_string.bytes_to_string(Bytes.uppercase(Caml_string.bytes_of_string(s$1))));
    return $$Buffer.add_char(ob, /* "\n" */10);
  };
  try {
    while(true) {
      scan_line(ib, output_line_digest);
    };
    return /* () */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.end_of_file) {
      return /* () */0;
    } else {
      throw exn;
    }
  }
}

function digest_file(fname) {
  var ib = Scanf.Scanning[/* from_file */4](fname);
  var ob = $$Buffer.create(42);
  add_digest_ib(ob, ib);
  return $$Buffer.contents(ob);
}

function test54() {
  return Caml_obj.caml_equal(get_lines(tscanf_data_file), tscanf_data_file_lines);
}

function test55() {
  var ob = $$Buffer.create(42);
  create_tscanf_data(ob, tscanf_data_file_lines);
  var s = $$Buffer.contents(ob);
  ob[/* position */1] = 0;
  var ib = Scanf.Scanning[/* from_string */6](s);
  add_digest_ib(ob, ib);
  var tscanf_data_file_lines_digest = $$Buffer.contents(ob);
  return digest_file(tscanf_data_file) === tscanf_data_file_lines_digest;
}

exports.tscanf_data_file = tscanf_data_file;
exports.tscanf_data_file_lines = tscanf_data_file_lines;
exports.create_tscanf_data = create_tscanf_data;
exports.write_tscanf_data_file = write_tscanf_data_file;
exports.get_lines = get_lines;
exports.add_digest_ib = add_digest_ib;
exports.digest_file = digest_file;
exports.test54 = test54;
exports.test55 = test55;
/* Scanf Not a pure module */
