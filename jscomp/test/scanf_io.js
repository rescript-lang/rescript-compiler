// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_io                 = require("../runtime/caml_io");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Bytes                   = require("../stdlib/bytes");
var Caml_obj                = require("../runtime/caml_obj");
var Pervasives              = require("../stdlib/pervasives");
var Digest                  = require("../stdlib/digest");
var Curry                   = require("../runtime/curry");
var Printf                  = require("../stdlib/printf");
var Scanf                   = require("../stdlib/scanf");
var Caml_primitive          = require("../runtime/caml_primitive");
var Buffer                  = require("../stdlib/buffer");
var List                    = require("../stdlib/list");
var Caml_string             = require("../runtime/caml_string");

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
    Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */[
                  /* Caml_string */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 3
                  },
                  "%S"
                ]), param[0]));
    Buffer.add_string(ob, " -> ");
    Buffer.add_string(ob, Curry._1(Printf.sprintf(/* Format */[
                  /* Caml_string */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 3
                  },
                  "%S"
                ]), param[1]));
    return Buffer.add_string(ob, ";\n");
  };
  return List.iter(add_line, lines);
}

function write_tscanf_data_file(fname, lines) {
  var oc = Pervasives.open_out(fname);
  var ob = Buffer.create(42);
  create_tscanf_data(ob, lines);
  Buffer.output_buffer(oc, ob);
  Caml_io.caml_ml_flush(oc);
  return Caml_primitive.caml_ml_close_channel(oc);
}

function get_lines(fname) {
  var ib = Curry._1(Scanf.Scanning[/* from_file */4], fname);
  var l = [/* [] */0];
  try {
    while(!Curry._1(Scanf.Scanning[/* end_of_input */9], ib)) {
      Curry._1(Scanf.bscanf(ib, /* Format */[
                /* Char_literal */{
                  0: /* " " */32,
                  1: /* Caml_string */{
                    0: /* No_padding */0,
                    1: /* String_literal */{
                      0: " -> ",
                      1: /* Caml_string */{
                        0: /* No_padding */0,
                        1: /* String_literal */{
                          0: "; ",
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 11
                        },
                        length: 2,
                        tag: 3
                      },
                      length: 2,
                      tag: 11
                    },
                    length: 2,
                    tag: 3
                  },
                  length: 2,
                  tag: 12
                },
                " %S -> %S; "
              ]), function (x, y) {
            l[0] = /* :: */[
              /* tuple */[
                x,
                y
              ],
              l[0]
            ];
            return /* () */0;
          });
    };
    return List.rev(l[0]);
  }
  catch (exn){
    if (exn[0] === Scanf.Scan_failure) {
      var s = Curry._2(Printf.sprintf(/* Format */[
                /* String_literal */{
                  0: "in file ",
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* String_literal */{
                      0: ", ",
                      1: /* String */{
                        0: /* No_padding */0,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 2
                      },
                      length: 2,
                      tag: 11
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 11
                },
                "in file %s, %s"
              ]), fname, exn[1]);
      throw [
            Caml_builtin_exceptions.failure,
            s
          ];
    }
    else if (exn === Caml_builtin_exceptions.end_of_file) {
      var s$1 = Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */{
                  0: "in file ",
                  1: /* String */{
                    0: /* No_padding */0,
                    1: /* String_literal */{
                      0: ", unexpected end of file",
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 11
                    },
                    length: 2,
                    tag: 2
                  },
                  length: 2,
                  tag: 11
                },
                "in file %s, unexpected end of file"
              ]), fname);
      throw [
            Caml_builtin_exceptions.failure,
            s$1
          ];
    }
    else {
      throw exn;
    }
  }
}

function add_digest_ib(ob, ib) {
  var scan_line = function (ib, f) {
    return Curry._1(Scanf.bscanf(ib, /* Format */[
                    /* Scan_char_set */{
                      0: /* None */0,
                      1: "\xff\xdb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                      2: /* Char_literal */{
                        0: /* "\n" */10,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 3,
                      tag: 20
                    },
                    "%[^\n\r]\n"
                  ]), f);
  };
  var output_line_digest = function (s) {
    Buffer.add_string(ob, s);
    Buffer.add_char(ob, /* "#" */35);
    var s$1 = Digest.to_hex(Digest.string(s));
    Buffer.add_string(ob, Caml_string.bytes_to_string(Bytes.uppercase(Caml_string.bytes_of_string(s$1))));
    return Buffer.add_char(ob, /* "\n" */10);
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
    }
    else {
      throw exn;
    }
  }
}

function digest_file(fname) {
  var ib = Curry._1(Scanf.Scanning[/* from_file */4], fname);
  var ob = Buffer.create(42);
  add_digest_ib(ob, ib);
  return Buffer.contents(ob);
}

function test54() {
  return Caml_obj.caml_equal(get_lines(tscanf_data_file), tscanf_data_file_lines);
}

function test55() {
  var ob = Buffer.create(42);
  create_tscanf_data(ob, tscanf_data_file_lines);
  var s = Buffer.contents(ob);
  ob[/* position */1] = 0;
  var ib = Curry._1(Scanf.Scanning[/* from_string */6], s);
  add_digest_ib(ob, ib);
  var tscanf_data_file_lines_digest = Buffer.contents(ob);
  return +(digest_file(tscanf_data_file) === tscanf_data_file_lines_digest);
}

exports.tscanf_data_file       = tscanf_data_file;
exports.tscanf_data_file_lines = tscanf_data_file_lines;
exports.create_tscanf_data     = create_tscanf_data;
exports.write_tscanf_data_file = write_tscanf_data_file;
exports.get_lines              = get_lines;
exports.add_digest_ib          = add_digest_ib;
exports.digest_file            = digest_file;
exports.test54                 = test54;
exports.test55                 = test55;
/* Scanf Not a pure module */
