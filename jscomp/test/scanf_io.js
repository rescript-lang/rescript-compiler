'use strict';

var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Digest = require("../../lib/js/digest.js");
var Printf = require("../../lib/js/printf.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var tscanf_data_file = "tscanf_data";

var tscanf_data_file_lines = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "Objective",
    "Caml"
  ],
  Arg1: "[]"
};

function create_tscanf_data(ob, lines) {
  var add_line = function (param) {
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  },
                  Arg1: "%S"
                }), param[0]));
    $$Buffer.add_string(ob, " -> ");
    $$Buffer.add_string(ob, Curry._1(Printf.sprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  },
                  Arg1: "%S"
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

function get_lines(fname) {
  var ib = Scanf.Scanning.from_file(fname);
  var l = /* record */[/* contents */"[]"];
  try {
    while(!Scanf.Scanning.end_of_input(ib)) {
      Curry._1(Scanf.bscanf(ib, /* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* " " */32,
                  Arg1: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "String_literal",
                      Arg0: " -> ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: "; ",
                          Arg1: "End_of_format"
                        }
                      }
                    }
                  }
                },
                Arg1: " %S -> %S; "
              }), (function (x, y) {
              l[0] = /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  x,
                  y
                ],
                Arg1: l[0]
              };
              return /* () */0;
            }));
    };
    return List.rev(l[0]);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Scanf.Scan_failure) {
      var s = Curry._2(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "in file ",
                  Arg1: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "String_literal",
                      Arg0: ", ",
                      Arg1: /* constructor */{
                        tag: "String",
                        Arg0: "No_padding",
                        Arg1: "End_of_format"
                      }
                    }
                  }
                },
                Arg1: "in file %s, %s"
              }), fname, exn[1]);
      throw [
            Caml_builtin_exceptions.failure,
            s
          ];
    } else if (exn === Caml_builtin_exceptions.end_of_file) {
      var s$1 = Curry._1(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "in file ",
                  Arg1: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "String_literal",
                      Arg0: ", unexpected end of file",
                      Arg1: "End_of_format"
                    }
                  }
                },
                Arg1: "in file %s, unexpected end of file"
              }), fname);
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
    return Curry._1(Scanf.bscanf(ib, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "Scan_char_set",
                      Arg0: undefined,
                      Arg1: "\xff\xdb\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
                      Arg2: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* "\n" */10,
                        Arg1: "End_of_format"
                      }
                    },
                    Arg1: "%[^\n\r]\n"
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
  var ib = Scanf.Scanning.from_file(fname);
  var ob = $$Buffer.create(42);
  add_digest_ib(ob, ib);
  return $$Buffer.contents(ob);
}

function test54(param) {
  return Caml_obj.caml_equal(get_lines(tscanf_data_file), tscanf_data_file_lines);
}

function test55(param) {
  var ob = $$Buffer.create(42);
  create_tscanf_data(ob, tscanf_data_file_lines);
  var s = $$Buffer.contents(ob);
  ob[/* position */1] = 0;
  var ib = Scanf.Scanning.from_string(s);
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
