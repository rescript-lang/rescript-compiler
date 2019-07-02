'use strict';

var Obj = require("./obj.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var Caml_io = require("./caml_io.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var printers = /* record */[/* contents : [] */0];

var locfmt = /* Format */[
  /* String_literal */Block.__(11, [
      "File \"",
      /* String */Block.__(2, [
          /* No_padding */0,
          /* String_literal */Block.__(11, [
              "\", line ",
              /* Int */Block.__(4, [
                  /* Int_d */0,
                  /* No_padding */0,
                  /* No_precision */0,
                  /* String_literal */Block.__(11, [
                      ", characters ",
                      /* Int */Block.__(4, [
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* Char_literal */Block.__(12, [
                              /* "-" */45,
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* String_literal */Block.__(11, [
                                      ": ",
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* End_of_format */0
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ])
        ])
    ]),
  "File \"%s\", line %d, characters %d-%d: %s"
];

function field(x, i) {
  var f = x[i];
  if (typeof f === "number") {
    return Curry._1(Printf.sprintf(/* Format */[
                    /* Int */Block.__(4, [
                        /* Int_d */0,
                        /* No_padding */0,
                        /* No_precision */0,
                        /* End_of_format */0
                      ]),
                    "%d"
                  ]), f);
  } else if ((f.tag | 0) === Obj.string_tag) {
    return Curry._1(Printf.sprintf(/* Format */[
                    /* Caml_string */Block.__(3, [
                        /* No_padding */0,
                        /* End_of_format */0
                      ]),
                    "%S"
                  ]), f);
  } else if ((f.tag | 0) === Obj.double_tag) {
    return Pervasives.string_of_float(f);
  } else {
    return "_";
  }
}

function other_fields(x, i) {
  if (i >= x.length) {
    return "";
  } else {
    return Curry._2(Printf.sprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        ", ",
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ])
                      ]),
                    ", %s%s"
                  ]), field(x, i), other_fields(x, i + 1 | 0));
  }
}

function fields(x) {
  var match = x.length;
  switch (match) {
    case 0 : 
    case 1 : 
        return "";
    case 2 : 
        return Curry._1(Printf.sprintf(/* Format */[
                        /* Char_literal */Block.__(12, [
                            /* "(" */40,
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* ")" */41,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "(%s)"
                      ]), field(x, 1));
    default:
      return Curry._2(Printf.sprintf(/* Format */[
                      /* Char_literal */Block.__(12, [
                          /* "(" */40,
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* ")" */41,
                                      /* End_of_format */0
                                    ])
                                ])
                            ])
                        ]),
                      "(%s%s)"
                    ]), field(x, 1), other_fields(x, 2));
  }
}

function to_string(x) {
  var _param = printers[0];
  while(true) {
    var param = _param;
    if (param) {
      var match;
      try {
        match = Curry._1(param[0], x);
      }
      catch (exn){
        match = undefined;
      }
      if (match !== undefined) {
        return match;
      } else {
        _param = param[1];
        continue ;
      }
    } else if (x === Caml_builtin_exceptions.out_of_memory) {
      return "Out of memory";
    } else if (x === Caml_builtin_exceptions.stack_overflow) {
      return "Stack overflow";
    } else if (x[0] === Caml_builtin_exceptions.match_failure) {
      var match$1 = x[1];
      var $$char = match$1[2];
      return Curry._5(Printf.sprintf(locfmt), match$1[0], match$1[1], $$char, $$char + 5 | 0, "Pattern matching failed");
    } else if (x[0] === Caml_builtin_exceptions.assert_failure) {
      var match$2 = x[1];
      var $$char$1 = match$2[2];
      return Curry._5(Printf.sprintf(locfmt), match$2[0], match$2[1], $$char$1, $$char$1 + 6 | 0, "Assertion failed");
    } else if (x[0] === Caml_builtin_exceptions.undefined_recursive_module) {
      var match$3 = x[1];
      var $$char$2 = match$3[2];
      return Curry._5(Printf.sprintf(locfmt), match$3[0], match$3[1], $$char$2, $$char$2 + 6 | 0, "Undefined recursive module");
    } else if ((x.tag | 0) !== 0) {
      return x[0];
    } else {
      var constructor = x[0][0];
      return constructor + fields(x);
    }
  };
}

function print(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    Curry._1(Printf.eprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Uncaught exception: ",
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* Char_literal */Block.__(12, [
                          /* "\n" */10,
                          /* End_of_format */0
                        ])
                    ])
                ]),
              "Uncaught exception: %s\n"
            ]), to_string(x));
    Caml_io.caml_ml_flush(Pervasives.stderr);
    throw x;
  }
}

function $$catch(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    Caml_io.caml_ml_flush(Pervasives.stdout);
    Curry._1(Printf.eprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Uncaught exception: ",
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* Char_literal */Block.__(12, [
                          /* "\n" */10,
                          /* End_of_format */0
                        ])
                    ])
                ]),
              "Uncaught exception: %s\n"
            ]), to_string(x));
    return Pervasives.exit(2);
  }
}

function convert_raw_backtrace_slot(param) {
  throw [
        Caml_builtin_exceptions.failure,
        "convert_raw_backtrace_slot not implemented"
      ];
}

function convert_raw_backtrace(bt) {
  try {
    return /* () */0;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function format_backtrace_slot(pos, slot) {
  var info = function (is_raise) {
    if (is_raise) {
      if (pos === 0) {
        return "Raised at";
      } else {
        return "Re-raised at";
      }
    } else if (pos === 0) {
      return "Raised by primitive operation at";
    } else {
      return "Called from";
    }
  };
  if (slot.tag) {
    if (slot[/* is_raise */0]) {
      return undefined;
    } else {
      return Curry._1(Printf.sprintf(/* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* String_literal */Block.__(11, [
                              " unknown location",
                              /* End_of_format */0
                            ])
                        ]),
                      "%s unknown location"
                    ]), info(false));
    }
  } else {
    return Curry._6(Printf.sprintf(/* Format */[
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            " file \"",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* "\"" */34,
                                    /* String */Block.__(2, [
                                        /* No_padding */0,
                                        /* String_literal */Block.__(11, [
                                            ", line ",
                                            /* Int */Block.__(4, [
                                                /* Int_d */0,
                                                /* No_padding */0,
                                                /* No_precision */0,
                                                /* String_literal */Block.__(11, [
                                                    ", characters ",
                                                    /* Int */Block.__(4, [
                                                        /* Int_d */0,
                                                        /* No_padding */0,
                                                        /* No_precision */0,
                                                        /* Char_literal */Block.__(12, [
                                                            /* "-" */45,
                                                            /* Int */Block.__(4, [
                                                                /* Int_d */0,
                                                                /* No_padding */0,
                                                                /* No_precision */0,
                                                                /* End_of_format */0
                                                              ])
                                                          ])
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ])
                              ])
                          ])
                      ]),
                    "%s file \"%s\"%s, line %d, characters %d-%d"
                  ]), info(slot[/* is_raise */0]), slot[/* filename */1], slot[/* is_inline */5] ? " (inlined)" : "", slot[/* line_number */2], slot[/* start_char */3], slot[/* end_char */4]);
  }
}

function print_raw_backtrace(outchan, raw_backtrace) {
  var outchan$1 = outchan;
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace !== undefined) {
    var a = backtrace;
    for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
      var match = format_backtrace_slot(i, Caml_array.caml_array_get(a, i));
      if (match !== undefined) {
        Curry._1(Printf.fprintf(outchan$1, /* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* Char_literal */Block.__(12, [
                          /* "\n" */10,
                          /* End_of_format */0
                        ])
                    ]),
                  "%s\n"
                ]), match);
      }
      
    }
    return /* () */0;
  } else {
    return Printf.fprintf(outchan$1, /* Format */[
                /* String_literal */Block.__(11, [
                    "(Program not linked with -g, cannot print stack backtrace)\n",
                    /* End_of_format */0
                  ]),
                "(Program not linked with -g, cannot print stack backtrace)\n"
              ]);
  }
}

function print_backtrace(outchan) {
  return print_raw_backtrace(outchan, /* () */0);
}

function raw_backtrace_to_string(raw_backtrace) {
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace !== undefined) {
    var a = backtrace;
    var b = $$Buffer.create(1024);
    for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
      var match = format_backtrace_slot(i, Caml_array.caml_array_get(a, i));
      if (match !== undefined) {
        Curry._1(Printf.bprintf(b, /* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* Char_literal */Block.__(12, [
                          /* "\n" */10,
                          /* End_of_format */0
                        ])
                    ]),
                  "%s\n"
                ]), match);
      }
      
    }
    return $$Buffer.contents(b);
  } else {
    return "(Program not linked with -g, cannot print stack backtrace)\n";
  }
}

function backtrace_slot_is_raise(param) {
  return param[/* is_raise */0];
}

function backtrace_slot_is_inline(param) {
  if (param.tag) {
    return false;
  } else {
    return param[/* is_inline */5];
  }
}

function backtrace_slot_location(param) {
  if (param.tag) {
    return undefined;
  } else {
    return /* record */[
            /* filename */param[/* filename */1],
            /* line_number */param[/* line_number */2],
            /* start_char */param[/* start_char */3],
            /* end_char */param[/* end_char */4]
          ];
  }
}

function backtrace_slots(raw_backtrace) {
  var match = convert_raw_backtrace(raw_backtrace);
  if (match !== undefined) {
    var backtrace = match;
    var usable_slot = function (param) {
      if (param.tag) {
        return false;
      } else {
        return true;
      }
    };
    var exists_usable = function (_i) {
      while(true) {
        var i = _i;
        if (i !== -1) {
          if (usable_slot(Caml_array.caml_array_get(backtrace, i))) {
            return true;
          } else {
            _i = i - 1 | 0;
            continue ;
          }
        } else {
          return false;
        }
      };
    };
    if (exists_usable(backtrace.length - 1 | 0)) {
      return backtrace;
    } else {
      return undefined;
    }
  }
  
}

function get_backtrace(param) {
  return raw_backtrace_to_string(/* () */0);
}

function register_printer(fn) {
  printers[0] = /* :: */[
    fn,
    printers[0]
  ];
  return /* () */0;
}

function exn_slot(x) {
  if (x.tag) {
    return x;
  } else {
    return x[0];
  }
}

function exn_slot_id(x) {
  var slot = exn_slot(x);
  return slot[1];
}

function exn_slot_name(x) {
  var slot = exn_slot(x);
  return slot[0];
}

var uncaught_exception_handler = /* record */[/* contents */undefined];

function set_uncaught_exception_handler(fn) {
  uncaught_exception_handler[0] = fn;
  return /* () */0;
}

function record_backtrace(prim) {
  return /* () */0;
}

function backtrace_status(prim) {
  return /* () */0;
}

function get_raw_backtrace(prim) {
  return /* () */0;
}

function get_callstack(prim) {
  return /* () */0;
}

var Slot = [
  backtrace_slot_is_raise,
  backtrace_slot_is_inline,
  backtrace_slot_location,
  format_backtrace_slot
];

function raw_backtrace_length(prim) {
  return Caml_external_polyfill.resolve("caml_raw_backtrace_length")(prim);
}

function get_raw_backtrace_slot(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_raw_backtrace_slot")(prim, prim$1);
}

function get_raw_backtrace_next_slot(prim) {
  return Caml_external_polyfill.resolve("caml_raw_backtrace_next_slot")(prim);
}

exports.to_string = to_string;
exports.print = print;
exports.$$catch = $$catch;
exports.print_backtrace = print_backtrace;
exports.get_backtrace = get_backtrace;
exports.record_backtrace = record_backtrace;
exports.backtrace_status = backtrace_status;
exports.register_printer = register_printer;
exports.get_raw_backtrace = get_raw_backtrace;
exports.print_raw_backtrace = print_raw_backtrace;
exports.raw_backtrace_to_string = raw_backtrace_to_string;
exports.get_callstack = get_callstack;
exports.set_uncaught_exception_handler = set_uncaught_exception_handler;
exports.backtrace_slots = backtrace_slots;
exports.Slot = Slot;
exports.raw_backtrace_length = raw_backtrace_length;
exports.get_raw_backtrace_slot = get_raw_backtrace_slot;
exports.convert_raw_backtrace_slot = convert_raw_backtrace_slot;
exports.get_raw_backtrace_next_slot = get_raw_backtrace_next_slot;
exports.exn_slot_id = exn_slot_id;
exports.exn_slot_name = exn_slot_name;
/* No side effect */
