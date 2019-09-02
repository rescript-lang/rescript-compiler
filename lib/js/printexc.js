'use strict';

var Obj = require("./obj.js");
var $$Array = require("./array.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var Caml_io = require("./caml_io.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var printers = /* record */[/* contents */"[]"];

var locfmt = /* constructor */{
  tag: "Format",
  Arg0: /* constructor */{
    tag: "String_literal",
    Arg0: "File \"",
    Arg1: /* constructor */{
      tag: "String",
      Arg0: "No_padding",
      Arg1: /* constructor */{
        tag: "String_literal",
        Arg0: "\", line ",
        Arg1: /* constructor */{
          tag: "Int",
          Arg0: "Int_d",
          Arg1: "No_padding",
          Arg2: "No_precision",
          Arg3: /* constructor */{
            tag: "String_literal",
            Arg0: ", characters ",
            Arg1: /* constructor */{
              tag: "Int",
              Arg0: "Int_d",
              Arg1: "No_padding",
              Arg2: "No_precision",
              Arg3: /* constructor */{
                tag: "Char_literal",
                Arg0: /* "-" */45,
                Arg1: /* constructor */{
                  tag: "Int",
                  Arg0: "Int_d",
                  Arg1: "No_padding",
                  Arg2: "No_precision",
                  Arg3: /* constructor */{
                    tag: "String_literal",
                    Arg0: ": ",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: "End_of_format"
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  },
  Arg1: "File \"%s\", line %d, characters %d-%d: %s"
};

function field(x, i) {
  var f = x[i];
  if (typeof f === "number") {
    return Curry._1(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: "End_of_format"
                    },
                    Arg1: "%d"
                  }), f);
  } else if (/* XXX */f.tag === Obj.string_tag) {
    return Curry._1(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "Caml_string",
                      Arg0: "No_padding",
                      Arg1: "End_of_format"
                    },
                    Arg1: "%S"
                  }), f);
  } else if (/* XXX */f.tag === Obj.double_tag) {
    return Pervasives.string_of_float(f);
  } else {
    return "_";
  }
}

function other_fields(x, i) {
  if (i >= x.length) {
    return "";
  } else {
    return Curry._2(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: ", ",
                      Arg1: /* constructor */{
                        tag: "String",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: "End_of_format"
                        }
                      }
                    },
                    Arg1: ", %s%s"
                  }), field(x, i), other_fields(x, i + 1 | 0));
  }
}

function fields(x) {
  var n = x.length;
  switch (n) {
    case 0 :
    case 1 :
        return "";
    case 2 :
        return Curry._1(Printf.sprintf(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* "(" */40,
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* ")" */41,
                              Arg1: "End_of_format"
                            }
                          }
                        },
                        Arg1: "(%s)"
                      }), field(x, 1));
    default:
      return Curry._2(Printf.sprintf(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* "(" */40,
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* ")" */41,
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      },
                      Arg1: "(%s%s)"
                    }), field(x, 1), other_fields(x, 2));
  }
}

function to_string(x) {
  var _param = printers[0];
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var match;
      try {
        match = Curry._1(param.Arg0, x);
      }
      catch (exn){
        match = undefined;
      }
      if (match !== undefined) {
        return match;
      } else {
        _param = param.Arg1;
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
    } else if (/* XXX */x.tag !== undefined) {
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
    Curry._1(Printf.eprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "Uncaught exception: ",
                Arg1: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: /* constructor */{
                    tag: "Char_literal",
                    Arg0: /* "\n" */10,
                    Arg1: "End_of_format"
                  }
                }
              },
              Arg1: "Uncaught exception: %s\n"
            }), to_string(x));
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
    Curry._1(Printf.eprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "Uncaught exception: ",
                Arg1: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: /* constructor */{
                    tag: "Char_literal",
                    Arg0: /* "\n" */10,
                    Arg1: "End_of_format"
                  }
                }
              },
              Arg1: "Uncaught exception: %s\n"
            }), to_string(x));
    return Pervasives.exit(2);
  }
}

function convert_raw_backtrace_slot(param) {
  throw [
        Caml_builtin_exceptions.failure,
        "convert_raw_backtrace_slot not implemented"
      ];
}

function convert_raw_backtrace(rbckt) {
  try {
    return $$Array.map(convert_raw_backtrace_slot, rbckt);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return ;
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
  if (/* XXX */slot.tag === "Known_location") {
    return Curry._5(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: " file \"",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: "\", line ",
                            Arg1: /* constructor */{
                              tag: "Int",
                              Arg0: "Int_d",
                              Arg1: "No_padding",
                              Arg2: "No_precision",
                              Arg3: /* constructor */{
                                tag: "String_literal",
                                Arg0: ", characters ",
                                Arg1: /* constructor */{
                                  tag: "Int",
                                  Arg0: "Int_d",
                                  Arg1: "No_padding",
                                  Arg2: "No_precision",
                                  Arg3: /* constructor */{
                                    tag: "Char_literal",
                                    Arg0: /* "-" */45,
                                    Arg1: /* constructor */{
                                      tag: "Int",
                                      Arg0: "Int_d",
                                      Arg1: "No_padding",
                                      Arg2: "No_precision",
                                      Arg3: "End_of_format"
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "%s file \"%s\", line %d, characters %d-%d"
                  }), info(slot.Arg0), slot.Arg1, slot.Arg2, slot.Arg3, slot.Arg4);
  } else if (slot.Arg0) {
    return ;
  } else {
    return Curry._1(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: " unknown location",
                        Arg1: "End_of_format"
                      }
                    },
                    Arg1: "%s unknown location"
                  }), info(false));
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
        Curry._1(Printf.fprintf(outchan$1, /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "Char_literal",
                      Arg0: /* "\n" */10,
                      Arg1: "End_of_format"
                    }
                  },
                  Arg1: "%s\n"
                }), match);
      }
      
    }
    return /* () */0;
  } else {
    return Printf.fprintf(outchan$1, /* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "(Program not linked with -g, cannot print stack backtrace)\n",
                  Arg1: "End_of_format"
                },
                Arg1: "(Program not linked with -g, cannot print stack backtrace)\n"
              });
  }
}

function print_backtrace(outchan) {
  return print_raw_backtrace(outchan, /* () */0);
}

function backtrace_to_string(backtrace) {
  if (backtrace !== undefined) {
    var a = backtrace;
    var b = $$Buffer.create(1024);
    for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
      var match = format_backtrace_slot(i, Caml_array.caml_array_get(a, i));
      if (match !== undefined) {
        Curry._1(Printf.bprintf(b, /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "Char_literal",
                      Arg0: /* "\n" */10,
                      Arg1: "End_of_format"
                    }
                  },
                  Arg1: "%s\n"
                }), match);
      }
      
    }
    return $$Buffer.contents(b);
  } else {
    return "(Program not linked with -g, cannot print stack backtrace)\n";
  }
}

function raw_backtrace_to_string(raw_backtrace) {
  return backtrace_to_string(convert_raw_backtrace(raw_backtrace));
}

function backtrace_slot_is_raise(param) {
  return param.Arg0;
}

function backtrace_slot_location(param) {
  if (/* XXX */param.tag === "Known_location") {
    return /* record */[
            /* filename */param.Arg1,
            /* line_number */param.Arg2,
            /* start_char */param.Arg3,
            /* end_char */param.Arg4
          ];
  }
  
}

function backtrace_slots(raw_backtrace) {
  var match = convert_raw_backtrace(raw_backtrace);
  if (match !== undefined) {
    var backtrace = match;
    var usable_slot = function (param) {
      if (/* XXX */param.tag === "Known_location") {
        return true;
      } else {
        return false;
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
      return ;
    }
  }
  
}

function raw_backtrace_length(bckt) {
  return bckt.length;
}

var get_raw_backtrace_slot = Caml_array.caml_array_get;

function get_backtrace(param) {
  return backtrace_to_string(convert_raw_backtrace(/* () */0));
}

function register_printer(fn) {
  printers[0] = /* constructor */{
    tag: "::",
    Arg0: fn,
    Arg1: printers[0]
  };
  return /* () */0;
}

function exn_slot(x) {
  if (/* XXX */x.tag === 0) {
    return x[0];
  } else {
    return x;
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

var Slot = {
  is_raise: backtrace_slot_is_raise,
  location: backtrace_slot_location,
  format: format_backtrace_slot
};

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
exports.exn_slot_id = exn_slot_id;
exports.exn_slot_name = exn_slot_name;
/* No side effect */
