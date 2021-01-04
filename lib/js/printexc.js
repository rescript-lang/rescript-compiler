'use strict';

var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");

var printers = {
  contents: /* [] */0
};

var locfmt = /* Format */{
  _0: {
    TAG: /* String_literal */11,
    _0: "File \"",
    _1: {
      TAG: /* String */2,
      _0: /* No_padding */0,
      _1: {
        TAG: /* String_literal */11,
        _0: "\", line ",
        _1: {
          TAG: /* Int */4,
          _0: /* Int_d */0,
          _1: /* No_padding */0,
          _2: /* No_precision */0,
          _3: {
            TAG: /* String_literal */11,
            _0: ", characters ",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* Char_literal */12,
                _0: /* '-' */45,
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_d */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* String_literal */11,
                    _0: ": ",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
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
  _1: "File \"%s\", line %d, characters %d-%d: %s"
};

var fields = (function(x){
  var s = "" 
  var index = 1
  while ("_"+index in x){
    s += x ["_" + index];
    ++ index
  }
  if(index === 1){
    return s 
  }
  return "(" + s + ")"
});

function to_string(x) {
  var _param = printers.contents;
  while(true) {
    var param = _param;
    if (param) {
      var s;
      try {
        s = Curry._1(param.hd, x);
      }
      catch (exn){
        s = undefined;
      }
      if (s !== undefined) {
        return s;
      }
      _param = param.tl;
      continue ;
    }
    if (x.RE_EXN_ID === "Out_of_memory") {
      return "Out of memory";
    }
    if (x.RE_EXN_ID === "Stack_overflow") {
      return "Stack overflow";
    }
    if (x.RE_EXN_ID === "Match_failure") {
      var match = x._1;
      var $$char = match[2];
      return Curry._5(Printf.sprintf(locfmt), match[0], match[1], $$char, $$char + 5 | 0, "Pattern matching failed");
    }
    if (x.RE_EXN_ID === "Assert_failure") {
      var match$1 = x._1;
      var $$char$1 = match$1[2];
      return Curry._5(Printf.sprintf(locfmt), match$1[0], match$1[1], $$char$1, $$char$1 + 6 | 0, "Assertion failed");
    }
    if (x.RE_EXN_ID === "Undefined_recursive_module") {
      var match$2 = x._1;
      var $$char$2 = match$2[2];
      return Curry._5(Printf.sprintf(locfmt), match$2[0], match$2[1], $$char$2, $$char$2 + 6 | 0, "Undefined recursive module");
    }
    var constructor = Caml_exceptions.caml_exn_slot_name(x);
    return constructor + fields(x);
  };
}

function print(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    Curry._1(Printf.eprintf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "Uncaught exception: ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "Uncaught exception: %s\n"
            }), to_string(x));
    Pervasives.flush(Pervasives.stderr);
    throw x;
  }
}

function $$catch(fct, arg) {
  try {
    return Curry._1(fct, arg);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    Pervasives.flush(Pervasives.stdout);
    Curry._1(Printf.eprintf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "Uncaught exception: ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "Uncaught exception: %s\n"
            }), to_string(x));
    return Pervasives.exit(2);
  }
}

function convert_raw_backtrace_slot(param) {
  return Pervasives.failwith("convert_raw_backtrace_slot not implemented");
}

function convert_raw_backtrace(bt) {
  try {
    return undefined;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return ;
    }
    throw exn;
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
  if (slot.TAG === /* Known_location */0) {
    return Curry._6(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " file \"",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* '"' */34,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: ", line ",
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* String_literal */11,
                                    _0: ", characters ",
                                    _1: {
                                      TAG: /* Int */4,
                                      _0: /* Int_d */0,
                                      _1: /* No_padding */0,
                                      _2: /* No_precision */0,
                                      _3: {
                                        TAG: /* Char_literal */12,
                                        _0: /* '-' */45,
                                        _1: {
                                          TAG: /* Int */4,
                                          _0: /* Int_d */0,
                                          _1: /* No_padding */0,
                                          _2: /* No_precision */0,
                                          _3: /* End_of_format */0
                                        }
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
                    _1: "%s file \"%s\"%s, line %d, characters %d-%d"
                  }), info(slot.is_raise), slot.filename, slot.is_inline ? " (inlined)" : "", slot.line_number, slot.start_char, slot.end_char);
  } else if (slot.is_raise) {
    return ;
  } else {
    return Curry._1(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " unknown location",
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "%s unknown location"
                  }), info(false));
  }
}

function print_raw_backtrace(outchan, raw_backtrace) {
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace === undefined) {
    return Printf.fprintf(outchan, /* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "(Program not linked with -g, cannot print stack backtrace)\n",
                  _1: /* End_of_format */0
                },
                _1: "(Program not linked with -g, cannot print stack backtrace)\n"
              });
  }
  for(var i = 0 ,i_finish = backtrace.length; i < i_finish; ++i){
    var str = format_backtrace_slot(i, Caml_array.get(backtrace, i));
    if (str !== undefined) {
      Curry._1(Printf.fprintf(outchan, /* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                },
                _1: "%s\n"
              }), str);
    }
    
  }
  
}

function print_backtrace(outchan) {
  return print_raw_backtrace(outchan, undefined);
}

function raw_backtrace_to_string(raw_backtrace) {
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace === undefined) {
    return "(Program not linked with -g, cannot print stack backtrace)\n";
  }
  var b = $$Buffer.create(1024);
  for(var i = 0 ,i_finish = backtrace.length; i < i_finish; ++i){
    var str = format_backtrace_slot(i, Caml_array.get(backtrace, i));
    if (str !== undefined) {
      Curry._1(Printf.bprintf(b, /* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                },
                _1: "%s\n"
              }), str);
    }
    
  }
  return $$Buffer.contents(b);
}

function backtrace_slot_is_raise(l) {
  return l.is_raise;
}

function backtrace_slot_is_inline(l) {
  if (l.TAG === /* Known_location */0) {
    return l.is_inline;
  } else {
    return false;
  }
}

function backtrace_slot_location(l) {
  if (l.TAG === /* Known_location */0) {
    return {
            filename: l.filename,
            line_number: l.line_number,
            start_char: l.start_char,
            end_char: l.end_char
          };
  }
  
}

function backtrace_slots(raw_backtrace) {
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace === undefined) {
    return ;
  }
  var usable_slot = function (param) {
    if (param.TAG === /* Known_location */0) {
      return true;
    } else {
      return false;
    }
  };
  var exists_usable = function (_i) {
    while(true) {
      var i = _i;
      if (i === -1) {
        return false;
      }
      if (usable_slot(Caml_array.get(backtrace, i))) {
        return true;
      }
      _i = i - 1 | 0;
      continue ;
    };
  };
  if (exists_usable(backtrace.length - 1 | 0)) {
    return backtrace;
  }
  
}

function get_backtrace(param) {
  return raw_backtrace_to_string(undefined);
}

function register_printer(fn) {
  printers.contents = {
    hd: fn,
    tl: printers.contents
  };
  
}

function set_uncaught_exception_handler(param) {
  
}

function record_backtrace(prim) {
  
}

function backtrace_status(prim) {
  
}

function get_raw_backtrace(prim) {
  
}

function get_callstack(prim) {
  
}

var Slot = {
  is_raise: backtrace_slot_is_raise,
  is_inline: backtrace_slot_is_inline,
  $$location: backtrace_slot_location,
  format: format_backtrace_slot
};

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
/* No side effect */
