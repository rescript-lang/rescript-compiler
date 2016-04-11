// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_io                 = require("../runtime/caml_io");
var Obj                     = require("./obj");
var Pervasives              = require("./pervasives");
var Printf                  = require("./printf");
var Caml_primitive          = require("../runtime/caml_primitive");
var $$Array                 = require("./array");
var Buffer                  = require("./buffer");
var Caml_curry              = require("../runtime/caml_curry");

var printers = [/* [] */0];

var locfmt = /* Format */{
  0: /* String_literal */{
    0: 'File "',
    1: /* String */{
      0: /* No_padding */0,
      1: /* String_literal */{
        0: '", line ',
        1: /* Int */{
          0: /* Int_d */0,
          1: /* No_padding */0,
          2: /* No_precision */0,
          3: /* String_literal */{
            0: ", characters ",
            1: /* Int */{
              0: /* Int_d */0,
              1: /* No_padding */0,
              2: /* No_precision */0,
              3: /* Char_literal */{
                0: /* "-" */45,
                1: /* Int */{
                  0: /* Int_d */0,
                  1: /* No_padding */0,
                  2: /* No_precision */0,
                  3: /* String_literal */{
                    0: ": ",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  length: 4,
                  tag: 4
                },
                length: 2,
                tag: 12
              },
              length: 4,
              tag: 4
            },
            length: 2,
            tag: 11
          },
          length: 4,
          tag: 4
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
  1: 'File "%s", line %d, characters %d-%d: %s',
  length: 2,
  tag: 0
};

function field(x, i) {
  var f = x[i];
  if (f.length === undefined) {
    return Caml_curry.app1(Printf.sprintf(/* Format */{
                    0: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    1: "%d",
                    length: 2,
                    tag: 0
                  }), f);
  }
  else if ((f.tag | 0) === Obj.string_tag) {
    return Caml_curry.app1(Printf.sprintf(/* Format */{
                    0: /* Caml_string */{
                      0: /* No_padding */0,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 3
                    },
                    1: "%S",
                    length: 2,
                    tag: 0
                  }), f);
  }
  else if ((f.tag | 0) === Obj.double_tag) {
    return Pervasives.string_of_float(f);
  }
  else {
    return "_";
  }
}

function other_fields(x, i) {
  if (i >= x.length) {
    return "";
  }
  else {
    return Caml_curry.app2(Printf.sprintf(/* Format */{
                    0: /* String_literal */{
                      0: ", ",
                      1: /* String */{
                        0: /* No_padding */0,
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 2
                      },
                      length: 2,
                      tag: 11
                    },
                    1: ", %s%s",
                    length: 2,
                    tag: 0
                  }), field(x, i), other_fields(x, i + 1 | 0));
  }
}

function fields(x) {
  var n = x.length;
  if (n > 2 || n < 0) {
    return Caml_curry.app2(Printf.sprintf(/* Format */{
                    0: /* Char_literal */{
                      0: /* "(" */40,
                      1: /* String */{
                        0: /* No_padding */0,
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* ")" */41,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 2
                      },
                      length: 2,
                      tag: 12
                    },
                    1: "(%s%s)",
                    length: 2,
                    tag: 0
                  }), field(x, 1), other_fields(x, 2));
  }
  else {
    switch (n) {
      case 0 : 
      case 1 : 
          return "";
      case 2 : 
          return Caml_curry.app1(Printf.sprintf(/* Format */{
                          0: /* Char_literal */{
                            0: /* "(" */40,
                            1: /* String */{
                              0: /* No_padding */0,
                              1: /* Char_literal */{
                                0: /* ")" */41,
                                1: /* End_of_format */0,
                                length: 2,
                                tag: 12
                              },
                              length: 2,
                              tag: 2
                            },
                            length: 2,
                            tag: 12
                          },
                          1: "(%s)",
                          length: 2,
                          tag: 0
                        }), field(x, 1));
      
    }
  }
}

function to_string(x) {
  var _param = printers[0];
  while(true) {
    var param = _param;
    if (param) {
      var match;
      try {
        match = Caml_curry.app1(param[0], x);
      }
      catch (exn){
        match = /* None */0;
      }
      if (match) {
        return match[0];
      }
      else {
        _param = param[1];
        continue ;
        
      }
    }
    else if (x === Caml_builtin_exceptions.out_of_memory) {
      return "Out of memory";
    }
    else if (x === Caml_builtin_exceptions.stack_overflow) {
      return "Stack overflow";
    }
    else if (x[0] === Caml_builtin_exceptions.match_failure) {
      var match$1 = x[1];
      var $$char = match$1[2];
      return Caml_curry.app5(Printf.sprintf(locfmt), match$1[0], match$1[1], $$char, $$char + 5 | 0, "Pattern matching failed");
    }
    else if (x[0] === Caml_builtin_exceptions.assert_failure) {
      var match$2 = x[1];
      var $$char$1 = match$2[2];
      return Caml_curry.app5(Printf.sprintf(locfmt), match$2[0], match$2[1], $$char$1, $$char$1 + 6 | 0, "Assertion failed");
    }
    else if (x[0] === Caml_builtin_exceptions.undefined_recursive_module) {
      var match$3 = x[1];
      var $$char$2 = match$3[2];
      return Caml_curry.app5(Printf.sprintf(locfmt), match$3[0], match$3[1], $$char$2, $$char$2 + 6 | 0, "Undefined recursive module");
    }
    else if ((x.tag | 0) !== 0) {
      return x[0];
    }
    else {
      var constructor = x[0][0];
      return constructor + fields(x);
    }
  };
}

function print(fct, arg) {
  try {
    return Caml_curry.app1(fct, arg);
  }
  catch (x){
    Caml_curry.app1(Printf.eprintf(/* Format */{
              0: /* String_literal */{
                0: "Uncaught exception: ",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* "\n" */10,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              1: "Uncaught exception: %s\n",
              length: 2,
              tag: 0
            }), to_string(x));
    Caml_io.caml_ml_flush(Pervasives.stderr);
    throw x;
  }
}

function $$catch(fct, arg) {
  try {
    return Caml_curry.app1(fct, arg);
  }
  catch (x){
    Caml_io.caml_ml_flush(Pervasives.stdout);
    Caml_curry.app1(Printf.eprintf(/* Format */{
              0: /* String_literal */{
                0: "Uncaught exception: ",
                1: /* String */{
                  0: /* No_padding */0,
                  1: /* Char_literal */{
                    0: /* "\n" */10,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 12
                  },
                  length: 2,
                  tag: 2
                },
                length: 2,
                tag: 11
              },
              1: "Uncaught exception: %s\n",
              length: 2,
              tag: 0
            }), to_string(x));
    return Pervasives.exit(2);
  }
}

function convert_raw_backtrace(rbckt) {
  try {
    return /* Some */[$$Array.map(Caml_primitive.caml_convert_raw_backtrace_slot, rbckt)];
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return /* None */0;
    }
    else {
      throw exn;
    }
  }
}

function format_backtrace_slot(pos, slot) {
  var info = function (is_raise) {
    if (is_raise) {
      if (pos) {
        return "Re-raised at";
      }
      else {
        return "Raised at";
      }
    }
    else if (pos) {
      return "Called from";
    }
    else {
      return "Raised by primitive operation at";
    }
  };
  if (slot.tag) {
    if (slot[0] !== 0) {
      return /* None */0;
    }
    else {
      return /* Some */[Caml_curry.app1(Printf.sprintf(/* Format */{
                        0: /* String */{
                          0: /* No_padding */0,
                          1: /* String_literal */{
                            0: " unknown location",
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 11
                          },
                          length: 2,
                          tag: 2
                        },
                        1: "%s unknown location",
                        length: 2,
                        tag: 0
                      }), info(/* false */0))];
    }
  }
  else {
    return /* Some */[Caml_curry.app5(Printf.sprintf(/* Format */{
                      0: /* String */{
                        0: /* No_padding */0,
                        1: /* String_literal */{
                          0: ' file "',
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* String_literal */{
                              0: '", line ',
                              1: /* Int */{
                                0: /* Int_d */0,
                                1: /* No_padding */0,
                                2: /* No_precision */0,
                                3: /* String_literal */{
                                  0: ", characters ",
                                  1: /* Int */{
                                    0: /* Int_d */0,
                                    1: /* No_padding */0,
                                    2: /* No_precision */0,
                                    3: /* Char_literal */{
                                      0: /* "-" */45,
                                      1: /* Int */{
                                        0: /* Int_d */0,
                                        1: /* No_padding */0,
                                        2: /* No_precision */0,
                                        3: /* End_of_format */0,
                                        length: 4,
                                        tag: 4
                                      },
                                      length: 2,
                                      tag: 12
                                    },
                                    length: 4,
                                    tag: 4
                                  },
                                  length: 2,
                                  tag: 11
                                },
                                length: 4,
                                tag: 4
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
                        length: 2,
                        tag: 2
                      },
                      1: '%s file "%s", line %d, characters %d-%d',
                      length: 2,
                      tag: 0
                    }), info(slot[0]), slot[1], slot[2], slot[3], slot[4])];
  }
}

function print_raw_backtrace(outchan, raw_backtrace) {
  var outchan$1 = outchan;
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace) {
    var a = backtrace[0];
    for(var i = 0 ,i_finish = a.length - 1 | 0; i<= i_finish; ++i){
      var match = format_backtrace_slot(i, a[i]);
      if (match) {
        Caml_curry.app1(Printf.fprintf(outchan$1, /* Format */{
                  0: /* String */{
                    0: /* No_padding */0,
                    1: /* Char_literal */{
                      0: /* "\n" */10,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 2
                  },
                  1: "%s\n",
                  length: 2,
                  tag: 0
                }), match[0]);
      }
      
    }
    return /* () */0;
  }
  else {
    return Printf.fprintf(outchan$1, /* Format */{
                0: /* String_literal */{
                  0: "(Program not linked with -g, cannot print stack backtrace)\n",
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 11
                },
                1: "(Program not linked with -g, cannot print stack backtrace)\n",
                length: 2,
                tag: 0
              });
  }
}

function print_backtrace(outchan) {
  return print_raw_backtrace(outchan, /* () */0);
}

function backtrace_to_string(backtrace) {
  if (backtrace) {
    var a = backtrace[0];
    var b = Buffer.create(1024);
    for(var i = 0 ,i_finish = a.length - 1 | 0; i<= i_finish; ++i){
      var match = format_backtrace_slot(i, a[i]);
      if (match) {
        Caml_curry.app1(Printf.bprintf(b, /* Format */{
                  0: /* String */{
                    0: /* No_padding */0,
                    1: /* Char_literal */{
                      0: /* "\n" */10,
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 12
                    },
                    length: 2,
                    tag: 2
                  },
                  1: "%s\n",
                  length: 2,
                  tag: 0
                }), match[0]);
      }
      
    }
    return Buffer.contents(b);
  }
  else {
    return "(Program not linked with -g, cannot print stack backtrace)\n";
  }
}

function raw_backtrace_to_string(raw_backtrace) {
  return backtrace_to_string(convert_raw_backtrace(raw_backtrace));
}

function backtrace_slot_is_raise(param) {
  return param[0];
}

function backtrace_slot_location(param) {
  if (param.tag) {
    return /* None */0;
  }
  else {
    return /* Some */[/* record */[
              param[1],
              param[2],
              param[3],
              param[4]
            ]];
  }
}

function backtrace_slots(raw_backtrace) {
  var match = convert_raw_backtrace(raw_backtrace);
  if (match) {
    var backtrace = match[0];
    var usable_slot = function (param) {
      if (param.tag) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    };
    var exists_usable = function (_i) {
      while(true) {
        var i = _i;
        if (i !== -1) {
          if (usable_slot(backtrace[i])) {
            return /* true */1;
          }
          else {
            _i = i - 1 | 0;
            continue ;
            
          }
        }
        else {
          return /* false */0;
        }
      };
    };
    if (exists_usable(backtrace.length - 1 | 0)) {
      return /* Some */[backtrace];
    }
    else {
      return /* None */0;
    }
  }
  else {
    return /* None */0;
  }
}

function raw_backtrace_length(bckt) {
  return bckt.length;
}

function get_raw_backtrace_slot(bckt, i) {
  return bckt[i];
}

function get_backtrace() {
  return backtrace_to_string(convert_raw_backtrace(/* () */0));
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
  }
  else {
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

var uncaught_exception_handler = [/* None */0];

function set_uncaught_exception_handler(fn) {
  uncaught_exception_handler[0] = /* Some */[fn];
  return /* () */0;
}

function record_backtrace() {
  return /* () */0;
}

function backtrace_status() {
  return /* () */0;
}

function get_raw_backtrace() {
  return /* () */0;
}

function get_callstack() {
  return /* () */0;
}

var Slot = [
  backtrace_slot_is_raise,
  backtrace_slot_location,
  format_backtrace_slot
];

var convert_raw_backtrace_slot = Caml_primitive.caml_convert_raw_backtrace_slot

exports.to_string                      = to_string;
exports.print                          = print;
exports.$$catch                        = $$catch;
exports.print_backtrace                = print_backtrace;
exports.get_backtrace                  = get_backtrace;
exports.record_backtrace               = record_backtrace;
exports.backtrace_status               = backtrace_status;
exports.register_printer               = register_printer;
exports.get_raw_backtrace              = get_raw_backtrace;
exports.print_raw_backtrace            = print_raw_backtrace;
exports.raw_backtrace_to_string        = raw_backtrace_to_string;
exports.get_callstack                  = get_callstack;
exports.set_uncaught_exception_handler = set_uncaught_exception_handler;
exports.backtrace_slots                = backtrace_slots;
exports.Slot                           = Slot;
exports.raw_backtrace_length           = raw_backtrace_length;
exports.get_raw_backtrace_slot         = get_raw_backtrace_slot;
exports.convert_raw_backtrace_slot     = convert_raw_backtrace_slot;
exports.exn_slot_id                    = exn_slot_id;
exports.exn_slot_name                  = exn_slot_name;
/* No side effect */
