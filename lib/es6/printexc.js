

import * as Block from "./block.js";
import * as Curry from "./curry.js";
import * as $$Buffer from "./buffer.js";
import * as Printf from "./printf.js";
import * as Caml_array from "./caml_array.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";
import * as Caml_external_polyfill from "./caml_external_polyfill.js";

var printers = {
  contents: /* [] */0
};

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
        s = Curry._1(param[0], x);
      }
      catch (exn){
        s = undefined;
      }
      if (s !== undefined) {
        return s;
      }
      _param = param[1];
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
  if (slot.tag) {
    if (slot[/* is_raise */0]) {
      return ;
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
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace === undefined) {
    return Printf.fprintf(outchan, /* Format */[
                /* String_literal */Block.__(11, [
                    "(Program not linked with -g, cannot print stack backtrace)\n",
                    /* End_of_format */0
                  ]),
                "(Program not linked with -g, cannot print stack backtrace)\n"
              ]);
  }
  for(var i = 0 ,i_finish = backtrace.length; i < i_finish; ++i){
    var str = format_backtrace_slot(i, Caml_array.caml_array_get(backtrace, i));
    if (str !== undefined) {
      Curry._1(Printf.fprintf(outchan, /* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ]),
                "%s\n"
              ]), str);
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
    var str = format_backtrace_slot(i, Caml_array.caml_array_get(backtrace, i));
    if (str !== undefined) {
      Curry._1(Printf.bprintf(b, /* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ]),
                "%s\n"
              ]), str);
    }
    
  }
  return $$Buffer.contents(b);
}

function backtrace_slot_is_raise(l) {
  return l[/* is_raise */0];
}

function backtrace_slot_is_inline(l) {
  if (l.tag) {
    return false;
  } else {
    return l[/* is_inline */5];
  }
}

function backtrace_slot_location(l) {
  if (l.tag) {
    return ;
  } else {
    return {
            filename: l[/* filename */1],
            line_number: l[/* line_number */2],
            start_char: l[/* start_char */3],
            end_char: l[/* end_char */4]
          };
  }
}

function backtrace_slots(raw_backtrace) {
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace === undefined) {
    return ;
  }
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
      if (i === -1) {
        return false;
      }
      if (usable_slot(Caml_array.caml_array_get(backtrace, i))) {
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
  printers.contents = /* :: */[
    fn,
    printers.contents
  ];
  
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

export {
  to_string ,
  print ,
  $$catch ,
  print_backtrace ,
  get_backtrace ,
  record_backtrace ,
  backtrace_status ,
  register_printer ,
  get_raw_backtrace ,
  print_raw_backtrace ,
  raw_backtrace_to_string ,
  get_callstack ,
  set_uncaught_exception_handler ,
  backtrace_slots ,
  Slot ,
  raw_backtrace_length ,
  get_raw_backtrace_slot ,
  convert_raw_backtrace_slot ,
  get_raw_backtrace_next_slot ,
  
}
/* No side effect */
