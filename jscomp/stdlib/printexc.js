// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Obj = require("./obj");
var Pervasives = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Printf = require("./printf");
var Caml_primitive = require("../runtime/caml_primitive");
var $$Array = require("./array");
var Buffer = require("./buffer");

var printers = [
  0,
  /* [] */0
];

var locfmt = [
  /* Format */0,
  [
    /* String_literal */11,
    'File "',
    [
      /* String */2,
      /* No_padding */0,
      [
        /* String_literal */11,
        '", line ',
        [
          /* Int */4,
          /* Int_d */0,
          /* No_padding */0,
          /* No_precision */0,
          [
            /* String_literal */11,
            ", characters ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "-" */45,
                [
                  /* Int */4,
                  /* Int_d */0,
                  /* No_padding */0,
                  /* No_precision */0,
                  [
                    /* String_literal */11,
                    ": ",
                    [
                      /* String */2,
                      /* No_padding */0,
                      /* End_of_format */0
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ],
  'File "%s", line %d, characters %d-%d: %s'
];

function field(x, i) {
  var f = x[i];
  if (!Caml_obj_runtime.caml_obj_is_block(f)) {
    return Printf.sprintf([
                  /* Format */0,
                  [
                    /* Int */4,
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* End_of_format */0
                  ],
                  "%d"
                ])(f);
  }
  else {
    if (Caml_obj_runtime.caml_obj_tag(f) === Obj.string_tag) {
      return Printf.sprintf([
                    /* Format */0,
                    [
                      /* Caml_string */3,
                      /* No_padding */0,
                      /* End_of_format */0
                    ],
                    "%S"
                  ])(f);
    }
    else {
      if (Caml_obj_runtime.caml_obj_tag(f) === Obj.double_tag) {
        return Pervasives.string_of_float(f);
      }
      else {
        return "_";
      }
    }
  }
}

function other_fields(x, i) {
  if (i >= x.length) {
    return "";
  }
  else {
    return Printf.sprintf([
                  /* Format */0,
                  [
                    /* String_literal */11,
                    ", ",
                    [
                      /* String */2,
                      /* No_padding */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        /* End_of_format */0
                      ]
                    ]
                  ],
                  ", %s%s"
                ])(field(x, i), other_fields(x, i + 1));
  }
}

function fields(x) {
  var n = x.length;
  if (2 < (n >>> 0)) {
    return Printf.sprintf([
                  /* Format */0,
                  [
                    /* Char_literal */12,
                    /* "(" */40,
                    [
                      /* String */2,
                      /* No_padding */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* Char_literal */12,
                          /* ")" */41,
                          /* End_of_format */0
                        ]
                      ]
                    ]
                  ],
                  "(%s%s)"
                ])(field(x, 1), other_fields(x, 2));
  }
  else {
    switch (n) {
      case 0 : 
      case 1 : 
          return "";
      case 2 : 
          return Printf.sprintf([
                        /* Format */0,
                        [
                          /* Char_literal */12,
                          /* "(" */40,
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* Char_literal */12,
                              /* ")" */41,
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "(%s)"
                      ])(field(x, 1));
      
    }
  }
}

function to_string(x) {
  var _param = printers[1];
  while(/* true */1) {
    var param = _param;
    if (param) {
      var match;
      try {
        match = param[1](x);
      }
      catch (exn){
        match = /* None */0;
      }
      if (match) {
        return match[1];
      }
      else {
        _param = param[2];
      }
    }
    else {
      if (x === Caml_exceptions.Out_of_memory) {
        return "Out of memory";
      }
      else {
        if (x === Caml_exceptions.Stack_overflow) {
          return "Stack overflow";
        }
        else {
          if (x[1] === Caml_exceptions.Match_failure) {
            var match$1 = x[2];
            var $$char = match$1[3];
            return Printf.sprintf(locfmt)(match$1[1], match$1[2], $$char, $$char + 5, "Pattern matching failed");
          }
          else {
            if (x[1] === Caml_exceptions.Assert_failure) {
              var match$2 = x[2];
              var $$char$1 = match$2[3];
              return Printf.sprintf(locfmt)(match$2[1], match$2[2], $$char$1, $$char$1 + 6, "Assertion failed");
            }
            else {
              if (x[1] === Caml_exceptions.Undefined_recursive_module) {
                var match$3 = x[2];
                var $$char$2 = match$3[3];
                return Printf.sprintf(locfmt)(match$3[1], match$3[2], $$char$2, $$char$2 + 6, "Undefined recursive module");
              }
              else {
                if (Caml_obj_runtime.caml_obj_tag(x) !== 0) {
                  return x[0];
                }
                else {
                  var constructor = x[0][0];
                  return constructor + fields(x);
                }
              }
            }
          }
        }
      }
    }
  };
}

function print(fct, arg) {
  try {
    return fct(arg);
  }
  catch (x){
    Printf.eprintf([
            /* Format */0,
            [
              /* String_literal */11,
              "Uncaught exception: ",
              [
                /* String */2,
                /* No_padding */0,
                [
                  /* Char_literal */12,
                  /* "\n" */10,
                  /* End_of_format */0
                ]
              ]
            ],
            "Uncaught exception: %s\n"
          ])(to_string(x));
    Pervasives.flush(Pervasives.stderr);
    throw x;
  }
}

function $$catch(fct, arg) {
  try {
    return fct(arg);
  }
  catch (x){
    Pervasives.flush(Pervasives.stdout);
    Printf.eprintf([
            /* Format */0,
            [
              /* String_literal */11,
              "Uncaught exception: ",
              [
                /* String */2,
                /* No_padding */0,
                [
                  /* Char_literal */12,
                  /* "\n" */10,
                  /* End_of_format */0
                ]
              ]
            ],
            "Uncaught exception: %s\n"
          ])(to_string(x));
    return Pervasives.exit(2);
  }
}

function convert_raw_backtrace(rbckt) {
  try {
    return [
            /* Some */0,
            $$Array.map(function (prim) {
                  return Caml_primitive.caml_convert_raw_backtrace_slot(prim);
                }, rbckt)
          ];
  }
  catch (exn){
    if (exn[1] === Caml_exceptions.Failure) {
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
    else {
      if (pos) {
        return "Called from";
      }
      else {
        return "Raised by primitive operation at";
      }
    }
  };
  if (slot[0]) {
    if (slot[1] !== 0) {
      return /* None */0;
    }
    else {
      return [
              /* Some */0,
              Printf.sprintf([
                      /* Format */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          " unknown location",
                          /* End_of_format */0
                        ]
                      ],
                      "%s unknown location"
                    ])(info(/* false */0))
            ];
    }
  }
  else {
    return [
            /* Some */0,
            Printf.sprintf([
                    /* Format */0,
                    [
                      /* String */2,
                      /* No_padding */0,
                      [
                        /* String_literal */11,
                        ' file "',
                        [
                          /* String */2,
                          /* No_padding */0,
                          [
                            /* String_literal */11,
                            '", line ',
                            [
                              /* Int */4,
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              [
                                /* String_literal */11,
                                ", characters ",
                                [
                                  /* Int */4,
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  [
                                    /* Char_literal */12,
                                    /* "-" */45,
                                    [
                                      /* Int */4,
                                      /* Int_d */0,
                                      /* No_padding */0,
                                      /* No_precision */0,
                                      /* End_of_format */0
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ],
                    '%s file "%s", line %d, characters %d-%d'
                  ])(info(slot[1]), slot[2], slot[3], slot[4], slot[5])
          ];
  }
}

function print_raw_backtrace(outchan, raw_backtrace) {
  var outchan$1 = outchan;
  var backtrace = convert_raw_backtrace(raw_backtrace);
  if (backtrace) {
    var a = backtrace[1];
    for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
      var match = format_backtrace_slot(i, a[i]);
      if (match) {
        Printf.fprintf(outchan$1, [
                /* Format */0,
                [
                  /* String */2,
                  /* No_padding */0,
                  [
                    /* Char_literal */12,
                    /* "\n" */10,
                    /* End_of_format */0
                  ]
                ],
                "%s\n"
              ])(match[1]);
      }
      
    }
    return /* () */0;
  }
  else {
    return Printf.fprintf(outchan$1, [
                /* Format */0,
                [
                  /* String_literal */11,
                  "(Program not linked with -g, cannot print stack backtrace)\n",
                  /* End_of_format */0
                ],
                "(Program not linked with -g, cannot print stack backtrace)\n"
              ]);
  }
}

function print_backtrace(outchan) {
  return print_raw_backtrace(outchan, /* () */0);
}

function backtrace_to_string(backtrace) {
  if (backtrace) {
    var a = backtrace[1];
    var b = Buffer.create(1024);
    for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
      var match = format_backtrace_slot(i, a[i]);
      if (match) {
        Printf.bprintf(b, [
                /* Format */0,
                [
                  /* String */2,
                  /* No_padding */0,
                  [
                    /* Char_literal */12,
                    /* "\n" */10,
                    /* End_of_format */0
                  ]
                ],
                "%s\n"
              ])(match[1]);
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
  return param[1];
}

function backtrace_slot_location(param) {
  if (param[0]) {
    return /* None */0;
  }
  else {
    return [
            /* Some */0,
            [
              /* record */0,
              param[2],
              param[3],
              param[4],
              param[5]
            ]
          ];
  }
}

function backtrace_slots(raw_backtrace) {
  var match = convert_raw_backtrace(raw_backtrace);
  if (match) {
    var backtrace = match[1];
    var usable_slot = function (param) {
      if (param[0]) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    };
    var exists_usable = function (i) {
      if (i !== -1) {
        return +(usable_slot(backtrace[i]) || exists_usable(i - 1));
      }
      else {
        return /* false */0;
      }
    };
    if (exists_usable(backtrace.length - 1)) {
      return [
              /* Some */0,
              backtrace
            ];
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
  printers[1] = [
    /* :: */0,
    fn,
    printers[1]
  ];
  return /* () */0;
}

function exn_slot(x) {
  if (Caml_obj_runtime.caml_obj_tag(x)) {
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

var uncaught_exception_handler = [
  0,
  /* None */0
];

function set_uncaught_exception_handler(fn) {
  uncaught_exception_handler[1] = [
    /* Some */0,
    fn
  ];
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
  0,
  backtrace_slot_is_raise,
  backtrace_slot_location,
  format_backtrace_slot
];

function convert_raw_backtrace_slot(prim) {
  return Caml_primitive.caml_convert_raw_backtrace_slot(prim);
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
exports.exn_slot_id = exn_slot_id;
exports.exn_slot_name = exn_slot_name;
/* No side effect */
