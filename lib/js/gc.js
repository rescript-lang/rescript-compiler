'use strict';

var Curry = require("./curry.js");
var Printf = require("./printf.js");
var Caml_gc = require("./caml_gc.js");

var dummy_stat = /* record */[
  /* minor_words */0,
  /* promoted_words */0,
  /* major_words */0,
  /* minor_collections */0,
  /* major_collections */0,
  /* heap_words */0,
  /* heap_chunks */0,
  /* live_words */0,
  /* live_blocks */0,
  /* free_words */0,
  /* free_blocks */0,
  /* largest_free */0,
  /* fragments */0,
  /* compactions */0,
  /* top_heap_words */0,
  /* stack_size */0
];

function stat(param) {
  return dummy_stat;
}

function quick_stat(param) {
  return dummy_stat;
}

function get(param) {
  return /* record */[
          /* minor_heap_size */0,
          /* major_heap_increment */0,
          /* space_overhead */0,
          /* verbose */0,
          /* max_overhead */0,
          /* stack_limit */0,
          /* allocation_policy */0
        ];
}

function print_stat(c) {
  var st = stat(/* () */0);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "minor_words: ",
              Arg1: /* constructor */{
                tag: "Float",
                Arg0: "Float_f",
                Arg1: "No_padding",
                Arg2: /* constructor */{
                  tag: "Lit_precision",
                  Arg0: 0
                },
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "minor_words: %.0f\n"
          }), st[/* minor_words */0]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "promoted_words: ",
              Arg1: /* constructor */{
                tag: "Float",
                Arg0: "Float_f",
                Arg1: "No_padding",
                Arg2: /* constructor */{
                  tag: "Lit_precision",
                  Arg0: 0
                },
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "promoted_words: %.0f\n"
          }), st[/* promoted_words */1]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "major_words: ",
              Arg1: /* constructor */{
                tag: "Float",
                Arg0: "Float_f",
                Arg1: "No_padding",
                Arg2: /* constructor */{
                  tag: "Lit_precision",
                  Arg0: 0
                },
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "major_words: %.0f\n"
          }), st[/* major_words */2]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "minor_collections: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "minor_collections: %d\n"
          }), st[/* minor_collections */3]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "major_collections: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "major_collections: %d\n"
          }), st[/* major_collections */4]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "heap_words: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "heap_words: %d\n"
          }), st[/* heap_words */5]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "heap_chunks: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "heap_chunks: %d\n"
          }), st[/* heap_chunks */6]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "top_heap_words: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "top_heap_words: %d\n"
          }), st[/* top_heap_words */14]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "live_words: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "live_words: %d\n"
          }), st[/* live_words */7]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "live_blocks: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "live_blocks: %d\n"
          }), st[/* live_blocks */8]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "free_words: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "free_words: %d\n"
          }), st[/* free_words */9]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "free_blocks: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "free_blocks: %d\n"
          }), st[/* free_blocks */10]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "largest_free: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "largest_free: %d\n"
          }), st[/* largest_free */11]);
  Curry._1(Printf.fprintf(c, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "fragments: ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "fragments: %d\n"
          }), st[/* fragments */12]);
  return Curry._1(Printf.fprintf(c, /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "compactions: ",
                    Arg1: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* "\n" */10,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "compactions: %d\n"
                }), st[/* compactions */13]);
}

function allocated_bytes(param) {
  var match = Caml_gc.caml_gc_counters(/* () */0);
  return (match[0] + match[2] - match[1]) * 4;
}

function call_alarm(arec) {
  if (arec[/* active */0][0]) {
    Caml_gc.caml_final_register(call_alarm, arec);
    return Curry._1(arec[/* f */1], /* () */0);
  } else {
    return 0;
  }
}

function create_alarm(f) {
  var arec_000 = /* active : record */[/* contents */true];
  var arec = /* record */[
    arec_000,
    /* f */f
  ];
  Caml_gc.caml_final_register(call_alarm, arec);
  return arec_000;
}

function delete_alarm(a) {
  a[0] = false;
  return /* () */0;
}

var finalise = Caml_gc.caml_final_register;

var finalise_release = Caml_gc.caml_final_release;

exports.stat = stat;
exports.quick_stat = quick_stat;
exports.get = get;
exports.print_stat = print_stat;
exports.allocated_bytes = allocated_bytes;
exports.finalise = finalise;
exports.finalise_release = finalise_release;
exports.create_alarm = create_alarm;
exports.delete_alarm = delete_alarm;
/* No side effect */
