'use strict';

var Block = require("./block.js");
var Curry = require("./curry.js");
var Printf = require("./printf.js");
var Caml_gc = require("./caml_gc.js");

function print_stat(c) {
  var st = Caml_gc.caml_gc_stat(/* () */0);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "minor_words: ",
                /* Float */Block.__(8, [
                    /* Float_f */0,
                    /* No_padding */0,
                    /* Lit_precision */[0],
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "minor_words: %.0f\n"
          ]), st[/* minor_words */0]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "promoted_words: ",
                /* Float */Block.__(8, [
                    /* Float_f */0,
                    /* No_padding */0,
                    /* Lit_precision */[0],
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "promoted_words: %.0f\n"
          ]), st[/* promoted_words */1]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "major_words: ",
                /* Float */Block.__(8, [
                    /* Float_f */0,
                    /* No_padding */0,
                    /* Lit_precision */[0],
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "major_words: %.0f\n"
          ]), st[/* major_words */2]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "minor_collections: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "minor_collections: %d\n"
          ]), st[/* minor_collections */3]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "major_collections: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "major_collections: %d\n"
          ]), st[/* major_collections */4]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "heap_words: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "heap_words: %d\n"
          ]), st[/* heap_words */5]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "heap_chunks: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "heap_chunks: %d\n"
          ]), st[/* heap_chunks */6]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "top_heap_words: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "top_heap_words: %d\n"
          ]), st[/* top_heap_words */14]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "live_words: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "live_words: %d\n"
          ]), st[/* live_words */7]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "live_blocks: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "live_blocks: %d\n"
          ]), st[/* live_blocks */8]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "free_words: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "free_words: %d\n"
          ]), st[/* free_words */9]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "free_blocks: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "free_blocks: %d\n"
          ]), st[/* free_blocks */10]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "largest_free: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "largest_free: %d\n"
          ]), st[/* largest_free */11]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */Block.__(11, [
                "fragments: ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* "\n" */10,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "fragments: %d\n"
          ]), st[/* fragments */12]);
  return Curry._1(Printf.fprintf(c, /* Format */[
                  /* String_literal */Block.__(11, [
                      "compactions: ",
                      /* Int */Block.__(4, [
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* Char_literal */Block.__(12, [
                              /* "\n" */10,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "compactions: %d\n"
                ]), st[/* compactions */13]);
}

function allocated_bytes() {
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
  var arec_000 = /* active */[true];
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

exports.print_stat = print_stat;
exports.allocated_bytes = allocated_bytes;
exports.finalise = finalise;
exports.finalise_release = finalise_release;
exports.create_alarm = create_alarm;
exports.delete_alarm = delete_alarm;
/* No side effect */
