// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_gc = require("../runtime/caml_gc");
var Sys     = require("./sys");
var Curry   = require("../runtime/curry");
var Printf  = require("./printf");

function print_stat(c) {
  var st = Caml_gc.caml_gc_stat(/* () */0);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "minor_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */[0],
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 8
              },
              length: 2,
              tag: 11
            },
            "minor_words: %.0f\n"
          ]), st[/* minor_words */0]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "promoted_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */[0],
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 8
              },
              length: 2,
              tag: 11
            },
            "promoted_words: %.0f\n"
          ]), st[/* promoted_words */1]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "major_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */[0],
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 8
              },
              length: 2,
              tag: 11
            },
            "major_words: %.0f\n"
          ]), st[/* major_words */2]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "minor_collections: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "minor_collections: %d\n"
          ]), st[/* minor_collections */3]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "major_collections: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "major_collections: %d\n"
          ]), st[/* major_collections */4]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "heap_words: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "heap_words: %d\n"
          ]), st[/* heap_words */5]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "heap_chunks: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "heap_chunks: %d\n"
          ]), st[/* heap_chunks */6]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "top_heap_words: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "top_heap_words: %d\n"
          ]), st[/* top_heap_words */14]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "live_words: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "live_words: %d\n"
          ]), st[/* live_words */7]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "live_blocks: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "live_blocks: %d\n"
          ]), st[/* live_blocks */8]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "free_words: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "free_words: %d\n"
          ]), st[/* free_words */9]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "free_blocks: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "free_blocks: %d\n"
          ]), st[/* free_blocks */10]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "largest_free: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "largest_free: %d\n"
          ]), st[/* largest_free */11]);
  Curry._1(Printf.fprintf(c, /* Format */[
            /* String_literal */{
              0: "fragments: ",
              1: /* Int */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              length: 2,
              tag: 11
            },
            "fragments: %d\n"
          ]), st[/* fragments */12]);
  return Curry._1(Printf.fprintf(c, /* Format */[
                  /* String_literal */{
                    0: "compactions: ",
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "\n" */10,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 11
                  },
                  "compactions: %d\n"
                ]), st[/* compactions */13]);
}

function allocated_bytes() {
  var match = Caml_gc.caml_gc_counters(/* () */0);
  return (match[0] + match[2] - match[1]) * (Sys.word_size / 8 | 0);
}

function call_alarm(arec) {
  if (arec[/* active */0][0]) {
    Caml_gc.caml_final_register(call_alarm, arec);
    return Curry._1(arec[/* f */1], /* () */0);
  }
  else {
    return 0;
  }
}

function create_alarm(f) {
  var arec_000 = [/* true */1];
  var arec = /* record */[
    arec_000,
    f
  ];
  Caml_gc.caml_final_register(call_alarm, arec);
  return arec_000;
}

function delete_alarm(a) {
  a[0] = /* false */0;
  return /* () */0;
}

var finalise = Caml_gc.caml_final_register

var finalise_release = Caml_gc.caml_final_release

exports.print_stat       = print_stat;
exports.allocated_bytes  = allocated_bytes;
exports.finalise         = finalise;
exports.finalise_release = finalise_release;
exports.create_alarm     = create_alarm;
exports.delete_alarm     = delete_alarm;
/* No side effect */
