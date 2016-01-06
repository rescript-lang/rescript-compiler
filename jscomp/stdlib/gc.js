// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Sys = require("./sys");
var Printf = require("./printf");

function print_stat(c) {
  var st = /* caml_gc_stat */[
    /* stat-record */0,
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
    /* larget_blocks */0,
    /* fragments */0,
    /* compactions */0,
    /* top_heap_words */0,
    /* stack_size */0
  ];
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "minor_words: ",
            [
              /* Float */8,
              /* Float_f */0,
              /* No_padding */0,
              [
                /* Lit_precision */0,
                0
              ],
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "minor_words: %.0f\n"
        ])(st[1]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "promoted_words: ",
            [
              /* Float */8,
              /* Float_f */0,
              /* No_padding */0,
              [
                /* Lit_precision */0,
                0
              ],
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "promoted_words: %.0f\n"
        ])(st[2]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "major_words: ",
            [
              /* Float */8,
              /* Float_f */0,
              /* No_padding */0,
              [
                /* Lit_precision */0,
                0
              ],
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "major_words: %.0f\n"
        ])(st[3]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "minor_collections: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "minor_collections: %d\n"
        ])(st[4]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "major_collections: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "major_collections: %d\n"
        ])(st[5]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "heap_words: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "heap_words: %d\n"
        ])(st[6]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "heap_chunks: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "heap_chunks: %d\n"
        ])(st[7]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "top_heap_words: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "top_heap_words: %d\n"
        ])(st[15]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "live_words: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "live_words: %d\n"
        ])(st[8]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "live_blocks: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "live_blocks: %d\n"
        ])(st[9]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "free_words: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "free_words: %d\n"
        ])(st[10]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "free_blocks: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "free_blocks: %d\n"
        ])(st[11]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "largest_free: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "largest_free: %d\n"
        ])(st[12]);
  Printf.fprintf(c, [
          /* Format */0,
          [
            /* String_literal */11,
            "fragments: ",
            [
              /* Int */4,
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* Char_literal */12,
                /* "\n" */10,
                /* End_of_format */0
              ]
            ]
          ],
          "fragments: %d\n"
        ])(st[13]);
  return Printf.fprintf(c, [
                /* Format */0,
                [
                  /* String_literal */11,
                  "compactions: ",
                  [
                    /* Int */4,
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    [
                      /* Char_literal */12,
                      /* "\n" */10,
                      /* End_of_format */0
                    ]
                  ]
                ],
                "compactions: %d\n"
              ])(st[14]);
}

function allocated_bytes() {
  var match = [
    0,
    0,
    0,
    0
  ];
  return (match[1] + match[3] - match[2]) * (Sys.word_size / 8 | 0);
}

function create_alarm(f) {
  var arec_001 = [
    0,
    /* true */1
  ];
  return arec_001;
}

function delete_alarm(a) {
  a[1] = /* false */0;
  return /* () */0;
}

function finalise(_, _$1) {
  return /* () */0;
}

function finalise_release() {
  return /* () */0;
}

exports.print_stat = print_stat;
exports.allocated_bytes = allocated_bytes;
exports.finalise = finalise;
exports.finalise_release = finalise_release;
exports.create_alarm = create_alarm;
exports.delete_alarm = delete_alarm;
/* No side effect */
