// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Sys = require("./sys");
var Printf = require("./printf");

function print_stat(c) {
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
        ])(0);
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
              ])(0);
}

function allocated_bytes() {
  return (0 + 0 - 0) * (Sys.word_size / 8 | 0);
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
