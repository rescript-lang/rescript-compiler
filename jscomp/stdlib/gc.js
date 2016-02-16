// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Sys        = require("./sys");
var Printf     = require("./printf");
var Caml_curry = require("../runtime/caml_curry");

function print_stat(c) {
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
              0: "minor_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */{
                  0: 0,
                  length: 1,
                  tag: 0
                },
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
            1: "minor_words: %.0f\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
              0: "promoted_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */{
                  0: 0,
                  length: 1,
                  tag: 0
                },
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
            1: "promoted_words: %.0f\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
              0: "major_words: ",
              1: /* Float */{
                0: /* Float_f */0,
                1: /* No_padding */0,
                2: /* Lit_precision */{
                  0: 0,
                  length: 1,
                  tag: 0
                },
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
            1: "major_words: %.0f\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "minor_collections: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "major_collections: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "heap_words: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "heap_chunks: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "top_heap_words: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "live_words: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "live_blocks: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "free_words: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "free_blocks: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "largest_free: %d\n",
            length: 2,
            tag: 0
          }), 0);
  Caml_curry.app1(Printf.fprintf(c, /* Format */{
            0: /* String_literal */{
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
            1: "fragments: %d\n",
            length: 2,
            tag: 0
          }), 0);
  return Caml_curry.app1(Printf.fprintf(c, /* Format */{
                  0: /* String_literal */{
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
                  1: "compactions: %d\n",
                  length: 2,
                  tag: 0
                }), 0);
}

function allocated_bytes() {
  return (0 + 0 - 0) * (Sys.word_size / 8 | 0);
}

function create_alarm(f) {
  var arec_000 = [/* true */1];
  return arec_000;
}

function delete_alarm(a) {
  a[0] = /* false */0;
  return /* () */0;
}

function finalise(_, _$1) {
  return /* () */0;
}

function finalise_release() {
  return /* () */0;
}

exports.print_stat       = print_stat;
exports.allocated_bytes  = allocated_bytes;
exports.finalise         = finalise;
exports.finalise_release = finalise_release;
exports.create_alarm     = create_alarm;
exports.delete_alarm     = delete_alarm;
/* No side effect */
