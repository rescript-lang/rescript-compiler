

import * as Curry from "./curry.js";
import * as Printf from "./printf.js";
import * as Caml_gc from "./caml_gc.js";

var dummy_stat = {
  minor_words: 0,
  promoted_words: 0,
  major_words: 0,
  minor_collections: 0,
  major_collections: 0,
  heap_words: 0,
  heap_chunks: 0,
  live_words: 0,
  live_blocks: 0,
  free_words: 0,
  free_blocks: 0,
  largest_free: 0,
  fragments: 0,
  compactions: 0,
  top_heap_words: 0,
  stack_size: 0
};

function stat(param) {
  return dummy_stat;
}

function quick_stat(param) {
  return dummy_stat;
}

function get(param) {
  return {
          minor_heap_size: 0,
          major_heap_increment: 0,
          space_overhead: 0,
          verbose: 0,
          max_overhead: 0,
          stack_limit: 0,
          allocation_policy: 0,
          window_size: 0
        };
}

function print_stat(c) {
  var st = stat(undefined);
  Curry._1(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "minor_collections: ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "minor_collections: %d\n"
          }), st.minor_collections);
  Curry._1(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "major_collections: ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "major_collections: %d\n"
          }), st.major_collections);
  Curry._1(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "compactions:       ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "compactions:       %d\n"
          }), st.compactions);
  Printf.fprintf(c, /* Format */{
        _0: {
          TAG: /* Char_literal */12,
          _0: /* "\n" */10,
          _1: /* End_of_format */0
        },
        _1: "\n"
      });
  var l1 = Curry._1(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* Float */8,
              _0: /* Float_f */0,
              _1: /* No_padding */0,
              _2: /* Lit_precision */{
                _0: 0
              },
              _3: /* End_of_format */0
            },
            _1: "%.0f"
          }), st.minor_words).length;
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "minor_words:    ",
              _1: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* Lit_precision */{
                  _0: 0
                },
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "minor_words:    %*.0f\n"
          }), l1, st.minor_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "promoted_words: ",
              _1: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* Lit_precision */{
                  _0: 0
                },
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "promoted_words: %*.0f\n"
          }), l1, st.promoted_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "major_words:    ",
              _1: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* Lit_precision */{
                  _0: 0
                },
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "major_words:    %*.0f\n"
          }), l1, st.major_words);
  Printf.fprintf(c, /* Format */{
        _0: {
          TAG: /* Char_literal */12,
          _0: /* "\n" */10,
          _1: /* End_of_format */0
        },
        _1: "\n"
      });
  var l2 = Curry._1(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: /* End_of_format */0
            },
            _1: "%d"
          }), st.top_heap_words).length;
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "top_heap_words: ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "top_heap_words: %*d\n"
          }), l2, st.top_heap_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "heap_words:     ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "heap_words:     %*d\n"
          }), l2, st.heap_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "live_words:     ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "live_words:     %*d\n"
          }), l2, st.live_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "free_words:     ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "free_words:     %*d\n"
          }), l2, st.free_words);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "largest_free:   ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "largest_free:   %*d\n"
          }), l2, st.largest_free);
  Curry._2(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "fragments:      ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "fragments:      %*d\n"
          }), l2, st.fragments);
  Printf.fprintf(c, /* Format */{
        _0: {
          TAG: /* Char_literal */12,
          _0: /* "\n" */10,
          _1: /* End_of_format */0
        },
        _1: "\n"
      });
  Curry._1(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "live_blocks: ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "live_blocks: %d\n"
          }), st.live_blocks);
  Curry._1(Printf.fprintf(c, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "free_blocks: ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* "\n" */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "free_blocks: %d\n"
          }), st.free_blocks);
  return Curry._1(Printf.fprintf(c, /* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "heap_chunks: ",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* "\n" */10,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "heap_chunks: %d\n"
                }), st.heap_chunks);
}

function allocated_bytes(param) {
  var match = Caml_gc.caml_gc_counters(undefined);
  return (match[0] + match[2] - match[1]) * 4;
}

function finalise_last(param, param$1) {
  
}

function call_alarm(arec) {
  if (arec.active.contents) {
    Caml_gc.caml_final_register(call_alarm, arec);
    return Curry._1(arec.f, undefined);
  }
  
}

function create_alarm(f) {
  var arec_active = {
    contents: true
  };
  var arec = {
    active: arec_active,
    f: f
  };
  Caml_gc.caml_final_register(call_alarm, arec);
  return arec_active;
}

function delete_alarm(a) {
  a.contents = false;
  
}

var finalise = Caml_gc.caml_final_register;

var finalise_release = Caml_gc.caml_final_release;

export {
  stat ,
  quick_stat ,
  get ,
  print_stat ,
  allocated_bytes ,
  finalise ,
  finalise_last ,
  finalise_release ,
  create_alarm ,
  delete_alarm ,
  
}
/* No side effect */
