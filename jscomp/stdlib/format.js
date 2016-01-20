// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var Caml_primitive = require("../runtime/caml_primitive");
var Buffer = require("./buffer");
var $$String = require("./string");
var CamlinternalFormat = require("./camlinternalFormat");

function make_queue() {
  return [
          /* record */0,
          /* Nil */0,
          /* Nil */0
        ];
}

function add_queue(x, q) {
  var c_001 = [
    /* record */0,
    x,
    /* Nil */0
  ];
  var c = [
    /* Cons */0,
    c_001
  ];
  var match = q[1];
  if (match) {
    q[1] = c;
    match[1][2] = c;
    return /* () */0;
  }
  else {
    q[1] = c;
    q[2] = c;
    return /* () */0;
  }
}

var Empty_queue = [
  248,
  "Format.Empty_queue",
  ++ Caml_exceptions.caml_oo_last_id
];

function peek_queue(param) {
  var match = param[2];
  if (match) {
    return match[1][1];
  }
  else {
    throw Empty_queue;
  }
}

function take_queue(q) {
  var match = q[2];
  if (match) {
    var match$1 = match[1];
    var x = match$1[1];
    var tl = match$1[2];
    q[2] = tl;
    if (!tl) {
      q[1] = /* Nil */0;
    }
    return x;
  }
  else {
    throw Empty_queue;
  }
}

function pp_enqueue(state, token) {
  state[13] += token[3];
  return add_queue(token, state[27]);
}

function pp_clear_queue(state) {
  state[12] = 1;
  state[13] = 1;
  var q = state[27];
  q[1] = /* Nil */0;
  q[2] = /* Nil */0;
  return /* () */0;
}

var pp_infinity = 1000000010;

function pp_output_string(state, s) {
  return state[17](s, 0, s.length);
}

function pp_output_newline(state) {
  return state[19](/* () */0);
}

function pp_output_spaces(state, n) {
  return state[20](n);
}

function break_new_line(state, offset, width) {
  pp_output_newline(state);
  state[11] = /* true */1;
  var indent = state[6] - width + offset;
  var real_indent = Pervasives.min(state[8], indent);
  state[10] = real_indent;
  state[9] = state[6] - state[10];
  return pp_output_spaces(state, state[10]);
}

function break_line(state, width) {
  return break_new_line(state, 0, width);
}

function break_same_line(state, width) {
  state[9] -= width;
  return pp_output_spaces(state, width);
}

function pp_force_break_line(state) {
  var match = state[2];
  if (match) {
    var match$1 = match[1];
    var width = match$1[2];
    var bl_ty = match$1[1];
    if (width > state[9]) {
      if (bl_ty !== 0) {
        if (bl_ty >= 5) {
          return /* () */0;
        }
        else {
          return break_line(state, width);
        }
      }
      else {
        return /* () */0;
      }
    }
    else {
      return 0;
    }
  }
  else {
    return pp_output_newline(state);
  }
}

function format_pp_token(state, size, param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          var match = state[3];
          if (match) {
            var tabs = match[1][1];
            var add_tab = function (n, ls) {
              if (ls) {
                var x = ls[1];
                if (Caml_primitive.caml_lessthan(n, x)) {
                  return [
                          /* :: */0,
                          n,
                          ls
                        ];
                }
                else {
                  return [
                          /* :: */0,
                          x,
                          add_tab(n, ls[2])
                        ];
                }
              }
              else {
                return [
                        /* :: */0,
                        n,
                        /* [] */0
                      ];
              }
            };
            tabs[1] = add_tab(state[6] - state[9], tabs[1]);
            return /* () */0;
          }
          else {
            return /* () */0;
          }
          break;
      case 1 : 
          var match$1 = state[2];
          if (match$1) {
            state[2] = match$1[2];
            return /* () */0;
          }
          else {
            return /* () */0;
          }
      case 2 : 
          var match$2 = state[3];
          if (match$2) {
            state[3] = match$2[2];
            return /* () */0;
          }
          else {
            return /* () */0;
          }
      case 3 : 
          var match$3 = state[2];
          if (match$3) {
            return break_line(state, match$3[1][2]);
          }
          else {
            return pp_output_newline(state);
          }
      case 4 : 
          if (state[10] !== state[6] - state[9]) {
            var state$1 = state;
            var match$4 = take_queue(state$1[27]);
            var size$1 = match$4[1];
            state$1[12] -= match$4[3];
            state$1[9] += size$1;
            return /* () */0;
          }
          else {
            return 0;
          }
      case 5 : 
          var match$5 = state[5];
          if (match$5) {
            var marker = state[24](match$5[1]);
            pp_output_string(state, marker);
            state[5] = match$5[2];
            return /* () */0;
          }
          else {
            return /* () */0;
          }
          break;
      
    }
  }
  else {
    switch (param[0]) {
      case 0 : 
          state[9] -= size;
          pp_output_string(state, param[1]);
          state[11] = /* false */0;
          return /* () */0;
      case 1 : 
          var off = param[2];
          var n = param[1];
          var match$6 = state[2];
          if (match$6) {
            var match$7 = match$6[1];
            var width = match$7[2];
            switch (match$7[1]) {
              case 1 : 
              case 2 : 
                  return break_new_line(state, off, width);
              case 3 : 
                  if (size > state[9]) {
                    return break_new_line(state, off, width);
                  }
                  else {
                    return break_same_line(state, n);
                  }
              case 4 : 
                  if (state[11]) {
                    return break_same_line(state, n);
                  }
                  else {
                    if (size > state[9]) {
                      return break_new_line(state, off, width);
                    }
                    else {
                      if (state[10] > state[6] - width + off) {
                        return break_new_line(state, off, width);
                      }
                      else {
                        return break_same_line(state, n);
                      }
                    }
                  }
              case 0 : 
              case 5 : 
                  return break_same_line(state, n);
              
            }
          }
          else {
            return /* () */0;
          }
          break;
      case 2 : 
          var insertion_point = state[6] - state[9];
          var match$8 = state[3];
          if (match$8) {
            var tabs$1 = match$8[1][1];
            var find = function (n, _param) {
              while(/* true */1) {
                var param = _param;
                if (param) {
                  var x = param[1];
                  if (Caml_primitive.caml_greaterequal(x, n)) {
                    return x;
                  }
                  else {
                    _param = param[2];
                  }
                }
                else {
                  throw Caml_exceptions.Not_found;
                }
              };
            };
            var match$9 = tabs$1[1];
            var tab;
            if (match$9) {
              try {
                tab = find(insertion_point, tabs$1[1]);
              }
              catch (exn){
                if (exn === Caml_exceptions.Not_found) {
                  tab = match$9[1];
                }
                else {
                  throw exn;
                }
              }
            }
            else {
              tab = insertion_point;
            }
            var offset = tab - insertion_point;
            if (offset >= 0) {
              return break_same_line(state, offset + param[1]);
            }
            else {
              return break_new_line(state, tab + param[2], state[6]);
            }
          }
          else {
            return /* () */0;
          }
          break;
      case 3 : 
          var ty = param[2];
          var insertion_point$1 = state[6] - state[9];
          if (insertion_point$1 > state[8]) {
            pp_force_break_line(state);
          }
          var offset$1 = state[9] - param[1];
          var bl_type = ty !== 1 ? (
              size > state[9] ? ty : /* Pp_fits */5
            ) : /* Pp_vbox */1;
          state[2] = [
            /* :: */0,
            [
              /* Format_elem */0,
              bl_type,
              offset$1
            ],
            state[2]
          ];
          return /* () */0;
      case 4 : 
          state[3] = [
            /* :: */0,
            param[1],
            state[3]
          ];
          return /* () */0;
      case 5 : 
          var tag_name = param[1];
          var marker$1 = state[23](tag_name);
          pp_output_string(state, marker$1);
          state[5] = [
            /* :: */0,
            tag_name,
            state[5]
          ];
          return /* () */0;
      
    }
  }
}

function advance_left(state) {
  try {
    var state$1 = state;
    while(/* true */1) {
      var match = peek_queue(state$1[27]);
      var size = match[1];
      if (!(size < 0 && state$1[13] - state$1[12] < state$1[9])) {
        take_queue(state$1[27]);
        format_pp_token(state$1, size < 0 ? pp_infinity : size, match[2]);
        return state$1[12] = match[3] + state$1[12];
      }
      else {
        return 0;
      }
    };
  }
  catch (exn){
    if (exn === Empty_queue) {
      return /* () */0;
    }
    else {
      throw exn;
    }
  }
}

function enqueue_advance(state, tok) {
  pp_enqueue(state, tok);
  return advance_left(state);
}

function make_queue_elem(size, tok, len) {
  return [
          /* record */0,
          size,
          tok,
          len
        ];
}

function enqueue_string_as(state, size, s) {
  return enqueue_advance(state, make_queue_elem(size, [
                  /* Pp_text */0,
                  s
                ], size));
}

var q_elem = make_queue_elem(-1, [
      /* Pp_text */0,
      ""
    ], 0);

var scan_stack_bottom_001 = [
  /* Scan_elem */0,
  -1,
  q_elem
];

var scan_stack_bottom = [
  /* :: */0,
  scan_stack_bottom_001,
  /* [] */0
];

function clear_scan_stack(state) {
  state[1] = scan_stack_bottom;
  return /* () */0;
}

function set_size(state, ty) {
  var match = state[1];
  if (match) {
    var match$1 = match[1];
    var queue_elem = match$1[2];
    var size = queue_elem[1];
    var t = match[2];
    if (match$1[1] < state[12]) {
      return clear_scan_stack(state);
    }
    else {
      var exit = 0;
      var $js = queue_elem[2];
      if (typeof $js === "number") {
        return /* () */0;
      }
      else {
        switch ($js[0]) {
          case 1 : 
          case 2 : 
              exit = 1;
              break;
          case 3 : 
              if (!ty) {
                queue_elem[1] = state[13] + size;
                state[1] = t;
                return /* () */0;
              }
              else {
                return 0;
              }
          default:
            return /* () */0;
        }
      }
      if (exit === 1) {
        if (ty) {
          queue_elem[1] = state[13] + size;
          state[1] = t;
          return /* () */0;
        }
        else {
          return 0;
        }
      }
      
    }
  }
  else {
    return /* () */0;
  }
}

function scan_push(state, b, tok) {
  pp_enqueue(state, tok);
  if (b) {
    set_size(state, /* true */1);
  }
  state[1] = [
    /* :: */0,
    [
      /* Scan_elem */0,
      state[13],
      tok
    ],
    state[1]
  ];
  return /* () */0;
}

function pp_open_box_gen(state, indent, br_ty) {
  ++ state[14];
  if (state[14] < state[15]) {
    var elem = make_queue_elem(-state[13], [
          /* Pp_begin */3,
          indent,
          br_ty
        ], 0);
    return scan_push(state, /* false */0, elem);
  }
  else {
    if (state[14] === state[15]) {
      var state$1 = state;
      var s = state[16];
      var len = s.length;
      return enqueue_string_as(state$1, len, s);
    }
    else {
      return 0;
    }
  }
}

function pp_close_box(state, _) {
  if (state[14] > 1) {
    if (state[14] < state[15]) {
      pp_enqueue(state, [
            /* record */0,
            0,
            /* Pp_end */1,
            0
          ]);
      set_size(state, /* true */1);
      set_size(state, /* false */0);
    }
    -- state[14];
    return /* () */0;
  }
  else {
    return 0;
  }
}

function pp_open_tag(state, tag_name) {
  if (state[21]) {
    state[4] = [
      /* :: */0,
      tag_name,
      state[4]
    ];
    state[25](tag_name);
  }
  if (state[22]) {
    return pp_enqueue(state, [
                /* record */0,
                0,
                [
                  /* Pp_open_tag */5,
                  tag_name
                ],
                0
              ]);
  }
  else {
    return 0;
  }
}

function pp_close_tag(state, _) {
  if (state[22]) {
    pp_enqueue(state, [
          /* record */0,
          0,
          /* Pp_close_tag */5,
          0
        ]);
  }
  if (state[21]) {
    var match = state[4];
    if (match) {
      state[26](match[1]);
      state[4] = match[2];
      return /* () */0;
    }
    else {
      return /* () */0;
    }
  }
  else {
    return 0;
  }
}

function pp_set_print_tags(state, b) {
  state[21] = b;
  return /* () */0;
}

function pp_set_mark_tags(state, b) {
  state[22] = b;
  return /* () */0;
}

function pp_get_print_tags(state, _) {
  return state[21];
}

function pp_get_mark_tags(state, _) {
  return state[22];
}

function pp_set_tags(state, b) {
  pp_set_print_tags(state, b);
  return pp_set_mark_tags(state, b);
}

function pp_get_formatter_tag_functions(state, _) {
  return [
          /* record */0,
          state[23],
          state[24],
          state[25],
          state[26]
        ];
}

function pp_set_formatter_tag_functions(state, param) {
  state[23] = param[1];
  state[24] = param[2];
  state[25] = param[3];
  state[26] = param[4];
  return /* () */0;
}

function pp_rinit(state) {
  pp_clear_queue(state);
  clear_scan_stack(state);
  state[2] = /* [] */0;
  state[3] = /* [] */0;
  state[4] = /* [] */0;
  state[5] = /* [] */0;
  state[10] = 0;
  state[14] = 0;
  state[9] = state[6];
  var state$1 = state;
  return pp_open_box_gen(state$1, 0, /* Pp_hovbox */3);
}

function pp_flush_queue(state, b) {
  while(state[14] > 1) {
    pp_close_box(state, /* () */0);
  };
  state[13] = pp_infinity;
  advance_left(state);
  if (b) {
    pp_output_newline(state);
  }
  return pp_rinit(state);
}

function pp_print_as_size(state, size, s) {
  if (state[14] < state[15]) {
    return enqueue_string_as(state, size, s);
  }
  else {
    return 0;
  }
}

function pp_print_as(state, isize, s) {
  return pp_print_as_size(state, isize, s);
}

function pp_print_string(state, s) {
  return pp_print_as(state, s.length, s);
}

function pp_print_int(state, i) {
  return pp_print_string(state, Pervasives.string_of_int(i));
}

function pp_print_float(state, f) {
  return pp_print_string(state, Pervasives.string_of_float(f));
}

function pp_print_bool(state, b) {
  return pp_print_string(state, Pervasives.string_of_bool(b));
}

function pp_print_char(state, c) {
  return pp_print_as(state, 1, $$String.make(1, c));
}

function pp_open_hbox(state, _) {
  return pp_open_box_gen(state, 0, /* Pp_hbox */0);
}

function pp_open_vbox(state, indent) {
  return pp_open_box_gen(state, indent, /* Pp_vbox */1);
}

function pp_open_hvbox(state, indent) {
  return pp_open_box_gen(state, indent, /* Pp_hvbox */2);
}

function pp_open_hovbox(state, indent) {
  return pp_open_box_gen(state, indent, /* Pp_hovbox */3);
}

function pp_open_box(state, indent) {
  return pp_open_box_gen(state, indent, /* Pp_box */4);
}

function pp_print_newline(state, _) {
  pp_flush_queue(state, /* true */1);
  return state[18](/* () */0);
}

function pp_print_flush(state, _) {
  pp_flush_queue(state, /* false */0);
  return state[18](/* () */0);
}

function pp_force_newline(state, _) {
  if (state[14] < state[15]) {
    return enqueue_advance(state, make_queue_elem(0, /* Pp_newline */3, 0));
  }
  else {
    return 0;
  }
}

function pp_print_if_newline(state, _) {
  if (state[14] < state[15]) {
    return enqueue_advance(state, make_queue_elem(0, /* Pp_if_newline */4, 0));
  }
  else {
    return 0;
  }
}

function pp_print_break(state, width, offset) {
  if (state[14] < state[15]) {
    var elem = make_queue_elem(-state[13], [
          /* Pp_break */1,
          width,
          offset
        ], width);
    return scan_push(state, /* true */1, elem);
  }
  else {
    return 0;
  }
}

function pp_print_space(state, _) {
  return pp_print_break(state, 1, 0);
}

function pp_print_cut(state, _) {
  return pp_print_break(state, 0, 0);
}

function pp_open_tbox(state, _) {
  ++ state[14];
  if (state[14] < state[15]) {
    var elem = make_queue_elem(0, [
          /* Pp_tbegin */4,
          [
            /* Pp_tbox */0,
            [
              0,
              /* [] */0
            ]
          ]
        ], 0);
    return enqueue_advance(state, elem);
  }
  else {
    return 0;
  }
}

function pp_close_tbox(state, _) {
  if (state[14] > 1) {
    if (state[14] < state[15]) {
      var elem = make_queue_elem(0, /* Pp_tend */2, 0);
      enqueue_advance(state, elem);
      -- state[14];
      return /* () */0;
    }
    else {
      return 0;
    }
  }
  else {
    return 0;
  }
}

function pp_print_tbreak(state, width, offset) {
  if (state[14] < state[15]) {
    var elem = make_queue_elem(-state[13], [
          /* Pp_tbreak */2,
          width,
          offset
        ], width);
    return scan_push(state, /* true */1, elem);
  }
  else {
    return 0;
  }
}

function pp_print_tab(state, _) {
  return pp_print_tbreak(state, 0, 0);
}

function pp_set_tab(state, _) {
  if (state[14] < state[15]) {
    var elem = make_queue_elem(0, /* Pp_stab */0, 0);
    return enqueue_advance(state, elem);
  }
  else {
    return 0;
  }
}

function pp_print_list(_$staropt$star, pp_v, ppf, _param) {
  while(/* true */1) {
    var param = _param;
    var $staropt$star = _$staropt$star;
    var pp_sep = $staropt$star ? $staropt$star[1] : pp_print_cut;
    if (param) {
      var vs = param[2];
      var v = param[1];
      if (vs) {
        pp_v(ppf, v);
        pp_sep(ppf, /* () */0);
        _param = vs;
        _$staropt$star = [
          /* Some */0,
          pp_sep
        ];
      }
      else {
        return pp_v(ppf, v);
      }
    }
    else {
      return /* () */0;
    }
  };
}

function pp_print_text(ppf, s) {
  var len = s.length;
  var left = [
    0,
    0
  ];
  var right = [
    0,
    0
  ];
  var flush = function () {
    pp_print_string(ppf, $$String.sub(s, left[1], right[1] - left[1]));
    ++ right[1];
    left[1] = right[1];
    return /* () */0;
  };
  while(right[1] !== len) {
    var match = s.charCodeAt(right[1]);
    if (match !== 10) {
      if (match !== 32) {
        ++ right[1];
      }
      else {
        flush(/* () */0);
        pp_print_space(ppf, /* () */0);
      }
    }
    else {
      flush(/* () */0);
      pp_force_newline(ppf, /* () */0);
    }
  };
  if (left[1] !== len) {
    return flush(/* () */0);
  }
  else {
    return 0;
  }
}

function pp_set_max_boxes(state, n) {
  if (n > 1) {
    state[15] = n;
    return /* () */0;
  }
  else {
    return 0;
  }
}

function pp_get_max_boxes(state, _) {
  return state[15];
}

function pp_over_max_boxes(state, _) {
  return +(state[14] === state[15]);
}

function pp_set_ellipsis_text(state, s) {
  state[16] = s;
  return /* () */0;
}

function pp_get_ellipsis_text(state, _) {
  return state[16];
}

function pp_limit(n) {
  if (n < pp_infinity) {
    return n;
  }
  else {
    return -1 + pp_infinity;
  }
}

function pp_set_max_indent(state, n) {
  var state$1 = state;
  var n$1 = state[6] - n;
  if (n$1 >= 1) {
    var n$2 = pp_limit(n$1);
    state$1[7] = n$2;
    state$1[8] = state$1[6] - state$1[7];
    return pp_rinit(state$1);
  }
  else {
    return 0;
  }
}

function pp_get_max_indent(state, _) {
  return state[8];
}

function pp_set_margin(state, n) {
  if (n >= 1) {
    var n$1 = pp_limit(n);
    state[6] = n$1;
    var new_max_indent = state[8] <= state[6] ? state[8] : Pervasives.max(Pervasives.max(state[6] - state[7], state[6] / 2 | 0), 1);
    return pp_set_max_indent(state, new_max_indent);
  }
  else {
    return 0;
  }
}

function pp_get_margin(state, _) {
  return state[6];
}

function pp_set_formatter_out_functions(state, param) {
  state[17] = param[1];
  state[18] = param[2];
  state[19] = param[3];
  state[20] = param[4];
  return /* () */0;
}

function pp_get_formatter_out_functions(state, _) {
  return [
          /* record */0,
          state[17],
          state[18],
          state[19],
          state[20]
        ];
}

function pp_set_formatter_output_functions(state, f, g) {
  state[17] = f;
  state[18] = g;
  return /* () */0;
}

function pp_get_formatter_output_functions(state, _) {
  return [
          /* tuple */0,
          state[17],
          state[18]
        ];
}

function pp_set_all_formatter_output_functions(state, f, g, h, i) {
  pp_set_formatter_output_functions(state, f, g);
  state[19] = h;
  state[20] = i;
  return /* () */0;
}

function pp_get_all_formatter_output_functions(state, _) {
  return [
          /* tuple */0,
          state[17],
          state[18],
          state[19],
          state[20]
        ];
}

function display_newline(state, _) {
  return state[17]("\n", 0, 1);
}

var blank_line = $$String.make(80, /* " " */32);

function display_blanks(state, _n) {
  while(/* true */1) {
    var n = _n;
    if (n > 0) {
      if (n <= 80) {
        return state[17](blank_line, 0, n);
      }
      else {
        state[17](blank_line, 0, 80);
        _n = n - 80;
      }
    }
    else {
      return 0;
    }
  };
}

function pp_set_formatter_out_channel(state, os) {
  state[17] = function (param, param$1, param$2) {
    return Pervasives.output_substring(os, param, param$1, param$2);
  };
  state[18] = function () {
    return Pervasives.flush(os);
  };
  state[19] = function (param) {
    return display_newline(state, param);
  };
  state[20] = function (param) {
    return display_blanks(state, param);
  };
  return /* () */0;
}

function default_pp_mark_open_tag(s) {
  return "<" + (s + ">");
}

function default_pp_mark_close_tag(s) {
  return "</" + (s + ">");
}

function default_pp_print_open_tag(prim) {
  return prim;
}

function default_pp_print_close_tag(prim) {
  return prim;
}

function pp_make_formatter(f, g, h, i) {
  var pp_q = make_queue(/* () */0);
  var sys_tok = make_queue_elem(-1, [
        /* Pp_begin */3,
        0,
        /* Pp_hovbox */3
      ], 0);
  add_queue(sys_tok, pp_q);
  var sys_scan_stack_001 = [
    /* Scan_elem */0,
    1,
    sys_tok
  ];
  var sys_scan_stack = [
    /* :: */0,
    sys_scan_stack_001,
    scan_stack_bottom
  ];
  return [
          /* record */0,
          sys_scan_stack,
          /* [] */0,
          /* [] */0,
          /* [] */0,
          /* [] */0,
          78,
          10,
          78 - 10,
          78,
          0,
          /* true */1,
          1,
          1,
          1,
          Pervasives.max_int,
          ".",
          f,
          g,
          h,
          i,
          /* false */0,
          /* false */0,
          default_pp_mark_open_tag,
          default_pp_mark_close_tag,
          default_pp_print_open_tag,
          default_pp_print_close_tag,
          pp_q
        ];
}

function make_formatter(output, flush) {
  var ppf = pp_make_formatter(output, flush, function (prim) {
        return prim;
      }, function (prim) {
        return prim;
      });
  ppf[19] = function (param) {
    return display_newline(ppf, param);
  };
  ppf[20] = function (param) {
    return display_blanks(ppf, param);
  };
  return ppf;
}

function formatter_of_out_channel(oc) {
  return make_formatter(function (param, param$1, param$2) {
              return Pervasives.output_substring(oc, param, param$1, param$2);
            }, function () {
              return Pervasives.flush(oc);
            });
}

function formatter_of_buffer(b) {
  return make_formatter(function (param, param$1, param$2) {
              return Buffer.add_substring(b, param, param$1, param$2);
            }, function (prim) {
              return prim;
            });
}

var stdbuf = Buffer.create(512);

var std_formatter = formatter_of_out_channel(Pervasives.stdout);

var err_formatter = formatter_of_out_channel(Pervasives.stderr);

var str_formatter = formatter_of_buffer(stdbuf);

function flush_str_formatter() {
  pp_flush_queue(str_formatter, /* false */0);
  var s = Buffer.contents(stdbuf);
  Buffer.reset(stdbuf);
  return s;
}

function flush_buf_formatter(buf, ppf) {
  pp_flush_queue(ppf, /* false */0);
  var s = Buffer.contents(buf);
  Buffer.reset(buf);
  return s;
}

function open_hbox(param) {
  return pp_open_hbox(std_formatter, param);
}

function open_vbox(param) {
  return pp_open_vbox(std_formatter, param);
}

function open_hvbox(param) {
  return pp_open_hvbox(std_formatter, param);
}

function open_hovbox(param) {
  return pp_open_hovbox(std_formatter, param);
}

function open_box(param) {
  return pp_open_box(std_formatter, param);
}

function close_box(param) {
  return pp_close_box(std_formatter, param);
}

function open_tag(param) {
  return pp_open_tag(std_formatter, param);
}

function close_tag(param) {
  return pp_close_tag(std_formatter, param);
}

function print_as(param, param$1) {
  return pp_print_as(std_formatter, param, param$1);
}

function print_string(param) {
  return pp_print_string(std_formatter, param);
}

function print_int(param) {
  return pp_print_int(std_formatter, param);
}

function print_float(param) {
  return pp_print_float(std_formatter, param);
}

function print_char(param) {
  return pp_print_char(std_formatter, param);
}

function print_bool(param) {
  return pp_print_bool(std_formatter, param);
}

function print_break(param, param$1) {
  return pp_print_break(std_formatter, param, param$1);
}

function print_cut(param) {
  return pp_print_cut(std_formatter, param);
}

function print_space(param) {
  return pp_print_space(std_formatter, param);
}

function force_newline(param) {
  return pp_force_newline(std_formatter, param);
}

function print_flush(param) {
  return pp_print_flush(std_formatter, param);
}

function print_newline(param) {
  return pp_print_newline(std_formatter, param);
}

function print_if_newline(param) {
  return pp_print_if_newline(std_formatter, param);
}

function open_tbox(param) {
  return pp_open_tbox(std_formatter, param);
}

function close_tbox(param) {
  return pp_close_tbox(std_formatter, param);
}

function print_tbreak(param, param$1) {
  return pp_print_tbreak(std_formatter, param, param$1);
}

function set_tab(param) {
  return pp_set_tab(std_formatter, param);
}

function print_tab(param) {
  return pp_print_tab(std_formatter, param);
}

function set_margin(param) {
  return pp_set_margin(std_formatter, param);
}

function get_margin(param) {
  return pp_get_margin(std_formatter, param);
}

function set_max_indent(param) {
  return pp_set_max_indent(std_formatter, param);
}

function get_max_indent(param) {
  return pp_get_max_indent(std_formatter, param);
}

function set_max_boxes(param) {
  return pp_set_max_boxes(std_formatter, param);
}

function get_max_boxes(param) {
  return pp_get_max_boxes(std_formatter, param);
}

function over_max_boxes(param) {
  return pp_over_max_boxes(std_formatter, param);
}

function set_ellipsis_text(param) {
  return pp_set_ellipsis_text(std_formatter, param);
}

function get_ellipsis_text(param) {
  return pp_get_ellipsis_text(std_formatter, param);
}

function set_formatter_out_channel(param) {
  return pp_set_formatter_out_channel(std_formatter, param);
}

function set_formatter_out_functions(param) {
  return pp_set_formatter_out_functions(std_formatter, param);
}

function get_formatter_out_functions(param) {
  return pp_get_formatter_out_functions(std_formatter, param);
}

function set_formatter_output_functions(param, param$1) {
  return pp_set_formatter_output_functions(std_formatter, param, param$1);
}

function get_formatter_output_functions(param) {
  return pp_get_formatter_output_functions(std_formatter, param);
}

function set_all_formatter_output_functions(param, param$1, param$2, param$3) {
  return pp_set_all_formatter_output_functions(std_formatter, param, param$1, param$2, param$3);
}

function get_all_formatter_output_functions(param) {
  return pp_get_all_formatter_output_functions(std_formatter, param);
}

function set_formatter_tag_functions(param) {
  return pp_set_formatter_tag_functions(std_formatter, param);
}

function get_formatter_tag_functions(param) {
  return pp_get_formatter_tag_functions(std_formatter, param);
}

function set_print_tags(param) {
  return pp_set_print_tags(std_formatter, param);
}

function get_print_tags(param) {
  return pp_get_print_tags(std_formatter, param);
}

function set_mark_tags(param) {
  return pp_set_mark_tags(std_formatter, param);
}

function get_mark_tags(param) {
  return pp_get_mark_tags(std_formatter, param);
}

function set_tags(param) {
  return pp_set_tags(std_formatter, param);
}

function compute_tag(output, tag_acc) {
  var buf = Buffer.create(16);
  var ppf = formatter_of_buffer(buf);
  output(ppf, tag_acc);
  pp_print_flush(ppf, /* () */0);
  var len = Buffer.length(buf);
  if (len < 2) {
    return Buffer.contents(buf);
  }
  else {
    return Buffer.sub(buf, 1, len - 2);
  }
}

function output_formatting_lit(ppf, fmting_lit) {
  if (typeof fmting_lit === "number") {
    switch (fmting_lit) {
      case 0 : 
          return pp_close_box(ppf, /* () */0);
      case 1 : 
          return pp_close_tag(ppf, /* () */0);
      case 2 : 
          return pp_print_flush(ppf, /* () */0);
      case 3 : 
          return pp_force_newline(ppf, /* () */0);
      case 4 : 
          return pp_print_newline(ppf, /* () */0);
      case 5 : 
          return pp_print_char(ppf, /* "@" */64);
      case 6 : 
          return pp_print_char(ppf, /* "%" */37);
      
    }
  }
  else {
    switch (fmting_lit[0]) {
      case 0 : 
          return pp_print_break(ppf, fmting_lit[2], fmting_lit[3]);
      case 1 : 
          return /* () */0;
      case 2 : 
          pp_print_char(ppf, /* "@" */64);
          return pp_print_char(ppf, fmting_lit[1]);
      
    }
  }
}

function output_acc(ppf, acc) {
  var exit = 0;
  var p;
  var size;
  var s;
  var p$1;
  var size$1;
  var c;
  var p$2;
  var s$1;
  var p$3;
  var c$1;
  if (typeof acc === "number") {
    return /* () */0;
  }
  else {
    switch (acc[0]) {
      case 0 : 
          output_acc(ppf, acc[1]);
          return output_formatting_lit(ppf, acc[2]);
      case 1 : 
          var match = acc[2];
          var p$4 = acc[1];
          output_acc(ppf, p$4);
          if (match[0]) {
            var match$1 = CamlinternalFormat.open_box_of_string(compute_tag(output_acc, match[1]));
            return pp_open_box_gen(ppf, match$1[1], match$1[2]);
          }
          else {
            return pp_open_tag(ppf, compute_tag(output_acc, match[1]));
          }
          break;
      case 2 : 
          var p$5 = acc[1];
          var exit$1 = 0;
          if (typeof p$5 === "number") {
            exit$1 = 5;
          }
          else {
            if (p$5[0]) {
              exit$1 = 5;
            }
            else {
              var match$2 = p$5[2];
              if (typeof match$2 === "number") {
                exit$1 = 5;
              }
              else {
                if (match$2[0] === 1) {
                  p = p$5[1];
                  size = match$2[2];
                  s = acc[2];
                  exit = 1;
                }
                else {
                  exit$1 = 5;
                }
              }
            }
          }
          if (exit$1 === 5) {
            p$2 = p$5;
            s$1 = acc[2];
            exit = 3;
          }
          break;
      case 3 : 
          var p$6 = acc[1];
          var exit$2 = 0;
          if (typeof p$6 === "number") {
            exit$2 = 5;
          }
          else {
            if (p$6[0]) {
              exit$2 = 5;
            }
            else {
              var match$3 = p$6[2];
              if (typeof match$3 === "number") {
                exit$2 = 5;
              }
              else {
                if (match$3[0] === 1) {
                  p$1 = p$6[1];
                  size$1 = match$3[2];
                  c = acc[2];
                  exit = 2;
                }
                else {
                  exit$2 = 5;
                }
              }
            }
          }
          if (exit$2 === 5) {
            p$3 = p$6;
            c$1 = acc[2];
            exit = 4;
          }
          break;
      case 4 : 
          var p$7 = acc[1];
          var exit$3 = 0;
          if (typeof p$7 === "number") {
            exit$3 = 5;
          }
          else {
            if (p$7[0]) {
              exit$3 = 5;
            }
            else {
              var match$4 = p$7[2];
              if (typeof match$4 === "number") {
                exit$3 = 5;
              }
              else {
                if (match$4[0] === 1) {
                  p = p$7[1];
                  size = match$4[2];
                  s = acc[2];
                  exit = 1;
                }
                else {
                  exit$3 = 5;
                }
              }
            }
          }
          if (exit$3 === 5) {
            p$2 = p$7;
            s$1 = acc[2];
            exit = 3;
          }
          break;
      case 5 : 
          var p$8 = acc[1];
          var exit$4 = 0;
          if (typeof p$8 === "number") {
            exit$4 = 5;
          }
          else {
            if (p$8[0]) {
              exit$4 = 5;
            }
            else {
              var match$5 = p$8[2];
              if (typeof match$5 === "number") {
                exit$4 = 5;
              }
              else {
                if (match$5[0] === 1) {
                  p$1 = p$8[1];
                  size$1 = match$5[2];
                  c = acc[2];
                  exit = 2;
                }
                else {
                  exit$4 = 5;
                }
              }
            }
          }
          if (exit$4 === 5) {
            p$3 = p$8;
            c$1 = acc[2];
            exit = 4;
          }
          break;
      case 6 : 
          output_acc(ppf, acc[1]);
          return acc[2](ppf);
      case 7 : 
          output_acc(ppf, acc[1]);
          return pp_print_flush(ppf, /* () */0);
      case 8 : 
          output_acc(ppf, acc[1]);
          return Pervasives.invalid_arg(acc[2]);
      
    }
  }
  switch (exit) {
    case 1 : 
        output_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 : 
        output_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, $$String.make(1, c));
    case 3 : 
        output_acc(ppf, p$2);
        return pp_print_string(ppf, s$1);
    case 4 : 
        output_acc(ppf, p$3);
        return pp_print_char(ppf, c$1);
    
  }
}

function strput_acc(ppf, acc) {
  var exit = 0;
  var p;
  var size;
  var s;
  var p$1;
  var size$1;
  var c;
  var p$2;
  var s$1;
  var p$3;
  var c$1;
  if (typeof acc === "number") {
    return /* () */0;
  }
  else {
    switch (acc[0]) {
      case 0 : 
          strput_acc(ppf, acc[1]);
          return output_formatting_lit(ppf, acc[2]);
      case 1 : 
          var match = acc[2];
          var p$4 = acc[1];
          strput_acc(ppf, p$4);
          if (match[0]) {
            var match$1 = CamlinternalFormat.open_box_of_string(compute_tag(strput_acc, match[1]));
            return pp_open_box_gen(ppf, match$1[1], match$1[2]);
          }
          else {
            return pp_open_tag(ppf, compute_tag(strput_acc, match[1]));
          }
          break;
      case 2 : 
          var p$5 = acc[1];
          var exit$1 = 0;
          if (typeof p$5 === "number") {
            exit$1 = 5;
          }
          else {
            if (p$5[0]) {
              exit$1 = 5;
            }
            else {
              var match$2 = p$5[2];
              if (typeof match$2 === "number") {
                exit$1 = 5;
              }
              else {
                if (match$2[0] === 1) {
                  p = p$5[1];
                  size = match$2[2];
                  s = acc[2];
                  exit = 1;
                }
                else {
                  exit$1 = 5;
                }
              }
            }
          }
          if (exit$1 === 5) {
            p$2 = p$5;
            s$1 = acc[2];
            exit = 3;
          }
          break;
      case 3 : 
          var p$6 = acc[1];
          var exit$2 = 0;
          if (typeof p$6 === "number") {
            exit$2 = 5;
          }
          else {
            if (p$6[0]) {
              exit$2 = 5;
            }
            else {
              var match$3 = p$6[2];
              if (typeof match$3 === "number") {
                exit$2 = 5;
              }
              else {
                if (match$3[0] === 1) {
                  p$1 = p$6[1];
                  size$1 = match$3[2];
                  c = acc[2];
                  exit = 2;
                }
                else {
                  exit$2 = 5;
                }
              }
            }
          }
          if (exit$2 === 5) {
            p$3 = p$6;
            c$1 = acc[2];
            exit = 4;
          }
          break;
      case 4 : 
          var p$7 = acc[1];
          var exit$3 = 0;
          if (typeof p$7 === "number") {
            exit$3 = 5;
          }
          else {
            if (p$7[0]) {
              exit$3 = 5;
            }
            else {
              var match$4 = p$7[2];
              if (typeof match$4 === "number") {
                exit$3 = 5;
              }
              else {
                if (match$4[0] === 1) {
                  p = p$7[1];
                  size = match$4[2];
                  s = acc[2];
                  exit = 1;
                }
                else {
                  exit$3 = 5;
                }
              }
            }
          }
          if (exit$3 === 5) {
            p$2 = p$7;
            s$1 = acc[2];
            exit = 3;
          }
          break;
      case 5 : 
          var p$8 = acc[1];
          var exit$4 = 0;
          if (typeof p$8 === "number") {
            exit$4 = 5;
          }
          else {
            if (p$8[0]) {
              exit$4 = 5;
            }
            else {
              var match$5 = p$8[2];
              if (typeof match$5 === "number") {
                exit$4 = 5;
              }
              else {
                if (match$5[0] === 1) {
                  p$1 = p$8[1];
                  size$1 = match$5[2];
                  c = acc[2];
                  exit = 2;
                }
                else {
                  exit$4 = 5;
                }
              }
            }
          }
          if (exit$4 === 5) {
            p$3 = p$8;
            c$1 = acc[2];
            exit = 4;
          }
          break;
      case 6 : 
          var p$9 = acc[1];
          var exit$5 = 0;
          if (typeof p$9 === "number") {
            exit$5 = 5;
          }
          else {
            if (p$9[0]) {
              exit$5 = 5;
            }
            else {
              var match$6 = p$9[2];
              if (typeof match$6 === "number") {
                exit$5 = 5;
              }
              else {
                if (match$6[0] === 1) {
                  strput_acc(ppf, p$9[1]);
                  return pp_print_as_size(ppf, match$6[2], acc[2](/* () */0));
                }
                else {
                  exit$5 = 5;
                }
              }
            }
          }
          if (exit$5 === 5) {
            strput_acc(ppf, p$9);
            return pp_print_string(ppf, acc[2](/* () */0));
          }
          break;
      case 7 : 
          strput_acc(ppf, acc[1]);
          return pp_print_flush(ppf, /* () */0);
      case 8 : 
          strput_acc(ppf, acc[1]);
          return Pervasives.invalid_arg(acc[2]);
      
    }
  }
  switch (exit) {
    case 1 : 
        strput_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 : 
        strput_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, $$String.make(1, c));
    case 3 : 
        strput_acc(ppf, p$2);
        return pp_print_string(ppf, s$1);
    case 4 : 
        strput_acc(ppf, p$3);
        return pp_print_char(ppf, c$1);
    
  }
}

function kfprintf(k, o, param) {
  return CamlinternalFormat.make_printf(function (o, acc) {
              output_acc(o, acc);
              return k(o);
            }, o, /* End_of_acc */0, param[1]);
}

function ikfprintf(k, x, param) {
  return CamlinternalFormat.make_printf(function (_, _$1) {
              return k(x);
            }, x, /* End_of_acc */0, param[1]);
}

function fprintf(ppf, fmt) {
  return kfprintf(function (prim) {
              return prim;
            }, ppf, fmt);
}

function ifprintf(ppf, fmt) {
  return ikfprintf(function (prim) {
              return prim;
            }, ppf, fmt);
}

function printf(fmt) {
  return fprintf(std_formatter, fmt);
}

function eprintf(fmt) {
  return fprintf(err_formatter, fmt);
}

function ksprintf(k, param) {
  var b = Buffer.create(512);
  var ppf = formatter_of_buffer(b);
  var k$prime = function (_, acc) {
    strput_acc(ppf, acc);
    return k(flush_buf_formatter(b, ppf));
  };
  return CamlinternalFormat.make_printf(k$prime, /* () */0, /* End_of_acc */0, param[1]);
}

function sprintf(fmt) {
  return ksprintf(function (s) {
              return s;
            }, fmt);
}

function asprintf(param) {
  var b = Buffer.create(512);
  var ppf = formatter_of_buffer(b);
  var k$prime = function (ppf, acc) {
    output_acc(ppf, acc);
    pp_flush_queue(ppf, /* false */0);
    return flush_buf_formatter(b, ppf);
  };
  return CamlinternalFormat.make_printf(k$prime, ppf, /* End_of_acc */0, param[1]);
}

function bprintf(b, param) {
  var k = function (ppf, acc) {
    output_acc(ppf, acc);
    return pp_flush_queue(ppf, /* false */0);
  };
  return CamlinternalFormat.make_printf(k, formatter_of_buffer(b), /* End_of_acc */0, param[1]);
}

Pervasives.at_exit(print_flush);

var kprintf = ksprintf;

exports.open_box = open_box;
exports.close_box = close_box;
exports.print_string = print_string;
exports.print_as = print_as;
exports.print_int = print_int;
exports.print_float = print_float;
exports.print_char = print_char;
exports.print_bool = print_bool;
exports.print_space = print_space;
exports.print_cut = print_cut;
exports.print_break = print_break;
exports.print_flush = print_flush;
exports.print_newline = print_newline;
exports.force_newline = force_newline;
exports.print_if_newline = print_if_newline;
exports.set_margin = set_margin;
exports.get_margin = get_margin;
exports.set_max_indent = set_max_indent;
exports.get_max_indent = get_max_indent;
exports.set_max_boxes = set_max_boxes;
exports.get_max_boxes = get_max_boxes;
exports.over_max_boxes = over_max_boxes;
exports.open_hbox = open_hbox;
exports.open_vbox = open_vbox;
exports.open_hvbox = open_hvbox;
exports.open_hovbox = open_hovbox;
exports.open_tbox = open_tbox;
exports.close_tbox = close_tbox;
exports.print_tbreak = print_tbreak;
exports.set_tab = set_tab;
exports.print_tab = print_tab;
exports.set_ellipsis_text = set_ellipsis_text;
exports.get_ellipsis_text = get_ellipsis_text;
exports.open_tag = open_tag;
exports.close_tag = close_tag;
exports.set_tags = set_tags;
exports.set_print_tags = set_print_tags;
exports.set_mark_tags = set_mark_tags;
exports.get_print_tags = get_print_tags;
exports.get_mark_tags = get_mark_tags;
exports.set_formatter_out_channel = set_formatter_out_channel;
exports.set_formatter_output_functions = set_formatter_output_functions;
exports.get_formatter_output_functions = get_formatter_output_functions;
exports.set_formatter_out_functions = set_formatter_out_functions;
exports.get_formatter_out_functions = get_formatter_out_functions;
exports.set_formatter_tag_functions = set_formatter_tag_functions;
exports.get_formatter_tag_functions = get_formatter_tag_functions;
exports.formatter_of_out_channel = formatter_of_out_channel;
exports.std_formatter = std_formatter;
exports.err_formatter = err_formatter;
exports.formatter_of_buffer = formatter_of_buffer;
exports.stdbuf = stdbuf;
exports.str_formatter = str_formatter;
exports.flush_str_formatter = flush_str_formatter;
exports.make_formatter = make_formatter;
exports.pp_open_hbox = pp_open_hbox;
exports.pp_open_vbox = pp_open_vbox;
exports.pp_open_hvbox = pp_open_hvbox;
exports.pp_open_hovbox = pp_open_hovbox;
exports.pp_open_box = pp_open_box;
exports.pp_close_box = pp_close_box;
exports.pp_open_tag = pp_open_tag;
exports.pp_close_tag = pp_close_tag;
exports.pp_print_string = pp_print_string;
exports.pp_print_as = pp_print_as;
exports.pp_print_int = pp_print_int;
exports.pp_print_float = pp_print_float;
exports.pp_print_char = pp_print_char;
exports.pp_print_bool = pp_print_bool;
exports.pp_print_break = pp_print_break;
exports.pp_print_cut = pp_print_cut;
exports.pp_print_space = pp_print_space;
exports.pp_force_newline = pp_force_newline;
exports.pp_print_flush = pp_print_flush;
exports.pp_print_newline = pp_print_newline;
exports.pp_print_if_newline = pp_print_if_newline;
exports.pp_open_tbox = pp_open_tbox;
exports.pp_close_tbox = pp_close_tbox;
exports.pp_print_tbreak = pp_print_tbreak;
exports.pp_set_tab = pp_set_tab;
exports.pp_print_tab = pp_print_tab;
exports.pp_set_tags = pp_set_tags;
exports.pp_set_print_tags = pp_set_print_tags;
exports.pp_set_mark_tags = pp_set_mark_tags;
exports.pp_get_print_tags = pp_get_print_tags;
exports.pp_get_mark_tags = pp_get_mark_tags;
exports.pp_set_margin = pp_set_margin;
exports.pp_get_margin = pp_get_margin;
exports.pp_set_max_indent = pp_set_max_indent;
exports.pp_get_max_indent = pp_get_max_indent;
exports.pp_set_max_boxes = pp_set_max_boxes;
exports.pp_get_max_boxes = pp_get_max_boxes;
exports.pp_over_max_boxes = pp_over_max_boxes;
exports.pp_set_ellipsis_text = pp_set_ellipsis_text;
exports.pp_get_ellipsis_text = pp_get_ellipsis_text;
exports.pp_set_formatter_out_channel = pp_set_formatter_out_channel;
exports.pp_set_formatter_output_functions = pp_set_formatter_output_functions;
exports.pp_get_formatter_output_functions = pp_get_formatter_output_functions;
exports.pp_set_formatter_tag_functions = pp_set_formatter_tag_functions;
exports.pp_get_formatter_tag_functions = pp_get_formatter_tag_functions;
exports.pp_set_formatter_out_functions = pp_set_formatter_out_functions;
exports.pp_get_formatter_out_functions = pp_get_formatter_out_functions;
exports.pp_print_list = pp_print_list;
exports.pp_print_text = pp_print_text;
exports.fprintf = fprintf;
exports.printf = printf;
exports.eprintf = eprintf;
exports.sprintf = sprintf;
exports.asprintf = asprintf;
exports.ifprintf = ifprintf;
exports.kfprintf = kfprintf;
exports.ikfprintf = ikfprintf;
exports.ksprintf = ksprintf;
exports.bprintf = bprintf;
exports.kprintf = kprintf;
exports.set_all_formatter_output_functions = set_all_formatter_output_functions;
exports.get_all_formatter_output_functions = get_all_formatter_output_functions;
exports.pp_set_all_formatter_output_functions = pp_set_all_formatter_output_functions;
exports.pp_get_all_formatter_output_functions = pp_get_all_formatter_output_functions;
/* q_elem Not a pure module */
