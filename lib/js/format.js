'use strict';

var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var $$String = require("./string.js");
var Caml_io = require("./caml_io.js");
var Caml_obj = require("./caml_obj.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_string = require("./caml_string.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_exceptions = require("./caml_exceptions.js");
var CamlinternalFormat = require("./camlinternalFormat.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function add_queue(x, q) {
  var c = /* constructor */{
    tag: "Cons",
    Arg0: /* record */[
      /* head */x,
      /* tail */"Nil"
    ]
  };
  var match = q[/* insert */0];
  if (match !== "Nil") {
    q[/* insert */0] = c;
    match.Arg0[/* tail */1] = c;
    return /* () */0;
  } else {
    q[/* insert */0] = c;
    q[/* body */1] = c;
    return /* () */0;
  }
}

var Empty_queue = Caml_exceptions.create("Format.Empty_queue");

function peek_queue(param) {
  var match = param[/* body */1];
  if (match !== "Nil") {
    return match.Arg0[/* head */0];
  } else {
    throw Empty_queue;
  }
}

function take_queue(q) {
  var match = q[/* body */1];
  if (match !== "Nil") {
    var match$1 = match.Arg0;
    var x = match$1[/* head */0];
    var tl = match$1[/* tail */1];
    q[/* body */1] = tl;
    if (tl === "Nil") {
      q[/* insert */0] = "Nil";
    }
    return x;
  } else {
    throw Empty_queue;
  }
}

function pp_enqueue(state, token) {
  state[/* pp_right_total */12] = state[/* pp_right_total */12] + token[/* length */2] | 0;
  return add_queue(token, state[/* pp_queue */26]);
}

function pp_clear_queue(state) {
  state[/* pp_left_total */11] = 1;
  state[/* pp_right_total */12] = 1;
  var q = state[/* pp_queue */26];
  q[/* insert */0] = "Nil";
  q[/* body */1] = "Nil";
  return /* () */0;
}

function pp_output_string(state, s) {
  return Curry._3(state[/* pp_out_string */16], s, 0, s.length);
}

function break_new_line(state, offset, width) {
  Curry._1(state[/* pp_out_newline */18], /* () */0);
  state[/* pp_is_new_line */10] = true;
  var indent = (state[/* pp_margin */5] - width | 0) + offset | 0;
  var real_indent = Caml_primitive.caml_int_min(state[/* pp_max_indent */7], indent);
  state[/* pp_current_indent */9] = real_indent;
  state[/* pp_space_left */8] = state[/* pp_margin */5] - state[/* pp_current_indent */9] | 0;
  return Curry._1(state[/* pp_out_spaces */19], state[/* pp_current_indent */9]);
}

function break_same_line(state, width) {
  state[/* pp_space_left */8] = state[/* pp_space_left */8] - width | 0;
  return Curry._1(state[/* pp_out_spaces */19], width);
}

function pp_force_break_line(state) {
  var match = state[/* pp_format_stack */1];
  if (match !== "[]") {
    var match$1 = match.Arg0;
    var width = match$1.Arg1;
    if (width > state[/* pp_space_left */8]) {
      switch (match$1.Arg0) {
        case "Pp_hbox" :
        case "Pp_fits" :
            return /* () */0;
        default:
          return break_new_line(state, 0, width);
      }
    } else {
      return 0;
    }
  } else {
    return Curry._1(state[/* pp_out_newline */18], /* () */0);
  }
}

function format_pp_token(state, size, param) {
  if (typeof param === "string") {
    switch (param) {
      case "Pp_stab" :
          var match = state[/* pp_tbox_stack */2];
          if (match !== "[]") {
            var tabs = match.Arg0.Arg0;
            var add_tab = function (n, ls) {
              if (ls !== "[]") {
                var x = ls.Arg0;
                if (Caml_obj.caml_lessthan(n, x)) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: n,
                          Arg1: ls
                        };
                } else {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x,
                          Arg1: add_tab(n, ls.Arg1)
                        };
                }
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: n,
                        Arg1: "[]"
                      };
              }
            };
            tabs[0] = add_tab(state[/* pp_margin */5] - state[/* pp_space_left */8] | 0, tabs[0]);
            return /* () */0;
          } else {
            return /* () */0;
          }
      case "Pp_end" :
          var match$1 = state[/* pp_format_stack */1];
          if (match$1 !== "[]") {
            state[/* pp_format_stack */1] = match$1.Arg1;
            return /* () */0;
          } else {
            return /* () */0;
          }
      case "Pp_tend" :
          var match$2 = state[/* pp_tbox_stack */2];
          if (match$2 !== "[]") {
            state[/* pp_tbox_stack */2] = match$2.Arg1;
            return /* () */0;
          } else {
            return /* () */0;
          }
      case "Pp_newline" :
          var match$3 = state[/* pp_format_stack */1];
          if (match$3 !== "[]") {
            return break_new_line(state, 0, match$3.Arg0.Arg1);
          } else {
            return Curry._1(state[/* pp_out_newline */18], /* () */0);
          }
      case "Pp_if_newline" :
          if (state[/* pp_current_indent */9] !== (state[/* pp_margin */5] - state[/* pp_space_left */8] | 0)) {
            var state$1 = state;
            var match$4 = take_queue(state$1[/* pp_queue */26]);
            var size$1 = match$4[/* elem_size */0];
            state$1[/* pp_left_total */11] = state$1[/* pp_left_total */11] - match$4[/* length */2] | 0;
            state$1[/* pp_space_left */8] = state$1[/* pp_space_left */8] + size$1 | 0;
            return /* () */0;
          } else {
            return 0;
          }
      case "Pp_close_tag" :
          var match$5 = state[/* pp_mark_stack */4];
          if (match$5 !== "[]") {
            var marker = Curry._1(state[/* pp_mark_close_tag */23], match$5.Arg0);
            pp_output_string(state, marker);
            state[/* pp_mark_stack */4] = match$5.Arg1;
            return /* () */0;
          } else {
            return /* () */0;
          }
      
    }
  } else {
    switch (/* XXX */param.tag) {
      case "Pp_text" :
          state[/* pp_space_left */8] = state[/* pp_space_left */8] - size | 0;
          pp_output_string(state, param.Arg0);
          state[/* pp_is_new_line */10] = false;
          return /* () */0;
      case "Pp_break" :
          var off = param.Arg1;
          var n = param.Arg0;
          var match$6 = state[/* pp_format_stack */1];
          if (match$6 !== "[]") {
            var match$7 = match$6.Arg0;
            var width = match$7.Arg1;
            switch (match$7.Arg0) {
              case "Pp_vbox" :
              case "Pp_hvbox" :
                  return break_new_line(state, off, width);
              case "Pp_hovbox" :
                  if (size > state[/* pp_space_left */8]) {
                    return break_new_line(state, off, width);
                  } else {
                    return break_same_line(state, n);
                  }
              case "Pp_box" :
                  if (state[/* pp_is_new_line */10] || !(size > state[/* pp_space_left */8] || state[/* pp_current_indent */9] > ((state[/* pp_margin */5] - width | 0) + off | 0))) {
                    return break_same_line(state, n);
                  } else {
                    return break_new_line(state, off, width);
                  }
              case "Pp_hbox" :
              case "Pp_fits" :
                  return break_same_line(state, n);
              
            }
          } else {
            return /* () */0;
          }
      case "Pp_tbreak" :
          var insertion_point = state[/* pp_margin */5] - state[/* pp_space_left */8] | 0;
          var match$8 = state[/* pp_tbox_stack */2];
          if (match$8 !== "[]") {
            var tabs$1 = match$8.Arg0.Arg0;
            var find = function (n, _param) {
              while(true) {
                var param = _param;
                if (param !== "[]") {
                  var x = param.Arg0;
                  if (Caml_obj.caml_greaterequal(x, n)) {
                    return x;
                  } else {
                    _param = param.Arg1;
                    continue ;
                  }
                } else {
                  throw Caml_builtin_exceptions.not_found;
                }
              };
            };
            var match$9 = tabs$1[0];
            var tab;
            if (match$9 !== "[]") {
              try {
                tab = find(insertion_point, tabs$1[0]);
              }
              catch (exn){
                if (exn === Caml_builtin_exceptions.not_found) {
                  tab = match$9.Arg0;
                } else {
                  throw exn;
                }
              }
            } else {
              tab = insertion_point;
            }
            var offset = tab - insertion_point | 0;
            if (offset >= 0) {
              return break_same_line(state, offset + param.Arg0 | 0);
            } else {
              return break_new_line(state, tab + param.Arg1 | 0, state[/* pp_margin */5]);
            }
          } else {
            return /* () */0;
          }
      case "Pp_begin" :
          var ty = param.Arg1;
          var insertion_point$1 = state[/* pp_margin */5] - state[/* pp_space_left */8] | 0;
          if (insertion_point$1 > state[/* pp_max_indent */7]) {
            pp_force_break_line(state);
          }
          var offset$1 = state[/* pp_space_left */8] - param.Arg0 | 0;
          var bl_type;
          bl_type = ty === "Pp_vbox" ? "Pp_vbox" : (
              size > state[/* pp_space_left */8] ? ty : "Pp_fits"
            );
          state[/* pp_format_stack */1] = /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "Format_elem",
              Arg0: bl_type,
              Arg1: offset$1
            },
            Arg1: state[/* pp_format_stack */1]
          };
          return /* () */0;
      case "Pp_tbegin" :
          state[/* pp_tbox_stack */2] = /* constructor */{
            tag: "::",
            Arg0: param.Arg0,
            Arg1: state[/* pp_tbox_stack */2]
          };
          return /* () */0;
      case "Pp_open_tag" :
          var tag_name = param.Arg0;
          var marker$1 = Curry._1(state[/* pp_mark_open_tag */22], tag_name);
          pp_output_string(state, marker$1);
          state[/* pp_mark_stack */4] = /* constructor */{
            tag: "::",
            Arg0: tag_name,
            Arg1: state[/* pp_mark_stack */4]
          };
          return /* () */0;
      
    }
  }
}

function advance_left(state) {
  try {
    var state$1 = state;
    while(true) {
      var match = peek_queue(state$1[/* pp_queue */26]);
      var size = match[/* elem_size */0];
      if (size < 0 && (state$1[/* pp_right_total */12] - state$1[/* pp_left_total */11] | 0) < state$1[/* pp_space_left */8]) {
        return 0;
      } else {
        take_queue(state$1[/* pp_queue */26]);
        format_pp_token(state$1, size < 0 ? 1000000010 : size, match[/* token */1]);
        state$1[/* pp_left_total */11] = match[/* length */2] + state$1[/* pp_left_total */11] | 0;
        continue ;
      }
    };
  }
  catch (exn){
    if (exn === Empty_queue) {
      return /* () */0;
    } else {
      throw exn;
    }
  }
}

function enqueue_advance(state, tok) {
  pp_enqueue(state, tok);
  return advance_left(state);
}

function enqueue_string_as(state, size, s) {
  return enqueue_advance(state, /* record */[
              /* elem_size */size,
              /* token : constructor */{
                tag: "Pp_text",
                Arg0: s
              },
              /* length */size
            ]);
}

var q_elem = /* record */[
  /* elem_size */-1,
  /* token : constructor */{
    tag: "Pp_text",
    Arg0: ""
  },
  /* length */0
];

var scan_stack_bottom = /* constructor */{
  tag: "::",
  Arg0: /* constructor */{
    tag: "Scan_elem",
    Arg0: -1,
    Arg1: q_elem
  },
  Arg1: "[]"
};

function set_size(state, ty) {
  var match = state[/* pp_scan_stack */0];
  if (match !== "[]") {
    var match$1 = match.Arg0;
    var queue_elem = match$1.Arg1;
    var size = queue_elem[/* elem_size */0];
    var t = match.Arg1;
    if (match$1.Arg0 < state[/* pp_left_total */11]) {
      state[/* pp_scan_stack */0] = scan_stack_bottom;
      return /* () */0;
    } else {
      var tmp = queue_elem[/* token */1];
      if (typeof tmp === "string") {
        return /* () */0;
      } else {
        switch (/* XXX */tmp.tag) {
          case "Pp_break" :
          case "Pp_tbreak" :
              break;
          case "Pp_begin" :
              if (ty) {
                return 0;
              } else {
                queue_elem[/* elem_size */0] = state[/* pp_right_total */12] + size | 0;
                state[/* pp_scan_stack */0] = t;
                return /* () */0;
              }
          default:
            return /* () */0;
        }
      }
      if (ty) {
        queue_elem[/* elem_size */0] = state[/* pp_right_total */12] + size | 0;
        state[/* pp_scan_stack */0] = t;
        return /* () */0;
      } else {
        return 0;
      }
    }
  } else {
    return /* () */0;
  }
}

function scan_push(state, b, tok) {
  pp_enqueue(state, tok);
  if (b) {
    set_size(state, true);
  }
  state[/* pp_scan_stack */0] = /* constructor */{
    tag: "::",
    Arg0: /* constructor */{
      tag: "Scan_elem",
      Arg0: state[/* pp_right_total */12],
      Arg1: tok
    },
    Arg1: state[/* pp_scan_stack */0]
  };
  return /* () */0;
}

function pp_open_box_gen(state, indent, br_ty) {
  state[/* pp_curr_depth */13] = state[/* pp_curr_depth */13] + 1 | 0;
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */-state[/* pp_right_total */12] | 0,
      /* token : constructor */{
        tag: "Pp_begin",
        Arg0: indent,
        Arg1: br_ty
      },
      /* length */0
    ];
    return scan_push(state, false, elem);
  } else if (state[/* pp_curr_depth */13] === state[/* pp_max_boxes */14]) {
    var state$1 = state;
    var s = state[/* pp_ellipsis */15];
    var len = s.length;
    return enqueue_string_as(state$1, len, s);
  } else {
    return 0;
  }
}

function pp_close_box(state, param) {
  if (state[/* pp_curr_depth */13] > 1) {
    if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
      pp_enqueue(state, /* record */[
            /* elem_size */0,
            /* token */"Pp_end",
            /* length */0
          ]);
      set_size(state, true);
      set_size(state, false);
    }
    state[/* pp_curr_depth */13] = state[/* pp_curr_depth */13] - 1 | 0;
    return /* () */0;
  } else {
    return 0;
  }
}

function pp_open_tag(state, tag_name) {
  if (state[/* pp_print_tags */20]) {
    state[/* pp_tag_stack */3] = /* constructor */{
      tag: "::",
      Arg0: tag_name,
      Arg1: state[/* pp_tag_stack */3]
    };
    Curry._1(state[/* pp_print_open_tag */24], tag_name);
  }
  if (state[/* pp_mark_tags */21]) {
    return pp_enqueue(state, /* record */[
                /* elem_size */0,
                /* token : constructor */{
                  tag: "Pp_open_tag",
                  Arg0: tag_name
                },
                /* length */0
              ]);
  } else {
    return 0;
  }
}

function pp_close_tag(state, param) {
  if (state[/* pp_mark_tags */21]) {
    pp_enqueue(state, /* record */[
          /* elem_size */0,
          /* token */"Pp_close_tag",
          /* length */0
        ]);
  }
  if (state[/* pp_print_tags */20]) {
    var match = state[/* pp_tag_stack */3];
    if (match !== "[]") {
      Curry._1(state[/* pp_print_close_tag */25], match.Arg0);
      state[/* pp_tag_stack */3] = match.Arg1;
      return /* () */0;
    } else {
      return /* () */0;
    }
  } else {
    return 0;
  }
}

function pp_set_print_tags(state, b) {
  state[/* pp_print_tags */20] = b;
  return /* () */0;
}

function pp_set_mark_tags(state, b) {
  state[/* pp_mark_tags */21] = b;
  return /* () */0;
}

function pp_get_print_tags(state, param) {
  return state[/* pp_print_tags */20];
}

function pp_get_mark_tags(state, param) {
  return state[/* pp_mark_tags */21];
}

function pp_set_tags(state, b) {
  state[/* pp_print_tags */20] = b;
  state[/* pp_mark_tags */21] = b;
  return /* () */0;
}

function pp_get_formatter_tag_functions(state, param) {
  return /* record */[
          /* mark_open_tag */state[/* pp_mark_open_tag */22],
          /* mark_close_tag */state[/* pp_mark_close_tag */23],
          /* print_open_tag */state[/* pp_print_open_tag */24],
          /* print_close_tag */state[/* pp_print_close_tag */25]
        ];
}

function pp_set_formatter_tag_functions(state, param) {
  state[/* pp_mark_open_tag */22] = param[/* mark_open_tag */0];
  state[/* pp_mark_close_tag */23] = param[/* mark_close_tag */1];
  state[/* pp_print_open_tag */24] = param[/* print_open_tag */2];
  state[/* pp_print_close_tag */25] = param[/* print_close_tag */3];
  return /* () */0;
}

function pp_rinit(state) {
  pp_clear_queue(state);
  state[/* pp_scan_stack */0] = scan_stack_bottom;
  state[/* pp_format_stack */1] = "[]";
  state[/* pp_tbox_stack */2] = "[]";
  state[/* pp_tag_stack */3] = "[]";
  state[/* pp_mark_stack */4] = "[]";
  state[/* pp_current_indent */9] = 0;
  state[/* pp_curr_depth */13] = 0;
  state[/* pp_space_left */8] = state[/* pp_margin */5];
  return pp_open_box_gen(state, 0, "Pp_hovbox");
}

function pp_flush_queue(state, b) {
  while(state[/* pp_curr_depth */13] > 1) {
    pp_close_box(state, /* () */0);
  };
  state[/* pp_right_total */12] = 1000000010;
  advance_left(state);
  if (b) {
    Curry._1(state[/* pp_out_newline */18], /* () */0);
  }
  return pp_rinit(state);
}

function pp_print_as_size(state, size, s) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    return enqueue_string_as(state, size, s);
  } else {
    return 0;
  }
}

var pp_print_as = pp_print_as_size;

function pp_print_string(state, s) {
  return pp_print_as(state, s.length, s);
}

function pp_print_int(state, i) {
  return pp_print_string(state, String(i));
}

function pp_print_float(state, f) {
  return pp_print_string(state, Pervasives.string_of_float(f));
}

function pp_print_bool(state, b) {
  return pp_print_string(state, b ? "true" : "false");
}

function pp_print_char(state, c) {
  return pp_print_as(state, 1, Caml_bytes.bytes_to_string(Bytes.make(1, c)));
}

function pp_open_hbox(state, param) {
  return pp_open_box_gen(state, 0, "Pp_hbox");
}

function pp_open_vbox(state, indent) {
  return pp_open_box_gen(state, indent, "Pp_vbox");
}

function pp_open_hvbox(state, indent) {
  return pp_open_box_gen(state, indent, "Pp_hvbox");
}

function pp_open_hovbox(state, indent) {
  return pp_open_box_gen(state, indent, "Pp_hovbox");
}

function pp_open_box(state, indent) {
  return pp_open_box_gen(state, indent, "Pp_box");
}

function pp_print_newline(state, param) {
  pp_flush_queue(state, true);
  return Curry._1(state[/* pp_out_flush */17], /* () */0);
}

function pp_print_flush(state, param) {
  pp_flush_queue(state, false);
  return Curry._1(state[/* pp_out_flush */17], /* () */0);
}

function pp_force_newline(state, param) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    return enqueue_advance(state, /* record */[
                /* elem_size */0,
                /* token */"Pp_newline",
                /* length */0
              ]);
  } else {
    return 0;
  }
}

function pp_print_if_newline(state, param) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    return enqueue_advance(state, /* record */[
                /* elem_size */0,
                /* token */"Pp_if_newline",
                /* length */0
              ]);
  } else {
    return 0;
  }
}

function pp_print_break(state, width, offset) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */-state[/* pp_right_total */12] | 0,
      /* token : constructor */{
        tag: "Pp_break",
        Arg0: width,
        Arg1: offset
      },
      /* length */width
    ];
    return scan_push(state, true, elem);
  } else {
    return 0;
  }
}

function pp_print_space(state, param) {
  return pp_print_break(state, 1, 0);
}

function pp_print_cut(state, param) {
  return pp_print_break(state, 0, 0);
}

function pp_open_tbox(state, param) {
  state[/* pp_curr_depth */13] = state[/* pp_curr_depth */13] + 1 | 0;
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */0,
      /* token : constructor */{
        tag: "Pp_tbegin",
        Arg0: /* constructor */{
          tag: "Pp_tbox",
          Arg0: /* record */[/* contents */"[]"]
        }
      },
      /* length */0
    ];
    return enqueue_advance(state, elem);
  } else {
    return 0;
  }
}

function pp_close_tbox(state, param) {
  if (state[/* pp_curr_depth */13] > 1 && state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */0,
      /* token */"Pp_tend",
      /* length */0
    ];
    enqueue_advance(state, elem);
    state[/* pp_curr_depth */13] = state[/* pp_curr_depth */13] - 1 | 0;
    return /* () */0;
  } else {
    return 0;
  }
}

function pp_print_tbreak(state, width, offset) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */-state[/* pp_right_total */12] | 0,
      /* token : constructor */{
        tag: "Pp_tbreak",
        Arg0: width,
        Arg1: offset
      },
      /* length */width
    ];
    return scan_push(state, true, elem);
  } else {
    return 0;
  }
}

function pp_print_tab(state, param) {
  return pp_print_tbreak(state, 0, 0);
}

function pp_set_tab(state, param) {
  if (state[/* pp_curr_depth */13] < state[/* pp_max_boxes */14]) {
    var elem = /* record */[
      /* elem_size */0,
      /* token */"Pp_stab",
      /* length */0
    ];
    return enqueue_advance(state, elem);
  } else {
    return 0;
  }
}

function pp_print_list(_$staropt$star, pp_v, ppf, _param) {
  while(true) {
    var param = _param;
    var $staropt$star = _$staropt$star;
    var pp_sep = $staropt$star !== undefined ? $staropt$star : pp_print_cut;
    if (param !== "[]") {
      var vs = param.Arg1;
      var v = param.Arg0;
      if (vs !== "[]") {
        Curry._2(pp_v, ppf, v);
        Curry._2(pp_sep, ppf, /* () */0);
        _param = vs;
        _$staropt$star = pp_sep;
        continue ;
      } else {
        return Curry._2(pp_v, ppf, v);
      }
    } else {
      return /* () */0;
    }
  };
}

function pp_print_text(ppf, s) {
  var len = s.length;
  var left = /* record */[/* contents */0];
  var right = /* record */[/* contents */0];
  var flush = function (param) {
    pp_print_string(ppf, $$String.sub(s, left[0], right[0] - left[0] | 0));
    right[0] = right[0] + 1 | 0;
    left[0] = right[0];
    return /* () */0;
  };
  while(right[0] !== len) {
    var match = Caml_string.get(s, right[0]);
    if (match !== 10) {
      if (match !== 32) {
        right[0] = right[0] + 1 | 0;
      } else {
        flush(/* () */0);
        pp_print_break(ppf, 1, 0);
      }
    } else {
      flush(/* () */0);
      pp_force_newline(ppf, /* () */0);
    }
  };
  if (left[0] !== len) {
    return flush(/* () */0);
  } else {
    return 0;
  }
}

function pp_set_max_boxes(state, n) {
  if (n > 1) {
    state[/* pp_max_boxes */14] = n;
    return /* () */0;
  } else {
    return 0;
  }
}

function pp_get_max_boxes(state, param) {
  return state[/* pp_max_boxes */14];
}

function pp_over_max_boxes(state, param) {
  return state[/* pp_curr_depth */13] === state[/* pp_max_boxes */14];
}

function pp_set_ellipsis_text(state, s) {
  state[/* pp_ellipsis */15] = s;
  return /* () */0;
}

function pp_get_ellipsis_text(state, param) {
  return state[/* pp_ellipsis */15];
}

function pp_limit(n) {
  if (n < 1000000010) {
    return n;
  } else {
    return 1000000009;
  }
}

function pp_set_max_indent(state, n) {
  var state$1 = state;
  var n$1 = state[/* pp_margin */5] - n | 0;
  if (n$1 >= 1) {
    var n$2 = pp_limit(n$1);
    state$1[/* pp_min_space_left */6] = n$2;
    state$1[/* pp_max_indent */7] = state$1[/* pp_margin */5] - state$1[/* pp_min_space_left */6] | 0;
    return pp_rinit(state$1);
  } else {
    return 0;
  }
}

function pp_get_max_indent(state, param) {
  return state[/* pp_max_indent */7];
}

function pp_set_margin(state, n) {
  if (n >= 1) {
    var n$1 = pp_limit(n);
    state[/* pp_margin */5] = n$1;
    var new_max_indent = state[/* pp_max_indent */7] <= state[/* pp_margin */5] ? state[/* pp_max_indent */7] : Caml_primitive.caml_int_max(Caml_primitive.caml_int_max(state[/* pp_margin */5] - state[/* pp_min_space_left */6] | 0, state[/* pp_margin */5] / 2 | 0), 1);
    return pp_set_max_indent(state, new_max_indent);
  } else {
    return 0;
  }
}

function pp_get_margin(state, param) {
  return state[/* pp_margin */5];
}

function pp_set_formatter_out_functions(state, param) {
  state[/* pp_out_string */16] = param[/* out_string */0];
  state[/* pp_out_flush */17] = param[/* out_flush */1];
  state[/* pp_out_newline */18] = param[/* out_newline */2];
  state[/* pp_out_spaces */19] = param[/* out_spaces */3];
  return /* () */0;
}

function pp_get_formatter_out_functions(state, param) {
  return /* record */[
          /* out_string */state[/* pp_out_string */16],
          /* out_flush */state[/* pp_out_flush */17],
          /* out_newline */state[/* pp_out_newline */18],
          /* out_spaces */state[/* pp_out_spaces */19]
        ];
}

function pp_set_formatter_output_functions(state, f, g) {
  state[/* pp_out_string */16] = f;
  state[/* pp_out_flush */17] = g;
  return /* () */0;
}

function pp_get_formatter_output_functions(state, param) {
  return /* tuple */[
          state[/* pp_out_string */16],
          state[/* pp_out_flush */17]
        ];
}

function pp_set_all_formatter_output_functions(state, f, g, h, i) {
  pp_set_formatter_output_functions(state, f, g);
  state[/* pp_out_newline */18] = h;
  state[/* pp_out_spaces */19] = i;
  return /* () */0;
}

function pp_get_all_formatter_output_functions(state, param) {
  return /* tuple */[
          state[/* pp_out_string */16],
          state[/* pp_out_flush */17],
          state[/* pp_out_newline */18],
          state[/* pp_out_spaces */19]
        ];
}

function display_newline(state, param) {
  return Curry._3(state[/* pp_out_string */16], "\n", 0, 1);
}

var blank_line = Caml_bytes.bytes_to_string(Bytes.make(80, /* " " */32));

function display_blanks(state, _n) {
  while(true) {
    var n = _n;
    if (n > 0) {
      if (n <= 80) {
        return Curry._3(state[/* pp_out_string */16], blank_line, 0, n);
      } else {
        Curry._3(state[/* pp_out_string */16], blank_line, 0, 80);
        _n = n - 80 | 0;
        continue ;
      }
    } else {
      return 0;
    }
  };
}

function pp_set_formatter_out_channel(state, os) {
  state[/* pp_out_string */16] = (function (param, param$1, param$2) {
      return Pervasives.output_substring(os, param, param$1, param$2);
    });
  state[/* pp_out_flush */17] = (function (param) {
      return Caml_io.caml_ml_flush(os);
    });
  state[/* pp_out_newline */18] = (function (param) {
      return display_newline(state, param);
    });
  state[/* pp_out_spaces */19] = (function (param) {
      return display_blanks(state, param);
    });
  return /* () */0;
}

function default_pp_mark_open_tag(s) {
  return "<" + (s + ">");
}

function default_pp_mark_close_tag(s) {
  return "</" + (s + ">");
}

function default_pp_print_open_tag(prim) {
  return /* () */0;
}

function default_pp_print_close_tag(prim) {
  return /* () */0;
}

function pp_make_formatter(f, g, h, i) {
  var pp_q = /* record */[
    /* insert */"Nil",
    /* body */"Nil"
  ];
  var sys_tok = /* record */[
    /* elem_size */-1,
    /* token : constructor */{
      tag: "Pp_begin",
      Arg0: 0,
      Arg1: "Pp_hovbox"
    },
    /* length */0
  ];
  add_queue(sys_tok, pp_q);
  var sys_scan_stack = /* constructor */{
    tag: "::",
    Arg0: /* constructor */{
      tag: "Scan_elem",
      Arg0: 1,
      Arg1: sys_tok
    },
    Arg1: scan_stack_bottom
  };
  return /* record */[
          /* pp_scan_stack */sys_scan_stack,
          /* pp_format_stack */"[]",
          /* pp_tbox_stack */"[]",
          /* pp_tag_stack */"[]",
          /* pp_mark_stack */"[]",
          /* pp_margin */78,
          /* pp_min_space_left */10,
          /* pp_max_indent */68,
          /* pp_space_left */78,
          /* pp_current_indent */0,
          /* pp_is_new_line */true,
          /* pp_left_total */1,
          /* pp_right_total */1,
          /* pp_curr_depth */1,
          /* pp_max_boxes */Pervasives.max_int,
          /* pp_ellipsis */".",
          /* pp_out_string */f,
          /* pp_out_flush */g,
          /* pp_out_newline */h,
          /* pp_out_spaces */i,
          /* pp_print_tags */false,
          /* pp_mark_tags */false,
          /* pp_mark_open_tag */default_pp_mark_open_tag,
          /* pp_mark_close_tag */default_pp_mark_close_tag,
          /* pp_print_open_tag */default_pp_print_open_tag,
          /* pp_print_close_tag */default_pp_print_close_tag,
          /* pp_queue */pp_q
        ];
}

function make_formatter(output, flush) {
  var ppf = pp_make_formatter(output, flush, (function (prim) {
          return /* () */0;
        }), (function (prim) {
          return /* () */0;
        }));
  ppf[/* pp_out_newline */18] = (function (param) {
      return display_newline(ppf, param);
    });
  ppf[/* pp_out_spaces */19] = (function (param) {
      return display_blanks(ppf, param);
    });
  return ppf;
}

function formatter_of_out_channel(oc) {
  return make_formatter((function (param, param$1, param$2) {
                return Pervasives.output_substring(oc, param, param$1, param$2);
              }), (function (param) {
                return Caml_io.caml_ml_flush(oc);
              }));
}

function formatter_of_buffer(b) {
  return make_formatter((function (param, param$1, param$2) {
                return $$Buffer.add_substring(b, param, param$1, param$2);
              }), (function (prim) {
                return /* () */0;
              }));
}

var stdbuf = $$Buffer.create(512);

var std_formatter = formatter_of_out_channel(Pervasives.stdout);

var err_formatter = formatter_of_out_channel(Pervasives.stderr);

var str_formatter = formatter_of_buffer(stdbuf);

function flush_str_formatter(param) {
  pp_flush_queue(str_formatter, false);
  var s = $$Buffer.contents(stdbuf);
  $$Buffer.reset(stdbuf);
  return s;
}

function flush_buf_formatter(buf, ppf) {
  pp_flush_queue(ppf, false);
  var s = $$Buffer.contents(buf);
  $$Buffer.reset(buf);
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
  return pp_print_string(std_formatter, String(param));
}

function print_float(param) {
  return pp_print_string(std_formatter, Pervasives.string_of_float(param));
}

function print_char(param) {
  return pp_print_char(std_formatter, param);
}

function print_bool(param) {
  return pp_print_string(std_formatter, param ? "true" : "false");
}

function print_break(param, param$1) {
  return pp_print_break(std_formatter, param, param$1);
}

function print_cut(param) {
  return pp_print_break(std_formatter, 0, 0);
}

function print_space(param) {
  return pp_print_break(std_formatter, 1, 0);
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
  return pp_print_tbreak(std_formatter, 0, 0);
}

function set_margin(param) {
  return pp_set_margin(std_formatter, param);
}

function get_margin(param) {
  return std_formatter[/* pp_margin */5];
}

function set_max_indent(param) {
  return pp_set_max_indent(std_formatter, param);
}

function get_max_indent(param) {
  return std_formatter[/* pp_max_indent */7];
}

function set_max_boxes(param) {
  return pp_set_max_boxes(std_formatter, param);
}

function get_max_boxes(param) {
  return std_formatter[/* pp_max_boxes */14];
}

function over_max_boxes(param) {
  return pp_over_max_boxes(std_formatter, param);
}

function set_ellipsis_text(param) {
  std_formatter[/* pp_ellipsis */15] = param;
  return /* () */0;
}

function get_ellipsis_text(param) {
  return std_formatter[/* pp_ellipsis */15];
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
  std_formatter[/* pp_print_tags */20] = param;
  return /* () */0;
}

function get_print_tags(param) {
  return std_formatter[/* pp_print_tags */20];
}

function set_mark_tags(param) {
  std_formatter[/* pp_mark_tags */21] = param;
  return /* () */0;
}

function get_mark_tags(param) {
  return std_formatter[/* pp_mark_tags */21];
}

function set_tags(param) {
  return pp_set_tags(std_formatter, param);
}

function compute_tag(output, tag_acc) {
  var buf = $$Buffer.create(16);
  var ppf = formatter_of_buffer(buf);
  Curry._2(output, ppf, tag_acc);
  pp_print_flush(ppf, /* () */0);
  var len = buf[/* position */1];
  if (len < 2) {
    return $$Buffer.contents(buf);
  } else {
    return $$Buffer.sub(buf, 1, len - 2 | 0);
  }
}

function output_formatting_lit(ppf, fmting_lit) {
  if (typeof fmting_lit === "string") {
    switch (fmting_lit) {
      case "Close_box" :
          return pp_close_box(ppf, /* () */0);
      case "Close_tag" :
          return pp_close_tag(ppf, /* () */0);
      case "FFlush" :
          return pp_print_flush(ppf, /* () */0);
      case "Force_newline" :
          return pp_force_newline(ppf, /* () */0);
      case "Flush_newline" :
          return pp_print_newline(ppf, /* () */0);
      case "Escaped_at" :
          return pp_print_char(ppf, /* "@" */64);
      case "Escaped_percent" :
          return pp_print_char(ppf, /* "%" */37);
      
    }
  } else {
    switch (/* XXX */fmting_lit.tag) {
      case "Break" :
          return pp_print_break(ppf, fmting_lit.Arg1, fmting_lit.Arg2);
      case "Magic_size" :
          return /* () */0;
      case "Scan_indic" :
          pp_print_char(ppf, /* "@" */64);
          return pp_print_char(ppf, fmting_lit.Arg0);
      
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
  if (typeof acc === "string") {
    return /* () */0;
  } else {
    switch (/* XXX */acc.tag) {
      case "Acc_formatting_lit" :
          output_acc(ppf, acc.Arg0);
          return output_formatting_lit(ppf, acc.Arg1);
      case "Acc_formatting_gen" :
          var match = acc.Arg1;
          var p$2 = acc.Arg0;
          output_acc(ppf, p$2);
          if (/* XXX */match.tag === "Acc_open_tag") {
            return pp_open_tag(ppf, compute_tag(output_acc, match.Arg0));
          } else {
            var match$1 = CamlinternalFormat.open_box_of_string(compute_tag(output_acc, match.Arg0));
            return pp_open_box_gen(ppf, match$1[0], match$1[1]);
          }
      case "Acc_string_literal" :
          var p$3 = acc.Arg0;
          if (typeof p$3 !== "string" && /* XXX */p$3.tag === "Acc_formatting_lit") {
            var match$2 = p$3.Arg1;
            if (typeof match$2 !== "string" && /* XXX */match$2.tag === "Magic_size") {
              p = p$3.Arg0;
              size = match$2.Arg1;
              s = acc.Arg1;
              exit = 1;
            }
            
          }
          output_acc(ppf, p$3);
          return pp_print_string(ppf, acc.Arg1);
      case "Acc_char_literal" :
          var p$4 = acc.Arg0;
          if (typeof p$4 !== "string" && /* XXX */p$4.tag === "Acc_formatting_lit") {
            var match$3 = p$4.Arg1;
            if (typeof match$3 !== "string" && /* XXX */match$3.tag === "Magic_size") {
              p$1 = p$4.Arg0;
              size$1 = match$3.Arg1;
              c = acc.Arg1;
              exit = 2;
            }
            
          }
          output_acc(ppf, p$4);
          return pp_print_char(ppf, acc.Arg1);
      case "Acc_data_string" :
          var p$5 = acc.Arg0;
          if (typeof p$5 !== "string" && /* XXX */p$5.tag === "Acc_formatting_lit") {
            var match$4 = p$5.Arg1;
            if (typeof match$4 !== "string" && /* XXX */match$4.tag === "Magic_size") {
              p = p$5.Arg0;
              size = match$4.Arg1;
              s = acc.Arg1;
              exit = 1;
            }
            
          }
          output_acc(ppf, p$5);
          return pp_print_string(ppf, acc.Arg1);
      case "Acc_data_char" :
          var p$6 = acc.Arg0;
          if (typeof p$6 !== "string" && /* XXX */p$6.tag === "Acc_formatting_lit") {
            var match$5 = p$6.Arg1;
            if (typeof match$5 !== "string" && /* XXX */match$5.tag === "Magic_size") {
              p$1 = p$6.Arg0;
              size$1 = match$5.Arg1;
              c = acc.Arg1;
              exit = 2;
            }
            
          }
          output_acc(ppf, p$6);
          return pp_print_char(ppf, acc.Arg1);
      case "Acc_delay" :
          output_acc(ppf, acc.Arg0);
          return Curry._1(acc.Arg1, ppf);
      case "Acc_flush" :
          output_acc(ppf, acc.Arg0);
          return pp_print_flush(ppf, /* () */0);
      case "Acc_invalid_arg" :
          output_acc(ppf, acc.Arg0);
          throw [
                Caml_builtin_exceptions.invalid_argument,
                acc.Arg1
              ];
      
    }
  }
  switch (exit) {
    case 1 :
        output_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 :
        output_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, Caml_bytes.bytes_to_string(Bytes.make(1, c)));
    
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
  if (typeof acc === "string") {
    return /* () */0;
  } else {
    switch (/* XXX */acc.tag) {
      case "Acc_formatting_lit" :
          strput_acc(ppf, acc.Arg0);
          return output_formatting_lit(ppf, acc.Arg1);
      case "Acc_formatting_gen" :
          var match = acc.Arg1;
          var p$2 = acc.Arg0;
          strput_acc(ppf, p$2);
          if (/* XXX */match.tag === "Acc_open_tag") {
            return pp_open_tag(ppf, compute_tag(strput_acc, match.Arg0));
          } else {
            var match$1 = CamlinternalFormat.open_box_of_string(compute_tag(strput_acc, match.Arg0));
            return pp_open_box_gen(ppf, match$1[0], match$1[1]);
          }
      case "Acc_string_literal" :
          var p$3 = acc.Arg0;
          if (typeof p$3 !== "string" && /* XXX */p$3.tag === "Acc_formatting_lit") {
            var match$2 = p$3.Arg1;
            if (typeof match$2 !== "string" && /* XXX */match$2.tag === "Magic_size") {
              p = p$3.Arg0;
              size = match$2.Arg1;
              s = acc.Arg1;
              exit = 1;
            }
            
          }
          strput_acc(ppf, p$3);
          return pp_print_string(ppf, acc.Arg1);
      case "Acc_char_literal" :
          var p$4 = acc.Arg0;
          if (typeof p$4 !== "string" && /* XXX */p$4.tag === "Acc_formatting_lit") {
            var match$3 = p$4.Arg1;
            if (typeof match$3 !== "string" && /* XXX */match$3.tag === "Magic_size") {
              p$1 = p$4.Arg0;
              size$1 = match$3.Arg1;
              c = acc.Arg1;
              exit = 2;
            }
            
          }
          strput_acc(ppf, p$4);
          return pp_print_char(ppf, acc.Arg1);
      case "Acc_data_string" :
          var p$5 = acc.Arg0;
          if (typeof p$5 !== "string" && /* XXX */p$5.tag === "Acc_formatting_lit") {
            var match$4 = p$5.Arg1;
            if (typeof match$4 !== "string" && /* XXX */match$4.tag === "Magic_size") {
              p = p$5.Arg0;
              size = match$4.Arg1;
              s = acc.Arg1;
              exit = 1;
            }
            
          }
          strput_acc(ppf, p$5);
          return pp_print_string(ppf, acc.Arg1);
      case "Acc_data_char" :
          var p$6 = acc.Arg0;
          if (typeof p$6 !== "string" && /* XXX */p$6.tag === "Acc_formatting_lit") {
            var match$5 = p$6.Arg1;
            if (typeof match$5 !== "string" && /* XXX */match$5.tag === "Magic_size") {
              p$1 = p$6.Arg0;
              size$1 = match$5.Arg1;
              c = acc.Arg1;
              exit = 2;
            }
            
          }
          strput_acc(ppf, p$6);
          return pp_print_char(ppf, acc.Arg1);
      case "Acc_delay" :
          var p$7 = acc.Arg0;
          if (typeof p$7 !== "string" && /* XXX */p$7.tag === "Acc_formatting_lit") {
            var match$6 = p$7.Arg1;
            if (typeof match$6 !== "string" && /* XXX */match$6.tag === "Magic_size") {
              strput_acc(ppf, p$7.Arg0);
              return pp_print_as_size(ppf, match$6.Arg1, Curry._1(acc.Arg1, /* () */0));
            }
            
          }
          strput_acc(ppf, p$7);
          return pp_print_string(ppf, Curry._1(acc.Arg1, /* () */0));
      case "Acc_flush" :
          strput_acc(ppf, acc.Arg0);
          return pp_print_flush(ppf, /* () */0);
      case "Acc_invalid_arg" :
          strput_acc(ppf, acc.Arg0);
          throw [
                Caml_builtin_exceptions.invalid_argument,
                acc.Arg1
              ];
      
    }
  }
  switch (exit) {
    case 1 :
        strput_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 :
        strput_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, Caml_bytes.bytes_to_string(Bytes.make(1, c)));
    
  }
}

function kfprintf(k, o, param) {
  return CamlinternalFormat.make_printf((function (o, acc) {
                output_acc(o, acc);
                return Curry._1(k, o);
              }), o, "End_of_acc", param.Arg0);
}

function ikfprintf(k, x, param) {
  return CamlinternalFormat.make_printf((function (param, param$1) {
                return Curry._1(k, x);
              }), x, "End_of_acc", param.Arg0);
}

function fprintf(ppf, fmt) {
  return kfprintf((function (prim) {
                return /* () */0;
              }), ppf, fmt);
}

function ifprintf(ppf, fmt) {
  return ikfprintf((function (prim) {
                return /* () */0;
              }), ppf, fmt);
}

function printf(fmt) {
  return fprintf(std_formatter, fmt);
}

function eprintf(fmt) {
  return fprintf(err_formatter, fmt);
}

function ksprintf(k, param) {
  var b = $$Buffer.create(512);
  var ppf = formatter_of_buffer(b);
  var k$prime = function (param, acc) {
    strput_acc(ppf, acc);
    return Curry._1(k, flush_buf_formatter(b, ppf));
  };
  return CamlinternalFormat.make_printf(k$prime, /* () */0, "End_of_acc", param.Arg0);
}

function sprintf(fmt) {
  return ksprintf((function (s) {
                return s;
              }), fmt);
}

function asprintf(param) {
  var b = $$Buffer.create(512);
  var ppf = formatter_of_buffer(b);
  var k$prime = function (ppf, acc) {
    output_acc(ppf, acc);
    pp_flush_queue(ppf, false);
    return flush_buf_formatter(b, ppf);
  };
  return CamlinternalFormat.make_printf(k$prime, ppf, "End_of_acc", param.Arg0);
}

function bprintf(b, param) {
  var k = function (ppf, acc) {
    output_acc(ppf, acc);
    return pp_flush_queue(ppf, false);
  };
  return CamlinternalFormat.make_printf(k, formatter_of_buffer(b), "End_of_acc", param.Arg0);
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
/* blank_line Not a pure module */
