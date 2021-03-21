

import * as Caml from "./caml.js";
import * as List from "./list.js";
import * as Curry from "./curry.js";
import * as $$Buffer from "./buffer.js";
import * as $$String from "./string.js";
import * as Caml_io from "./caml_io.js";
import * as Caml_obj from "./caml_obj.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";
import * as CamlinternalFormat from "./camlinternalFormat.js";

function add_queue(x, q) {
  var c = /* Cons */{
    head: x,
    tail: /* Nil */0
  };
  var cell = q.insert;
  if (cell) {
    q.insert = c;
    cell.tail = c;
  } else {
    q.insert = c;
    q.body = c;
  }
  
}

var Empty_queue = /* @__PURE__ */Caml_exceptions.create("Format.Empty_queue");

function peek_queue(param) {
  var match = param.body;
  if (match) {
    return match.head;
  }
  throw {
        RE_EXN_ID: Empty_queue,
        Error: new Error()
      };
}

function take_queue(q) {
  var match = q.body;
  if (match) {
    var tl = match.tail;
    q.body = tl;
    if (tl === /* Nil */0) {
      q.insert = /* Nil */0;
    }
    return match.head;
  }
  throw {
        RE_EXN_ID: Empty_queue,
        Error: new Error()
      };
}

function pp_enqueue(state, token) {
  state.pp_right_total = state.pp_right_total + token.length | 0;
  return add_queue(token, state.pp_queue);
}

function pp_clear_queue(state) {
  state.pp_left_total = 1;
  state.pp_right_total = 1;
  var q = state.pp_queue;
  q.insert = /* Nil */0;
  q.body = /* Nil */0;
  
}

function pp_output_string(state, s) {
  return Curry._3(state.pp_out_string, s, 0, s.length);
}

function break_new_line(state, offset, width) {
  Curry._1(state.pp_out_newline, undefined);
  state.pp_is_new_line = true;
  var indent = (state.pp_margin - width | 0) + offset | 0;
  var real_indent = state.pp_max_indent < indent ? state.pp_max_indent : indent;
  state.pp_current_indent = real_indent;
  state.pp_space_left = state.pp_margin - state.pp_current_indent | 0;
  return Curry._1(state.pp_out_indent, state.pp_current_indent);
}

function break_same_line(state, width) {
  state.pp_space_left = state.pp_space_left - width | 0;
  return Curry._1(state.pp_out_spaces, width);
}

function pp_force_break_line(state) {
  var match = state.pp_format_stack;
  if (!match) {
    return Curry._1(state.pp_out_newline, undefined);
  }
  var match$1 = match.hd;
  var width = match$1._1;
  if (width > state.pp_space_left && (match$1._0 - 1 >>> 0) <= 3) {
    return break_new_line(state, 0, width);
  }
  
}

function format_pp_token(state, size, s) {
  if (typeof s === "number") {
    switch (s) {
      case /* Pp_stab */0 :
          var match = state.pp_tbox_stack;
          if (!match) {
            return ;
          }
          var tabs = match.hd._0;
          var add_tab = function (n, ls) {
            if (!ls) {
              return {
                      hd: n,
                      tl: /* [] */0
                    };
            }
            var x = ls.hd;
            if (Caml_obj.caml_lessthan(n, x)) {
              return {
                      hd: n,
                      tl: ls
                    };
            } else {
              return {
                      hd: x,
                      tl: add_tab(n, ls.tl)
                    };
            }
          };
          tabs.contents = add_tab(state.pp_margin - state.pp_space_left | 0, tabs.contents);
          return ;
      case /* Pp_end */1 :
          var match$1 = state.pp_format_stack;
          if (match$1) {
            state.pp_format_stack = match$1.tl;
            return ;
          } else {
            return ;
          }
      case /* Pp_tend */2 :
          var match$2 = state.pp_tbox_stack;
          if (match$2) {
            state.pp_tbox_stack = match$2.tl;
            return ;
          } else {
            return ;
          }
      case /* Pp_newline */3 :
          var match$3 = state.pp_format_stack;
          if (match$3) {
            return break_new_line(state, 0, match$3.hd._1);
          } else {
            return Curry._1(state.pp_out_newline, undefined);
          }
      case /* Pp_if_newline */4 :
          if (state.pp_current_indent !== (state.pp_margin - state.pp_space_left | 0)) {
            var match$4 = take_queue(state.pp_queue);
            var size$1 = match$4.elem_size;
            state.pp_left_total = state.pp_left_total - match$4.length | 0;
            state.pp_space_left = state.pp_space_left + size$1 | 0;
            return ;
          } else {
            return ;
          }
      case /* Pp_close_tag */5 :
          var match$5 = state.pp_mark_stack;
          if (!match$5) {
            return ;
          }
          var marker = Curry._1(state.pp_mark_close_tag, match$5.hd);
          pp_output_string(state, marker);
          state.pp_mark_stack = match$5.tl;
          return ;
      
    }
  } else {
    switch (s.TAG | 0) {
      case /* Pp_text */0 :
          state.pp_space_left = state.pp_space_left - size | 0;
          pp_output_string(state, s._0);
          state.pp_is_new_line = false;
          return ;
      case /* Pp_break */1 :
          var off = s._1;
          var n = s._0;
          var match$6 = state.pp_format_stack;
          if (!match$6) {
            return ;
          }
          var match$7 = match$6.hd;
          var width = match$7._1;
          switch (match$7._0) {
            case /* Pp_vbox */1 :
            case /* Pp_hvbox */2 :
                return break_new_line(state, off, width);
            case /* Pp_hovbox */3 :
                if (size > state.pp_space_left) {
                  return break_new_line(state, off, width);
                } else {
                  return break_same_line(state, n);
                }
            case /* Pp_box */4 :
                if (state.pp_is_new_line || !(size > state.pp_space_left || state.pp_current_indent > ((state.pp_margin - width | 0) + off | 0))) {
                  return break_same_line(state, n);
                } else {
                  return break_new_line(state, off, width);
                }
            case /* Pp_hbox */0 :
            case /* Pp_fits */5 :
                return break_same_line(state, n);
            
          }
      case /* Pp_tbreak */2 :
          var insertion_point = state.pp_margin - state.pp_space_left | 0;
          var match$8 = state.pp_tbox_stack;
          if (!match$8) {
            return ;
          }
          var tabs$1 = match$8.hd._0;
          var find = function (n, _param) {
            while(true) {
              var param = _param;
              if (param) {
                var x = param.hd;
                if (Caml_obj.caml_greaterequal(x, n)) {
                  return x;
                }
                _param = param.tl;
                continue ;
              }
              throw {
                    RE_EXN_ID: "Not_found",
                    Error: new Error()
                  };
            };
          };
          var match$9 = tabs$1.contents;
          var tab;
          if (match$9) {
            try {
              tab = find(insertion_point, tabs$1.contents);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === "Not_found") {
                tab = match$9.hd;
              } else {
                throw exn;
              }
            }
          } else {
            tab = insertion_point;
          }
          var offset = tab - insertion_point | 0;
          if (offset >= 0) {
            return break_same_line(state, offset + s._0 | 0);
          } else {
            return break_new_line(state, tab + s._1 | 0, state.pp_margin);
          }
      case /* Pp_begin */3 :
          var ty = s._1;
          var insertion_point$1 = state.pp_margin - state.pp_space_left | 0;
          if (insertion_point$1 > state.pp_max_indent) {
            pp_force_break_line(state);
          }
          var offset$1 = state.pp_space_left - s._0 | 0;
          var bl_type = ty !== 1 ? (
              size > state.pp_space_left ? ty : /* Pp_fits */5
            ) : /* Pp_vbox */1;
          state.pp_format_stack = {
            hd: /* Format_elem */{
              _0: bl_type,
              _1: offset$1
            },
            tl: state.pp_format_stack
          };
          return ;
      case /* Pp_tbegin */4 :
          state.pp_tbox_stack = {
            hd: s._0,
            tl: state.pp_tbox_stack
          };
          return ;
      case /* Pp_open_tag */5 :
          var tag_name = s._0;
          var marker$1 = Curry._1(state.pp_mark_open_tag, tag_name);
          pp_output_string(state, marker$1);
          state.pp_mark_stack = {
            hd: tag_name,
            tl: state.pp_mark_stack
          };
          return ;
      
    }
  }
}

function advance_left(state) {
  try {
    while(true) {
      var match = peek_queue(state.pp_queue);
      var size = match.elem_size;
      if (size < 0 && (state.pp_right_total - state.pp_left_total | 0) < state.pp_space_left) {
        return ;
      }
      take_queue(state.pp_queue);
      format_pp_token(state, size < 0 ? 1000000010 : size, match.token);
      state.pp_left_total = match.length + state.pp_left_total | 0;
      continue ;
    };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Empty_queue) {
      return ;
    }
    throw exn;
  }
}

function enqueue_advance(state, tok) {
  pp_enqueue(state, tok);
  return advance_left(state);
}

function enqueue_string_as(state, size, s) {
  return enqueue_advance(state, {
              elem_size: size,
              token: {
                TAG: /* Pp_text */0,
                _0: s
              },
              length: size
            });
}

var q_elem = {
  elem_size: -1,
  token: {
    TAG: /* Pp_text */0,
    _0: ""
  },
  length: 0
};

var scan_stack_bottom_0 = /* Scan_elem */{
  _0: -1,
  _1: q_elem
};

var scan_stack_bottom = {
  hd: scan_stack_bottom_0,
  tl: /* [] */0
};

function set_size(state, ty) {
  var match = state.pp_scan_stack;
  if (!match) {
    return ;
  }
  var match$1 = match.hd;
  var queue_elem = match$1._1;
  var size = queue_elem.elem_size;
  var t = match.tl;
  if (match$1._0 < state.pp_left_total) {
    state.pp_scan_stack = scan_stack_bottom;
    return ;
  }
  var tmp = queue_elem.token;
  if (typeof tmp === "number") {
    return ;
  }
  switch (tmp.TAG | 0) {
    case /* Pp_break */1 :
    case /* Pp_tbreak */2 :
        break;
    case /* Pp_begin */3 :
        if (!ty) {
          queue_elem.elem_size = state.pp_right_total + size | 0;
          state.pp_scan_stack = t;
          return ;
        } else {
          return ;
        }
    default:
      return ;
  }
  if (ty) {
    queue_elem.elem_size = state.pp_right_total + size | 0;
    state.pp_scan_stack = t;
    return ;
  }
  
}

function scan_push(state, b, tok) {
  pp_enqueue(state, tok);
  if (b) {
    set_size(state, true);
  }
  state.pp_scan_stack = {
    hd: /* Scan_elem */{
      _0: state.pp_right_total,
      _1: tok
    },
    tl: state.pp_scan_stack
  };
  
}

function pp_open_box_gen(state, indent, br_ty) {
  state.pp_curr_depth = state.pp_curr_depth + 1 | 0;
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    if (state.pp_curr_depth === state.pp_max_boxes) {
      var s = state.pp_ellipsis;
      var len = s.length;
      return enqueue_string_as(state, len, s);
    } else {
      return ;
    }
  }
  var elem = {
    elem_size: -state.pp_right_total | 0,
    token: {
      TAG: /* Pp_begin */3,
      _0: indent,
      _1: br_ty
    },
    length: 0
  };
  return scan_push(state, false, elem);
}

function pp_close_box(state, param) {
  if (state.pp_curr_depth > 1) {
    if (state.pp_curr_depth < state.pp_max_boxes) {
      pp_enqueue(state, {
            elem_size: 0,
            token: /* Pp_end */1,
            length: 0
          });
      set_size(state, true);
      set_size(state, false);
    }
    state.pp_curr_depth = state.pp_curr_depth - 1 | 0;
    return ;
  }
  
}

function pp_open_tag(state, tag_name) {
  if (state.pp_print_tags) {
    state.pp_tag_stack = {
      hd: tag_name,
      tl: state.pp_tag_stack
    };
    Curry._1(state.pp_print_open_tag, tag_name);
  }
  if (state.pp_mark_tags) {
    return pp_enqueue(state, {
                elem_size: 0,
                token: {
                  TAG: /* Pp_open_tag */5,
                  _0: tag_name
                },
                length: 0
              });
  }
  
}

function pp_close_tag(state, param) {
  if (state.pp_mark_tags) {
    pp_enqueue(state, {
          elem_size: 0,
          token: /* Pp_close_tag */5,
          length: 0
        });
  }
  if (!state.pp_print_tags) {
    return ;
  }
  var match = state.pp_tag_stack;
  if (match) {
    Curry._1(state.pp_print_close_tag, match.hd);
    state.pp_tag_stack = match.tl;
    return ;
  }
  
}

function pp_set_print_tags(state, b) {
  state.pp_print_tags = b;
  
}

function pp_set_mark_tags(state, b) {
  state.pp_mark_tags = b;
  
}

function pp_get_print_tags(state, param) {
  return state.pp_print_tags;
}

function pp_get_mark_tags(state, param) {
  return state.pp_mark_tags;
}

function pp_set_tags(state, b) {
  state.pp_print_tags = b;
  state.pp_mark_tags = b;
  
}

function pp_get_formatter_tag_functions(state, param) {
  return {
          mark_open_tag: state.pp_mark_open_tag,
          mark_close_tag: state.pp_mark_close_tag,
          print_open_tag: state.pp_print_open_tag,
          print_close_tag: state.pp_print_close_tag
        };
}

function pp_set_formatter_tag_functions(state, param) {
  state.pp_mark_open_tag = param.mark_open_tag;
  state.pp_mark_close_tag = param.mark_close_tag;
  state.pp_print_open_tag = param.print_open_tag;
  state.pp_print_close_tag = param.print_close_tag;
  
}

function pp_rinit(state) {
  pp_clear_queue(state);
  state.pp_scan_stack = scan_stack_bottom;
  state.pp_format_stack = /* [] */0;
  state.pp_tbox_stack = /* [] */0;
  state.pp_tag_stack = /* [] */0;
  state.pp_mark_stack = /* [] */0;
  state.pp_current_indent = 0;
  state.pp_curr_depth = 0;
  state.pp_space_left = state.pp_margin;
  return pp_open_box_gen(state, 0, /* Pp_hovbox */3);
}

function clear_tag_stack(state) {
  return List.iter((function (param) {
                return pp_close_tag(state, undefined);
              }), state.pp_tag_stack);
}

function pp_flush_queue(state, b) {
  clear_tag_stack(state);
  while(state.pp_curr_depth > 1) {
    pp_close_box(state, undefined);
  };
  state.pp_right_total = 1000000010;
  advance_left(state);
  if (b) {
    Curry._1(state.pp_out_newline, undefined);
  }
  return pp_rinit(state);
}

function pp_print_as_size(state, size, s) {
  if (state.pp_curr_depth < state.pp_max_boxes) {
    return enqueue_string_as(state, size, s);
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
  return pp_print_as(state, 1, Caml_string.make(1, c));
}

function pp_open_hbox(state, param) {
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

function pp_print_newline(state, param) {
  pp_flush_queue(state, true);
  return Curry._1(state.pp_out_flush, undefined);
}

function pp_print_flush(state, param) {
  pp_flush_queue(state, false);
  return Curry._1(state.pp_out_flush, undefined);
}

function pp_force_newline(state, param) {
  if (state.pp_curr_depth < state.pp_max_boxes) {
    return enqueue_advance(state, {
                elem_size: 0,
                token: /* Pp_newline */3,
                length: 0
              });
  }
  
}

function pp_print_if_newline(state, param) {
  if (state.pp_curr_depth < state.pp_max_boxes) {
    return enqueue_advance(state, {
                elem_size: 0,
                token: /* Pp_if_newline */4,
                length: 0
              });
  }
  
}

function pp_print_break(state, width, offset) {
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    return ;
  }
  var elem = {
    elem_size: -state.pp_right_total | 0,
    token: {
      TAG: /* Pp_break */1,
      _0: width,
      _1: offset
    },
    length: width
  };
  return scan_push(state, true, elem);
}

function pp_print_space(state, param) {
  return pp_print_break(state, 1, 0);
}

function pp_print_cut(state, param) {
  return pp_print_break(state, 0, 0);
}

function pp_open_tbox(state, param) {
  state.pp_curr_depth = state.pp_curr_depth + 1 | 0;
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    return ;
  }
  var elem = {
    elem_size: 0,
    token: {
      TAG: /* Pp_tbegin */4,
      _0: /* Pp_tbox */{
        _0: {
          contents: /* [] */0
        }
      }
    },
    length: 0
  };
  return enqueue_advance(state, elem);
}

function pp_close_tbox(state, param) {
  if (state.pp_curr_depth <= 1) {
    return ;
  }
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    return ;
  }
  var elem = {
    elem_size: 0,
    token: /* Pp_tend */2,
    length: 0
  };
  enqueue_advance(state, elem);
  state.pp_curr_depth = state.pp_curr_depth - 1 | 0;
  
}

function pp_print_tbreak(state, width, offset) {
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    return ;
  }
  var elem = {
    elem_size: -state.pp_right_total | 0,
    token: {
      TAG: /* Pp_tbreak */2,
      _0: width,
      _1: offset
    },
    length: width
  };
  return scan_push(state, true, elem);
}

function pp_print_tab(state, param) {
  return pp_print_tbreak(state, 0, 0);
}

function pp_set_tab(state, param) {
  if (state.pp_curr_depth >= state.pp_max_boxes) {
    return ;
  }
  var elem = {
    elem_size: 0,
    token: /* Pp_stab */0,
    length: 0
  };
  return enqueue_advance(state, elem);
}

function pp_set_max_boxes(state, n) {
  if (n > 1) {
    state.pp_max_boxes = n;
    return ;
  }
  
}

function pp_get_max_boxes(state, param) {
  return state.pp_max_boxes;
}

function pp_over_max_boxes(state, param) {
  return state.pp_curr_depth === state.pp_max_boxes;
}

function pp_set_ellipsis_text(state, s) {
  state.pp_ellipsis = s;
  
}

function pp_get_ellipsis_text(state, param) {
  return state.pp_ellipsis;
}

function pp_limit(n) {
  if (n < 1000000010) {
    return n;
  } else {
    return 1000000009;
  }
}

function pp_set_max_indent(state, n) {
  var n$1 = state.pp_margin - n | 0;
  if (n$1 < 1) {
    return ;
  }
  var n$2 = pp_limit(n$1);
  state.pp_min_space_left = n$2;
  state.pp_max_indent = state.pp_margin - state.pp_min_space_left | 0;
  return pp_rinit(state);
}

function pp_get_max_indent(state, param) {
  return state.pp_max_indent;
}

function pp_set_margin(state, n) {
  if (n < 1) {
    return ;
  }
  var n$1 = pp_limit(n);
  state.pp_margin = n$1;
  var new_max_indent = state.pp_max_indent <= state.pp_margin ? state.pp_max_indent : Caml.caml_int_max(Caml.caml_int_max(state.pp_margin - state.pp_min_space_left | 0, state.pp_margin / 2 | 0), 1);
  return pp_set_max_indent(state, new_max_indent);
}

function pp_get_margin(state, param) {
  return state.pp_margin;
}

function pp_set_formatter_out_functions(state, param) {
  state.pp_out_string = param.out_string;
  state.pp_out_flush = param.out_flush;
  state.pp_out_newline = param.out_newline;
  state.pp_out_spaces = param.out_spaces;
  state.pp_out_indent = param.out_indent;
  
}

function pp_get_formatter_out_functions(state, param) {
  return {
          out_string: state.pp_out_string,
          out_flush: state.pp_out_flush,
          out_newline: state.pp_out_newline,
          out_spaces: state.pp_out_spaces,
          out_indent: state.pp_out_indent
        };
}

function pp_set_formatter_output_functions(state, f, g) {
  state.pp_out_string = f;
  state.pp_out_flush = g;
  
}

function pp_get_formatter_output_functions(state, param) {
  return [
          state.pp_out_string,
          state.pp_out_flush
        ];
}

function display_newline(state, param) {
  return Curry._3(state.pp_out_string, "\n", 0, 1);
}

var blank_line = " ".repeat(80);

function display_blanks(state, _n) {
  while(true) {
    var n = _n;
    if (n <= 0) {
      return ;
    }
    if (n <= 80) {
      return Curry._3(state.pp_out_string, blank_line, 0, n);
    }
    Curry._3(state.pp_out_string, blank_line, 0, 80);
    _n = n - 80 | 0;
    continue ;
  };
}

function pp_set_formatter_out_channel(state, oc) {
  state.pp_out_string = (function (param, param$1, param$2) {
      return Pervasives.output_substring(oc, param, param$1, param$2);
    });
  state.pp_out_flush = (function (param) {
      return Caml_io.caml_ml_flush(oc);
    });
  state.pp_out_newline = (function (param) {
      return display_newline(state, param);
    });
  state.pp_out_spaces = (function (param) {
      return display_blanks(state, param);
    });
  state.pp_out_indent = (function (param) {
      return display_blanks(state, param);
    });
  
}

function default_pp_mark_open_tag(s) {
  return "<" + (s + ">");
}

function default_pp_mark_close_tag(s) {
  return "</" + (s + ">");
}

function default_pp_print_open_tag(prim) {
  
}

function default_pp_print_close_tag(prim) {
  
}

function pp_make_formatter(f, g, h, i, j) {
  var pp_queue = {
    insert: /* Nil */0,
    body: /* Nil */0
  };
  var sys_tok = {
    elem_size: -1,
    token: {
      TAG: /* Pp_begin */3,
      _0: 0,
      _1: /* Pp_hovbox */3
    },
    length: 0
  };
  add_queue(sys_tok, pp_queue);
  var sys_scan_stack_0 = /* Scan_elem */{
    _0: 1,
    _1: sys_tok
  };
  var sys_scan_stack = {
    hd: sys_scan_stack_0,
    tl: scan_stack_bottom
  };
  return {
          pp_scan_stack: sys_scan_stack,
          pp_format_stack: /* [] */0,
          pp_tbox_stack: /* [] */0,
          pp_tag_stack: /* [] */0,
          pp_mark_stack: /* [] */0,
          pp_margin: 78,
          pp_min_space_left: 10,
          pp_max_indent: 68,
          pp_space_left: 78,
          pp_current_indent: 0,
          pp_is_new_line: true,
          pp_left_total: 1,
          pp_right_total: 1,
          pp_curr_depth: 1,
          pp_max_boxes: Pervasives.max_int,
          pp_ellipsis: ".",
          pp_out_string: f,
          pp_out_flush: g,
          pp_out_newline: h,
          pp_out_spaces: i,
          pp_out_indent: j,
          pp_print_tags: false,
          pp_mark_tags: false,
          pp_mark_open_tag: default_pp_mark_open_tag,
          pp_mark_close_tag: default_pp_mark_close_tag,
          pp_print_open_tag: default_pp_print_open_tag,
          pp_print_close_tag: default_pp_print_close_tag,
          pp_queue: pp_queue
        };
}

function formatter_of_out_functions(out_funs) {
  return pp_make_formatter(out_funs.out_string, out_funs.out_flush, out_funs.out_newline, out_funs.out_spaces, out_funs.out_indent);
}

function make_formatter(output, flush) {
  var ppf = pp_make_formatter(output, flush, (function (prim) {
          
        }), (function (prim) {
          
        }), (function (prim) {
          
        }));
  ppf.pp_out_newline = (function (param) {
      return display_newline(ppf, param);
    });
  ppf.pp_out_spaces = (function (param) {
      return display_blanks(ppf, param);
    });
  ppf.pp_out_indent = (function (param) {
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
                
              }));
}

var stdbuf = $$Buffer.create(512);

var std_formatter = formatter_of_out_channel(Pervasives.stdout);

var err_formatter = formatter_of_out_channel(Pervasives.stderr);

var str_formatter = formatter_of_buffer(stdbuf);

function flush_buffer_formatter(buf, ppf) {
  pp_flush_queue(ppf, false);
  var s = $$Buffer.contents(buf);
  $$Buffer.reset(buf);
  return s;
}

function flush_str_formatter(param) {
  return flush_buffer_formatter(stdbuf, str_formatter);
}

function make_symbolic_output_buffer(param) {
  return {
          symbolic_output_contents: /* [] */0
        };
}

function clear_symbolic_output_buffer(sob) {
  sob.symbolic_output_contents = /* [] */0;
  
}

function get_symbolic_output_buffer(sob) {
  return List.rev(sob.symbolic_output_contents);
}

function flush_symbolic_output_buffer(sob) {
  var items = List.rev(sob.symbolic_output_contents);
  sob.symbolic_output_contents = /* [] */0;
  return items;
}

function add_symbolic_output_item(sob, item) {
  sob.symbolic_output_contents = {
    hd: item,
    tl: sob.symbolic_output_contents
  };
  
}

function formatter_of_symbolic_output_buffer(sob) {
  var f = function (param, param$1, param$2) {
    return add_symbolic_output_item(sob, {
                TAG: /* Output_string */0,
                _0: $$String.sub(param, param$1, param$2)
              });
  };
  var g = function (param) {
    return add_symbolic_output_item(sob, /* Output_flush */0);
  };
  var h = function (param) {
    return add_symbolic_output_item(sob, /* Output_newline */1);
  };
  var i = function (param) {
    return add_symbolic_output_item(sob, {
                TAG: /* Output_spaces */1,
                _0: param
              });
  };
  var j = function (param) {
    return add_symbolic_output_item(sob, {
                TAG: /* Output_indent */2,
                _0: param
              });
  };
  return pp_make_formatter(f, g, h, i, j);
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
  return std_formatter.pp_margin;
}

function set_max_indent(param) {
  return pp_set_max_indent(std_formatter, param);
}

function get_max_indent(param) {
  return std_formatter.pp_max_indent;
}

function set_max_boxes(param) {
  return pp_set_max_boxes(std_formatter, param);
}

function get_max_boxes(param) {
  return std_formatter.pp_max_boxes;
}

function over_max_boxes(param) {
  return pp_over_max_boxes(std_formatter, param);
}

function set_ellipsis_text(param) {
  std_formatter.pp_ellipsis = param;
  
}

function get_ellipsis_text(param) {
  return std_formatter.pp_ellipsis;
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

function set_formatter_tag_functions(param) {
  return pp_set_formatter_tag_functions(std_formatter, param);
}

function get_formatter_tag_functions(param) {
  return pp_get_formatter_tag_functions(std_formatter, param);
}

function set_print_tags(param) {
  std_formatter.pp_print_tags = param;
  
}

function get_print_tags(param) {
  return std_formatter.pp_print_tags;
}

function set_mark_tags(param) {
  std_formatter.pp_mark_tags = param;
  
}

function get_mark_tags(param) {
  return std_formatter.pp_mark_tags;
}

function set_tags(param) {
  return pp_set_tags(std_formatter, param);
}

function pp_print_list(_$staropt$star, pp_v, ppf, _param) {
  while(true) {
    var param = _param;
    var $staropt$star = _$staropt$star;
    var pp_sep = $staropt$star !== undefined ? $staropt$star : pp_print_cut;
    if (!param) {
      return ;
    }
    var vs = param.tl;
    var v = param.hd;
    if (!vs) {
      return Curry._2(pp_v, ppf, v);
    }
    Curry._2(pp_v, ppf, v);
    Curry._2(pp_sep, ppf, undefined);
    _param = vs;
    _$staropt$star = pp_sep;
    continue ;
  };
}

function pp_print_text(ppf, s) {
  var len = s.length;
  var left = {
    contents: 0
  };
  var right = {
    contents: 0
  };
  var flush = function (param) {
    pp_print_string(ppf, $$String.sub(s, left.contents, right.contents - left.contents | 0));
    right.contents = right.contents + 1 | 0;
    left.contents = right.contents;
    
  };
  while(right.contents !== len) {
    var match = Caml_string.get(s, right.contents);
    if (match !== 10) {
      if (match !== 32) {
        right.contents = right.contents + 1 | 0;
      } else {
        flush(undefined);
        pp_print_break(ppf, 1, 0);
      }
    } else {
      flush(undefined);
      pp_force_newline(ppf, undefined);
    }
  };
  if (left.contents !== len) {
    return flush(undefined);
  }
  
}

function compute_tag(output, tag_acc) {
  var buf = $$Buffer.create(16);
  var ppf = formatter_of_buffer(buf);
  Curry._2(output, ppf, tag_acc);
  pp_print_flush(ppf, undefined);
  var len = buf.position;
  if (len < 2) {
    return $$Buffer.contents(buf);
  } else {
    return $$Buffer.sub(buf, 1, len - 2 | 0);
  }
}

function output_formatting_lit(ppf, fmting_lit) {
  if (typeof fmting_lit === "number") {
    switch (fmting_lit) {
      case /* Close_box */0 :
          return pp_close_box(ppf, undefined);
      case /* Close_tag */1 :
          return pp_close_tag(ppf, undefined);
      case /* FFlush */2 :
          return pp_print_flush(ppf, undefined);
      case /* Force_newline */3 :
          return pp_force_newline(ppf, undefined);
      case /* Flush_newline */4 :
          return pp_print_newline(ppf, undefined);
      case /* Escaped_at */5 :
          return pp_print_char(ppf, /* '@' */64);
      case /* Escaped_percent */6 :
          return pp_print_char(ppf, /* '%' */37);
      
    }
  } else {
    switch (fmting_lit.TAG | 0) {
      case /* Break */0 :
          return pp_print_break(ppf, fmting_lit._1, fmting_lit._2);
      case /* Magic_size */1 :
          return ;
      case /* Scan_indic */2 :
          pp_print_char(ppf, /* '@' */64);
          return pp_print_char(ppf, fmting_lit._0);
      
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
  if (typeof acc === "number") {
    return ;
  }
  switch (acc.TAG | 0) {
    case /* Acc_formatting_lit */0 :
        output_acc(ppf, acc._0);
        return output_formatting_lit(ppf, acc._1);
    case /* Acc_formatting_gen */1 :
        var acc$p = acc._1;
        var p$2 = acc._0;
        if (acc$p.TAG === /* Acc_open_tag */0) {
          output_acc(ppf, p$2);
          return pp_open_tag(ppf, compute_tag(output_acc, acc$p._0));
        }
        output_acc(ppf, p$2);
        var match = CamlinternalFormat.open_box_of_string(compute_tag(output_acc, acc$p._0));
        return pp_open_box_gen(ppf, match[0], match[1]);
    case /* Acc_string_literal */2 :
        var p$3 = acc._0;
        var exit$1 = 0;
        if (typeof p$3 === "number" || p$3.TAG !== /* Acc_formatting_lit */0) {
          exit$1 = 3;
        } else {
          var match$1 = p$3._1;
          if (typeof match$1 === "number" || match$1.TAG !== /* Magic_size */1) {
            exit$1 = 3;
          } else {
            p = p$3._0;
            size = match$1._1;
            s = acc._1;
            exit = 1;
          }
        }
        if (exit$1 === 3) {
          output_acc(ppf, p$3);
          return pp_print_string(ppf, acc._1);
        }
        break;
    case /* Acc_char_literal */3 :
        var p$4 = acc._0;
        var exit$2 = 0;
        if (typeof p$4 === "number" || p$4.TAG !== /* Acc_formatting_lit */0) {
          exit$2 = 3;
        } else {
          var match$2 = p$4._1;
          if (typeof match$2 === "number" || match$2.TAG !== /* Magic_size */1) {
            exit$2 = 3;
          } else {
            p$1 = p$4._0;
            size$1 = match$2._1;
            c = acc._1;
            exit = 2;
          }
        }
        if (exit$2 === 3) {
          output_acc(ppf, p$4);
          return pp_print_char(ppf, acc._1);
        }
        break;
    case /* Acc_data_string */4 :
        var p$5 = acc._0;
        var exit$3 = 0;
        if (typeof p$5 === "number" || p$5.TAG !== /* Acc_formatting_lit */0) {
          exit$3 = 3;
        } else {
          var match$3 = p$5._1;
          if (typeof match$3 === "number" || match$3.TAG !== /* Magic_size */1) {
            exit$3 = 3;
          } else {
            p = p$5._0;
            size = match$3._1;
            s = acc._1;
            exit = 1;
          }
        }
        if (exit$3 === 3) {
          output_acc(ppf, p$5);
          return pp_print_string(ppf, acc._1);
        }
        break;
    case /* Acc_data_char */5 :
        var p$6 = acc._0;
        var exit$4 = 0;
        if (typeof p$6 === "number" || p$6.TAG !== /* Acc_formatting_lit */0) {
          exit$4 = 3;
        } else {
          var match$4 = p$6._1;
          if (typeof match$4 === "number" || match$4.TAG !== /* Magic_size */1) {
            exit$4 = 3;
          } else {
            p$1 = p$6._0;
            size$1 = match$4._1;
            c = acc._1;
            exit = 2;
          }
        }
        if (exit$4 === 3) {
          output_acc(ppf, p$6);
          return pp_print_char(ppf, acc._1);
        }
        break;
    case /* Acc_delay */6 :
        output_acc(ppf, acc._0);
        return Curry._1(acc._1, ppf);
    case /* Acc_flush */7 :
        output_acc(ppf, acc._0);
        return pp_print_flush(ppf, undefined);
    case /* Acc_invalid_arg */8 :
        output_acc(ppf, acc._0);
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: acc._1,
              Error: new Error()
            };
    
  }
  switch (exit) {
    case 1 :
        output_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 :
        output_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, Caml_string.make(1, c));
    
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
  if (typeof acc === "number") {
    return ;
  }
  switch (acc.TAG | 0) {
    case /* Acc_formatting_lit */0 :
        strput_acc(ppf, acc._0);
        return output_formatting_lit(ppf, acc._1);
    case /* Acc_formatting_gen */1 :
        var acc$p = acc._1;
        var p$2 = acc._0;
        if (acc$p.TAG === /* Acc_open_tag */0) {
          strput_acc(ppf, p$2);
          return pp_open_tag(ppf, compute_tag(strput_acc, acc$p._0));
        }
        strput_acc(ppf, p$2);
        var match = CamlinternalFormat.open_box_of_string(compute_tag(strput_acc, acc$p._0));
        return pp_open_box_gen(ppf, match[0], match[1]);
    case /* Acc_string_literal */2 :
        var p$3 = acc._0;
        var exit$1 = 0;
        if (typeof p$3 === "number" || p$3.TAG !== /* Acc_formatting_lit */0) {
          exit$1 = 3;
        } else {
          var match$1 = p$3._1;
          if (typeof match$1 === "number" || match$1.TAG !== /* Magic_size */1) {
            exit$1 = 3;
          } else {
            p = p$3._0;
            size = match$1._1;
            s = acc._1;
            exit = 1;
          }
        }
        if (exit$1 === 3) {
          strput_acc(ppf, p$3);
          return pp_print_string(ppf, acc._1);
        }
        break;
    case /* Acc_char_literal */3 :
        var p$4 = acc._0;
        var exit$2 = 0;
        if (typeof p$4 === "number" || p$4.TAG !== /* Acc_formatting_lit */0) {
          exit$2 = 3;
        } else {
          var match$2 = p$4._1;
          if (typeof match$2 === "number" || match$2.TAG !== /* Magic_size */1) {
            exit$2 = 3;
          } else {
            p$1 = p$4._0;
            size$1 = match$2._1;
            c = acc._1;
            exit = 2;
          }
        }
        if (exit$2 === 3) {
          strput_acc(ppf, p$4);
          return pp_print_char(ppf, acc._1);
        }
        break;
    case /* Acc_data_string */4 :
        var p$5 = acc._0;
        var exit$3 = 0;
        if (typeof p$5 === "number" || p$5.TAG !== /* Acc_formatting_lit */0) {
          exit$3 = 3;
        } else {
          var match$3 = p$5._1;
          if (typeof match$3 === "number" || match$3.TAG !== /* Magic_size */1) {
            exit$3 = 3;
          } else {
            p = p$5._0;
            size = match$3._1;
            s = acc._1;
            exit = 1;
          }
        }
        if (exit$3 === 3) {
          strput_acc(ppf, p$5);
          return pp_print_string(ppf, acc._1);
        }
        break;
    case /* Acc_data_char */5 :
        var p$6 = acc._0;
        var exit$4 = 0;
        if (typeof p$6 === "number" || p$6.TAG !== /* Acc_formatting_lit */0) {
          exit$4 = 3;
        } else {
          var match$4 = p$6._1;
          if (typeof match$4 === "number" || match$4.TAG !== /* Magic_size */1) {
            exit$4 = 3;
          } else {
            p$1 = p$6._0;
            size$1 = match$4._1;
            c = acc._1;
            exit = 2;
          }
        }
        if (exit$4 === 3) {
          strput_acc(ppf, p$6);
          return pp_print_char(ppf, acc._1);
        }
        break;
    case /* Acc_delay */6 :
        var p$7 = acc._0;
        var exit$5 = 0;
        if (typeof p$7 === "number" || p$7.TAG !== /* Acc_formatting_lit */0) {
          exit$5 = 3;
        } else {
          var match$5 = p$7._1;
          if (typeof match$5 === "number") {
            exit$5 = 3;
          } else {
            if (match$5.TAG === /* Magic_size */1) {
              strput_acc(ppf, p$7._0);
              return pp_print_as_size(ppf, match$5._1, Curry._1(acc._1, undefined));
            }
            exit$5 = 3;
          }
        }
        if (exit$5 === 3) {
          strput_acc(ppf, p$7);
          return pp_print_string(ppf, Curry._1(acc._1, undefined));
        }
        break;
    case /* Acc_flush */7 :
        strput_acc(ppf, acc._0);
        return pp_print_flush(ppf, undefined);
    case /* Acc_invalid_arg */8 :
        strput_acc(ppf, acc._0);
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: acc._1,
              Error: new Error()
            };
    
  }
  switch (exit) {
    case 1 :
        strput_acc(ppf, p);
        return pp_print_as_size(ppf, size, s);
    case 2 :
        strput_acc(ppf, p$1);
        return pp_print_as_size(ppf, size$1, Caml_string.make(1, c));
    
  }
}

function kfprintf(k, ppf, param) {
  return CamlinternalFormat.make_printf((function (ppf, acc) {
                output_acc(ppf, acc);
                return Curry._1(k, ppf);
              }), ppf, /* End_of_acc */0, param._0);
}

function ikfprintf(k, ppf, param) {
  return CamlinternalFormat.make_iprintf(k, ppf, param._0);
}

function fprintf(ppf, fmt) {
  return kfprintf((function (prim) {
                
              }), ppf, fmt);
}

function ifprintf(ppf, fmt) {
  return ikfprintf((function (prim) {
                
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
  var k$1 = function (param, acc) {
    strput_acc(ppf, acc);
    return Curry._1(k, flush_buffer_formatter(b, ppf));
  };
  return CamlinternalFormat.make_printf(k$1, undefined, /* End_of_acc */0, param._0);
}

function sprintf(fmt) {
  return ksprintf((function (s) {
                return s;
              }), fmt);
}

function kasprintf(k, param) {
  var b = $$Buffer.create(512);
  var ppf = formatter_of_buffer(b);
  var k$1 = function (ppf, acc) {
    output_acc(ppf, acc);
    return Curry._1(k, flush_buffer_formatter(b, ppf));
  };
  return CamlinternalFormat.make_printf(k$1, ppf, /* End_of_acc */0, param._0);
}

function asprintf(fmt) {
  return kasprintf((function (s) {
                return s;
              }), fmt);
}

Pervasives.at_exit(print_flush);

function pp_set_all_formatter_output_functions(state, f, g, h, i) {
  pp_set_formatter_output_functions(state, f, g);
  state.pp_out_newline = h;
  state.pp_out_spaces = i;
  
}

function pp_get_all_formatter_output_functions(state, param) {
  return [
          state.pp_out_string,
          state.pp_out_flush,
          state.pp_out_newline,
          state.pp_out_spaces
        ];
}

function set_all_formatter_output_functions(param, param$1, param$2, param$3) {
  return pp_set_all_formatter_output_functions(std_formatter, param, param$1, param$2, param$3);
}

function get_all_formatter_output_functions(param) {
  return pp_get_all_formatter_output_functions(std_formatter, param);
}

function bprintf(b, param) {
  var k = function (ppf, acc) {
    output_acc(ppf, acc);
    return pp_flush_queue(ppf, false);
  };
  return CamlinternalFormat.make_printf(k, formatter_of_buffer(b), /* End_of_acc */0, param._0);
}

var kprintf = ksprintf;

export {
  pp_open_box ,
  open_box ,
  pp_close_box ,
  close_box ,
  pp_open_hbox ,
  open_hbox ,
  pp_open_vbox ,
  open_vbox ,
  pp_open_hvbox ,
  open_hvbox ,
  pp_open_hovbox ,
  open_hovbox ,
  pp_print_string ,
  print_string ,
  pp_print_as ,
  print_as ,
  pp_print_int ,
  print_int ,
  pp_print_float ,
  print_float ,
  pp_print_char ,
  print_char ,
  pp_print_bool ,
  print_bool ,
  pp_print_space ,
  print_space ,
  pp_print_cut ,
  print_cut ,
  pp_print_break ,
  print_break ,
  pp_force_newline ,
  force_newline ,
  pp_print_if_newline ,
  print_if_newline ,
  pp_print_flush ,
  print_flush ,
  pp_print_newline ,
  print_newline ,
  pp_set_margin ,
  set_margin ,
  pp_get_margin ,
  get_margin ,
  pp_set_max_indent ,
  set_max_indent ,
  pp_get_max_indent ,
  get_max_indent ,
  pp_set_max_boxes ,
  set_max_boxes ,
  pp_get_max_boxes ,
  get_max_boxes ,
  pp_over_max_boxes ,
  over_max_boxes ,
  pp_open_tbox ,
  open_tbox ,
  pp_close_tbox ,
  close_tbox ,
  pp_set_tab ,
  set_tab ,
  pp_print_tab ,
  print_tab ,
  pp_print_tbreak ,
  print_tbreak ,
  pp_set_ellipsis_text ,
  set_ellipsis_text ,
  pp_get_ellipsis_text ,
  get_ellipsis_text ,
  pp_open_tag ,
  open_tag ,
  pp_close_tag ,
  close_tag ,
  pp_set_tags ,
  set_tags ,
  pp_set_print_tags ,
  set_print_tags ,
  pp_set_mark_tags ,
  set_mark_tags ,
  pp_get_print_tags ,
  get_print_tags ,
  pp_get_mark_tags ,
  get_mark_tags ,
  pp_set_formatter_out_channel ,
  set_formatter_out_channel ,
  pp_set_formatter_output_functions ,
  set_formatter_output_functions ,
  pp_get_formatter_output_functions ,
  get_formatter_output_functions ,
  pp_set_formatter_out_functions ,
  set_formatter_out_functions ,
  pp_get_formatter_out_functions ,
  get_formatter_out_functions ,
  pp_set_formatter_tag_functions ,
  set_formatter_tag_functions ,
  pp_get_formatter_tag_functions ,
  get_formatter_tag_functions ,
  formatter_of_out_channel ,
  std_formatter ,
  err_formatter ,
  formatter_of_buffer ,
  stdbuf ,
  str_formatter ,
  flush_str_formatter ,
  make_formatter ,
  formatter_of_out_functions ,
  make_symbolic_output_buffer ,
  clear_symbolic_output_buffer ,
  get_symbolic_output_buffer ,
  flush_symbolic_output_buffer ,
  add_symbolic_output_item ,
  formatter_of_symbolic_output_buffer ,
  pp_print_list ,
  pp_print_text ,
  fprintf ,
  printf ,
  eprintf ,
  sprintf ,
  asprintf ,
  ifprintf ,
  kfprintf ,
  ikfprintf ,
  ksprintf ,
  kasprintf ,
  bprintf ,
  kprintf ,
  set_all_formatter_output_functions ,
  get_all_formatter_output_functions ,
  pp_set_all_formatter_output_functions ,
  pp_get_all_formatter_output_functions ,
  
}
/* stdbuf Not a pure module */
