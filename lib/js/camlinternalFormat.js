'use strict';

var Char = require("./char.js");
var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var $$String = require("./string.js");
var Caml_io = require("./caml_io.js");
var Caml_obj = require("./caml_obj.js");
var Caml_bytes = require("./caml_bytes.js");
var Caml_int32 = require("./caml_int32.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");
var CamlinternalFormatBasics = require("./camlinternalFormatBasics.js");

function create_char_set(param) {
  return Bytes.make(32, /* "\000" */0);
}

function add_in_char_set(char_set, c) {
  var str_ind = (c >>> 3);
  var mask = (1 << (c & 7));
  char_set[str_ind] = Pervasives.char_of_int(Caml_bytes.get(char_set, str_ind) | mask);
  return /* () */0;
}

var freeze_char_set = Bytes.to_string;

function rev_char_set(char_set) {
  var char_set$prime = Bytes.make(32, /* "\000" */0);
  for(var i = 0; i <= 31; ++i){
    char_set$prime[i] = Pervasives.char_of_int(Caml_string.get(char_set, i) ^ 255);
  }
  return Caml_bytes.bytes_to_string(char_set$prime);
}

function is_in_char_set(char_set, c) {
  var str_ind = (c >>> 3);
  var mask = (1 << (c & 7));
  return (Caml_string.get(char_set, str_ind) & mask) !== 0;
}

function pad_of_pad_opt(pad_opt) {
  if (pad_opt !== undefined) {
    return /* constructor */{
            tag: "Lit_padding",
            Arg0: "Right",
            Arg1: pad_opt
          };
  } else {
    return "No_padding";
  }
}

function prec_of_prec_opt(prec_opt) {
  if (prec_opt !== undefined) {
    return /* constructor */{
            tag: "Lit_precision",
            Arg0: prec_opt
          };
  } else {
    return "No_precision";
  }
}

function param_format_of_ignored_format(ign, fmt) {
  if (typeof ign === "string") {
    switch (ign) {
      case "Ignored_char" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Char",
                    Arg0: fmt
                  }
                };
      case "Ignored_caml_char" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Caml_char",
                    Arg0: fmt
                  }
                };
      case "Ignored_bool" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Bool",
                    Arg0: fmt
                  }
                };
      case "Ignored_reader" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Reader",
                    Arg0: fmt
                  }
                };
      case "Ignored_scan_next_char" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Scan_next_char",
                    Arg0: fmt
                  }
                };
      
    }
  } else {
    switch (/* XXX */ign.tag) {
      case "Ignored_string" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "String",
                    Arg0: pad_of_pad_opt(ign.Arg0),
                    Arg1: fmt
                  }
                };
      case "Ignored_caml_string" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Caml_string",
                    Arg0: pad_of_pad_opt(ign.Arg0),
                    Arg1: fmt
                  }
                };
      case "Ignored_int" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Int",
                    Arg0: ign.Arg0,
                    Arg1: pad_of_pad_opt(ign.Arg1),
                    Arg2: "No_precision",
                    Arg3: fmt
                  }
                };
      case "Ignored_int32" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Int32",
                    Arg0: ign.Arg0,
                    Arg1: pad_of_pad_opt(ign.Arg1),
                    Arg2: "No_precision",
                    Arg3: fmt
                  }
                };
      case "Ignored_nativeint" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Nativeint",
                    Arg0: ign.Arg0,
                    Arg1: pad_of_pad_opt(ign.Arg1),
                    Arg2: "No_precision",
                    Arg3: fmt
                  }
                };
      case "Ignored_int64" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Int64",
                    Arg0: ign.Arg0,
                    Arg1: pad_of_pad_opt(ign.Arg1),
                    Arg2: "No_precision",
                    Arg3: fmt
                  }
                };
      case "Ignored_float" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Float",
                    Arg0: "Float_f",
                    Arg1: pad_of_pad_opt(ign.Arg0),
                    Arg2: prec_of_prec_opt(ign.Arg1),
                    Arg3: fmt
                  }
                };
      case "Ignored_format_arg" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Format_arg",
                    Arg0: ign.Arg0,
                    Arg1: ign.Arg1,
                    Arg2: fmt
                  }
                };
      case "Ignored_format_subst" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Format_subst",
                    Arg0: ign.Arg0,
                    Arg1: ign.Arg1,
                    Arg2: fmt
                  }
                };
      case "Ignored_scan_char_set" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Scan_char_set",
                    Arg0: ign.Arg0,
                    Arg1: ign.Arg1,
                    Arg2: fmt
                  }
                };
      case "Ignored_scan_get_counter" :
          return /* constructor */{
                  tag: "Param_format_EBB",
                  Arg0: /* constructor */{
                    tag: "Scan_get_counter",
                    Arg0: ign.Arg0,
                    Arg1: fmt
                  }
                };
      
    }
  }
}

function buffer_check_size(buf, overhead) {
  var len = buf[/* bytes */1].length;
  var min_len = buf[/* ind */0] + overhead | 0;
  if (min_len > len) {
    var new_len = Caml_primitive.caml_int_max((len << 1), min_len);
    var new_str = Caml_bytes.caml_create_bytes(new_len);
    Bytes.blit(buf[/* bytes */1], 0, new_str, 0, len);
    buf[/* bytes */1] = new_str;
    return /* () */0;
  } else {
    return 0;
  }
}

function buffer_add_char(buf, c) {
  buffer_check_size(buf, 1);
  buf[/* bytes */1][buf[/* ind */0]] = c;
  buf[/* ind */0] = buf[/* ind */0] + 1 | 0;
  return /* () */0;
}

function buffer_add_string(buf, s) {
  var str_len = s.length;
  buffer_check_size(buf, str_len);
  $$String.blit(s, 0, buf[/* bytes */1], buf[/* ind */0], str_len);
  buf[/* ind */0] = buf[/* ind */0] + str_len | 0;
  return /* () */0;
}

function buffer_contents(buf) {
  return Bytes.sub_string(buf[/* bytes */1], 0, buf[/* ind */0]);
}

function char_of_iconv(iconv) {
  switch (iconv) {
    case "Int_i" :
    case "Int_pi" :
    case "Int_si" :
        return /* "i" */105;
    case "Int_x" :
    case "Int_Cx" :
        return /* "x" */120;
    case "Int_X" :
    case "Int_CX" :
        return /* "X" */88;
    case "Int_o" :
    case "Int_Co" :
        return /* "o" */111;
    case "Int_u" :
        return /* "u" */117;
    default:
      return /* "d" */100;
  }
}

function char_of_fconv(fconv) {
  switch (fconv) {
    case "Float_f" :
    case "Float_pf" :
    case "Float_sf" :
        return /* "f" */102;
    case "Float_e" :
    case "Float_pe" :
    case "Float_se" :
        return /* "e" */101;
    case "Float_E" :
    case "Float_pE" :
    case "Float_sE" :
        return /* "E" */69;
    case "Float_g" :
    case "Float_pg" :
    case "Float_sg" :
        return /* "g" */103;
    case "Float_F" :
        return /* "F" */70;
    default:
      return /* "G" */71;
  }
}

function char_of_counter(counter) {
  switch (counter) {
    case "Line_counter" :
        return /* "l" */108;
    case "Char_counter" :
        return /* "n" */110;
    case "Token_counter" :
        return /* "N" */78;
    
  }
}

function bprint_char_set(buf, char_set) {
  var print_char = function (buf, i) {
    var c = Pervasives.char_of_int(i);
    if (c !== 37) {
      if (c !== 64) {
        return buffer_add_char(buf, c);
      } else {
        buffer_add_char(buf, /* "%" */37);
        return buffer_add_char(buf, /* "@" */64);
      }
    } else {
      buffer_add_char(buf, /* "%" */37);
      return buffer_add_char(buf, /* "%" */37);
    }
  };
  var print_out = function (set, _i) {
    while(true) {
      var i = _i;
      if (i < 256) {
        if (is_in_char_set(set, Pervasives.char_of_int(i))) {
          var set$1 = set;
          var i$1 = i;
          var match = Pervasives.char_of_int(i$1);
          var switcher = match - 45 | 0;
          if (switcher > 48 || switcher < 0) {
            if (switcher >= 210) {
              return print_char(buf, 255);
            } else {
              return print_second(set$1, i$1 + 1 | 0);
            }
          } else if (switcher > 47 || switcher < 1) {
            return print_out(set$1, i$1 + 1 | 0);
          } else {
            return print_second(set$1, i$1 + 1 | 0);
          }
        } else {
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        return 0;
      }
    };
  };
  var print_second = function (set, i) {
    if (is_in_char_set(set, Pervasives.char_of_int(i))) {
      var match = Pervasives.char_of_int(i);
      var switcher = match - 45 | 0;
      if (switcher > 48 || switcher < 0) {
        if (switcher >= 210) {
          print_char(buf, 254);
          return print_char(buf, 255);
        }
        
      } else if ((switcher > 47 || switcher < 1) && !is_in_char_set(set, Pervasives.char_of_int(i + 1 | 0))) {
        print_char(buf, i - 1 | 0);
        return print_out(set, i + 1 | 0);
      }
      if (is_in_char_set(set, Pervasives.char_of_int(i + 1 | 0))) {
        var set$1 = set;
        var i$1 = i - 1 | 0;
        var _j = i + 2 | 0;
        while(true) {
          var j = _j;
          if (j === 256 || !is_in_char_set(set$1, Pervasives.char_of_int(j))) {
            print_char(buf, i$1);
            print_char(buf, /* "-" */45);
            print_char(buf, j - 1 | 0);
            if (j < 256) {
              return print_out(set$1, j + 1 | 0);
            } else {
              return 0;
            }
          } else {
            _j = j + 1 | 0;
            continue ;
          }
        };
      } else {
        print_char(buf, i - 1 | 0);
        print_char(buf, i);
        return print_out(set, i + 2 | 0);
      }
    } else {
      print_char(buf, i - 1 | 0);
      return print_out(set, i + 1 | 0);
    }
  };
  var print_start = function (set) {
    var is_alone = function (c) {
      var before = Char.chr(c - 1 | 0);
      var after = Char.chr(c + 1 | 0);
      if (is_in_char_set(set, c)) {
        return !(is_in_char_set(set, before) && is_in_char_set(set, after));
      } else {
        return false;
      }
    };
    if (is_alone(/* "]" */93)) {
      buffer_add_char(buf, /* "]" */93);
    }
    print_out(set, 1);
    if (is_alone(/* "-" */45)) {
      return buffer_add_char(buf, /* "-" */45);
    } else {
      return 0;
    }
  };
  buffer_add_char(buf, /* "[" */91);
  print_start(is_in_char_set(char_set, /* "\000" */0) ? (buffer_add_char(buf, /* "^" */94), rev_char_set(char_set)) : char_set);
  return buffer_add_char(buf, /* "]" */93);
}

function bprint_padty(buf, padty) {
  switch (padty) {
    case "Left" :
        return buffer_add_char(buf, /* "-" */45);
    case "Right" :
        return /* () */0;
    case "Zeros" :
        return buffer_add_char(buf, /* "0" */48);
    
  }
}

function bprint_ignored_flag(buf, ign_flag) {
  if (ign_flag) {
    return buffer_add_char(buf, /* "_" */95);
  } else {
    return 0;
  }
}

function bprint_pad_opt(buf, pad_opt) {
  if (pad_opt !== undefined) {
    return buffer_add_string(buf, String(pad_opt));
  } else {
    return /* () */0;
  }
}

function bprint_padding(buf, pad) {
  if (typeof pad === "string") {
    return /* () */0;
  } else {
    bprint_padty(buf, pad.Arg0);
    if (/* XXX */pad.tag === "Lit_padding") {
      return buffer_add_string(buf, String(pad.Arg1));
    } else {
      return buffer_add_char(buf, /* "*" */42);
    }
  }
}

function bprint_precision(buf, prec) {
  if (typeof prec === "string") {
    if (prec === "No_precision") {
      return /* () */0;
    } else {
      return buffer_add_string(buf, ".*");
    }
  } else {
    buffer_add_char(buf, /* "." */46);
    return buffer_add_string(buf, String(prec.Arg0));
  }
}

function bprint_iconv_flag(buf, iconv) {
  switch (iconv) {
    case "Int_pd" :
    case "Int_pi" :
        return buffer_add_char(buf, /* "+" */43);
    case "Int_sd" :
    case "Int_si" :
        return buffer_add_char(buf, /* " " */32);
    case "Int_Cx" :
    case "Int_CX" :
    case "Int_Co" :
        return buffer_add_char(buf, /* "#" */35);
    default:
      return /* () */0;
  }
}

function bprint_int_fmt(buf, ign_flag, iconv, pad, prec) {
  buffer_add_char(buf, /* "%" */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_iconv_flag(buf, iconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  return buffer_add_char(buf, char_of_iconv(iconv));
}

function bprint_altint_fmt(buf, ign_flag, iconv, pad, prec, c) {
  buffer_add_char(buf, /* "%" */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_iconv_flag(buf, iconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  buffer_add_char(buf, c);
  return buffer_add_char(buf, char_of_iconv(iconv));
}

function bprint_fconv_flag(buf, fconv) {
  switch (fconv) {
    case "Float_pf" :
    case "Float_pe" :
    case "Float_pE" :
    case "Float_pg" :
    case "Float_pG" :
        return buffer_add_char(buf, /* "+" */43);
    case "Float_sf" :
    case "Float_se" :
    case "Float_sE" :
    case "Float_sg" :
    case "Float_sG" :
        return buffer_add_char(buf, /* " " */32);
    default:
      return /* () */0;
  }
}

function bprint_float_fmt(buf, ign_flag, fconv, pad, prec) {
  buffer_add_char(buf, /* "%" */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_fconv_flag(buf, fconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  return buffer_add_char(buf, char_of_fconv(fconv));
}

function string_of_formatting_lit(formatting_lit) {
  if (typeof formatting_lit === "string") {
    switch (formatting_lit) {
      case "Close_box" :
          return "@]";
      case "Close_tag" :
          return "@}";
      case "FFlush" :
          return "@?";
      case "Force_newline" :
          return "@\n";
      case "Flush_newline" :
          return "@.";
      case "Escaped_at" :
          return "@@";
      case "Escaped_percent" :
          return "@%";
      
    }
  } else {
    switch (/* XXX */formatting_lit.tag) {
      case "Break" :
      case "Magic_size" :
          return formatting_lit.Arg0;
      case "Scan_indic" :
          return "@" + Caml_bytes.bytes_to_string(Bytes.make(1, formatting_lit.Arg0));
      
    }
  }
}

function string_of_formatting_gen(formatting_gen) {
  return formatting_gen.Arg0.Arg1;
}

function bprint_char_literal(buf, chr) {
  if (chr !== 37) {
    return buffer_add_char(buf, chr);
  } else {
    return buffer_add_string(buf, "%%");
  }
}

function bprint_string_literal(buf, str) {
  for(var i = 0 ,i_finish = str.length - 1 | 0; i <= i_finish; ++i){
    bprint_char_literal(buf, Caml_string.get(str, i));
  }
  return /* () */0;
}

function bprint_fmtty(buf, _fmtty) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "string") {
      return /* () */0;
    } else {
      switch (/* XXX */fmtty.tag) {
        case "Char_ty" :
            buffer_add_string(buf, "%c");
            _fmtty = fmtty.Arg0;
            continue ;
        case "String_ty" :
            buffer_add_string(buf, "%s");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Int_ty" :
            buffer_add_string(buf, "%i");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Int32_ty" :
            buffer_add_string(buf, "%li");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Nativeint_ty" :
            buffer_add_string(buf, "%ni");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Int64_ty" :
            buffer_add_string(buf, "%Li");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Float_ty" :
            buffer_add_string(buf, "%f");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Bool_ty" :
            buffer_add_string(buf, "%B");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Format_arg_ty" :
            buffer_add_string(buf, "%{");
            bprint_fmtty(buf, fmtty.Arg0);
            buffer_add_string(buf, "%}");
            _fmtty = fmtty.Arg1;
            continue ;
        case "Format_subst_ty" :
            buffer_add_string(buf, "%(");
            bprint_fmtty(buf, fmtty.Arg0);
            buffer_add_string(buf, "%)");
            _fmtty = fmtty.Arg2;
            continue ;
        case "Alpha_ty" :
            buffer_add_string(buf, "%a");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Theta_ty" :
            buffer_add_string(buf, "%t");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Any_ty" :
            buffer_add_string(buf, "%?");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Reader_ty" :
            buffer_add_string(buf, "%r");
            _fmtty = fmtty.Arg0;
            continue ;
        case "Ignored_reader_ty" :
            buffer_add_string(buf, "%_r");
            _fmtty = fmtty.Arg0;
            continue ;
        
      }
    }
  };
}

function int_of_custom_arity(param) {
  if (param !== "Custom_zero") {
    return 1 + int_of_custom_arity(param.Arg0) | 0;
  } else {
    return 0;
  }
}

function bprint_fmt(buf, fmt) {
  var _fmt = fmt;
  var _ign_flag = false;
  while(true) {
    var ign_flag = _ign_flag;
    var fmt$1 = _fmt;
    if (typeof fmt$1 === "string") {
      return /* () */0;
    } else {
      switch (/* XXX */fmt$1.tag) {
        case "Char" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "c" */99);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Caml_char" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "C" */67);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "String" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_padding(buf, fmt$1.Arg0);
            buffer_add_char(buf, /* "s" */115);
            _ign_flag = false;
            _fmt = fmt$1.Arg1;
            continue ;
        case "Caml_string" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_padding(buf, fmt$1.Arg0);
            buffer_add_char(buf, /* "S" */83);
            _ign_flag = false;
            _fmt = fmt$1.Arg1;
            continue ;
        case "Int" :
            bprint_int_fmt(buf, ign_flag, fmt$1.Arg0, fmt$1.Arg1, fmt$1.Arg2);
            _ign_flag = false;
            _fmt = fmt$1.Arg3;
            continue ;
        case "Int32" :
            bprint_altint_fmt(buf, ign_flag, fmt$1.Arg0, fmt$1.Arg1, fmt$1.Arg2, /* "l" */108);
            _ign_flag = false;
            _fmt = fmt$1.Arg3;
            continue ;
        case "Nativeint" :
            bprint_altint_fmt(buf, ign_flag, fmt$1.Arg0, fmt$1.Arg1, fmt$1.Arg2, /* "n" */110);
            _ign_flag = false;
            _fmt = fmt$1.Arg3;
            continue ;
        case "Int64" :
            bprint_altint_fmt(buf, ign_flag, fmt$1.Arg0, fmt$1.Arg1, fmt$1.Arg2, /* "L" */76);
            _ign_flag = false;
            _fmt = fmt$1.Arg3;
            continue ;
        case "Float" :
            bprint_float_fmt(buf, ign_flag, fmt$1.Arg0, fmt$1.Arg1, fmt$1.Arg2);
            _ign_flag = false;
            _fmt = fmt$1.Arg3;
            continue ;
        case "Bool" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "B" */66);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Flush" :
            buffer_add_string(buf, "%!");
            _fmt = fmt$1.Arg0;
            continue ;
        case "String_literal" :
            bprint_string_literal(buf, fmt$1.Arg0);
            _fmt = fmt$1.Arg1;
            continue ;
        case "Char_literal" :
            bprint_char_literal(buf, fmt$1.Arg0);
            _fmt = fmt$1.Arg1;
            continue ;
        case "Format_arg" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_pad_opt(buf, fmt$1.Arg0);
            buffer_add_char(buf, /* "{" */123);
            bprint_fmtty(buf, fmt$1.Arg1);
            buffer_add_char(buf, /* "%" */37);
            buffer_add_char(buf, /* "}" */125);
            _ign_flag = false;
            _fmt = fmt$1.Arg2;
            continue ;
        case "Format_subst" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_pad_opt(buf, fmt$1.Arg0);
            buffer_add_char(buf, /* "(" */40);
            bprint_fmtty(buf, fmt$1.Arg1);
            buffer_add_char(buf, /* "%" */37);
            buffer_add_char(buf, /* ")" */41);
            _ign_flag = false;
            _fmt = fmt$1.Arg2;
            continue ;
        case "Alpha" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "a" */97);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Theta" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "t" */116);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Formatting_lit" :
            bprint_string_literal(buf, string_of_formatting_lit(fmt$1.Arg0));
            _fmt = fmt$1.Arg1;
            continue ;
        case "Formatting_gen" :
            bprint_string_literal(buf, "@{");
            bprint_string_literal(buf, string_of_formatting_gen(fmt$1.Arg0));
            _fmt = fmt$1.Arg1;
            continue ;
        case "Reader" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* "r" */114);
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Scan_char_set" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_pad_opt(buf, fmt$1.Arg0);
            bprint_char_set(buf, fmt$1.Arg1);
            _ign_flag = false;
            _fmt = fmt$1.Arg2;
            continue ;
        case "Scan_get_counter" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, char_of_counter(fmt$1.Arg0));
            _ign_flag = false;
            _fmt = fmt$1.Arg1;
            continue ;
        case "Scan_next_char" :
            buffer_add_char(buf, /* "%" */37);
            bprint_ignored_flag(buf, ign_flag);
            bprint_string_literal(buf, "0c");
            _ign_flag = false;
            _fmt = fmt$1.Arg0;
            continue ;
        case "Ignored_param" :
            var match = param_format_of_ignored_format(fmt$1.Arg0, fmt$1.Arg1);
            _ign_flag = true;
            _fmt = match.Arg0;
            continue ;
        case "Custom" :
            for(var _i = 1 ,_i_finish = int_of_custom_arity(fmt$1.Arg0); _i <= _i_finish; ++_i){
              buffer_add_char(buf, /* "%" */37);
              bprint_ignored_flag(buf, ign_flag);
              buffer_add_char(buf, /* "?" */63);
            }
            _ign_flag = false;
            _fmt = fmt$1.Arg2;
            continue ;
        
      }
    }
  };
}

function string_of_fmt(fmt) {
  var buf = /* record */[
    /* ind */0,
    /* bytes */Caml_bytes.caml_create_bytes(16)
  ];
  bprint_fmt(buf, fmt);
  return buffer_contents(buf);
}

function symm(param) {
  if (typeof param === "string") {
    return "End_of_fmtty";
  } else {
    switch (/* XXX */param.tag) {
      case "Char_ty" :
          return /* constructor */{
                  tag: "Char_ty",
                  Arg0: symm(param.Arg0)
                };
      case "String_ty" :
          return /* constructor */{
                  tag: "String_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Int_ty" :
          return /* constructor */{
                  tag: "Int_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Int32_ty" :
          return /* constructor */{
                  tag: "Int32_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Nativeint_ty" :
          return /* constructor */{
                  tag: "Nativeint_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Int64_ty" :
          return /* constructor */{
                  tag: "Int64_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Float_ty" :
          return /* constructor */{
                  tag: "Float_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Bool_ty" :
          return /* constructor */{
                  tag: "Bool_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Format_arg_ty" :
          return /* constructor */{
                  tag: "Format_arg_ty",
                  Arg0: param.Arg0,
                  Arg1: symm(param.Arg1)
                };
      case "Format_subst_ty" :
          return /* constructor */{
                  tag: "Format_subst_ty",
                  Arg0: param.Arg1,
                  Arg1: param.Arg0,
                  Arg2: symm(param.Arg2)
                };
      case "Alpha_ty" :
          return /* constructor */{
                  tag: "Alpha_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Theta_ty" :
          return /* constructor */{
                  tag: "Theta_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Any_ty" :
          return /* constructor */{
                  tag: "Any_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Reader_ty" :
          return /* constructor */{
                  tag: "Reader_ty",
                  Arg0: symm(param.Arg0)
                };
      case "Ignored_reader_ty" :
          return /* constructor */{
                  tag: "Ignored_reader_ty",
                  Arg0: symm(param.Arg0)
                };
      
    }
  }
}

function fmtty_rel_det(param) {
  if (typeof param === "string") {
    return /* tuple */[
            (function (param) {
                return "Refl";
              }),
            (function (param) {
                return "Refl";
              }),
            (function (param) {
                return "Refl";
              }),
            (function (param) {
                return "Refl";
              })
          ];
  } else {
    switch (/* XXX */param.tag) {
      case "Char_ty" :
          var match = fmtty_rel_det(param.Arg0);
          var af = match[1];
          var fa = match[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af, "Refl");
                      return "Refl";
                    }),
                  match[2],
                  match[3]
                ];
      case "String_ty" :
          var match$1 = fmtty_rel_det(param.Arg0);
          var af$1 = match$1[1];
          var fa$1 = match$1[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$1, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$1, "Refl");
                      return "Refl";
                    }),
                  match$1[2],
                  match$1[3]
                ];
      case "Int_ty" :
          var match$2 = fmtty_rel_det(param.Arg0);
          var af$2 = match$2[1];
          var fa$2 = match$2[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$2, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$2, "Refl");
                      return "Refl";
                    }),
                  match$2[2],
                  match$2[3]
                ];
      case "Int32_ty" :
          var match$3 = fmtty_rel_det(param.Arg0);
          var af$3 = match$3[1];
          var fa$3 = match$3[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$3, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$3, "Refl");
                      return "Refl";
                    }),
                  match$3[2],
                  match$3[3]
                ];
      case "Nativeint_ty" :
          var match$4 = fmtty_rel_det(param.Arg0);
          var af$4 = match$4[1];
          var fa$4 = match$4[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$4, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$4, "Refl");
                      return "Refl";
                    }),
                  match$4[2],
                  match$4[3]
                ];
      case "Int64_ty" :
          var match$5 = fmtty_rel_det(param.Arg0);
          var af$5 = match$5[1];
          var fa$5 = match$5[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$5, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$5, "Refl");
                      return "Refl";
                    }),
                  match$5[2],
                  match$5[3]
                ];
      case "Float_ty" :
          var match$6 = fmtty_rel_det(param.Arg0);
          var af$6 = match$6[1];
          var fa$6 = match$6[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$6, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$6, "Refl");
                      return "Refl";
                    }),
                  match$6[2],
                  match$6[3]
                ];
      case "Bool_ty" :
          var match$7 = fmtty_rel_det(param.Arg0);
          var af$7 = match$7[1];
          var fa$7 = match$7[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$7, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$7, "Refl");
                      return "Refl";
                    }),
                  match$7[2],
                  match$7[3]
                ];
      case "Format_arg_ty" :
          var match$8 = fmtty_rel_det(param.Arg1);
          var af$8 = match$8[1];
          var fa$8 = match$8[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$8, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$8, "Refl");
                      return "Refl";
                    }),
                  match$8[2],
                  match$8[3]
                ];
      case "Format_subst_ty" :
          var match$9 = fmtty_rel_det(param.Arg2);
          var de = match$9[3];
          var ed = match$9[2];
          var af$9 = match$9[1];
          var fa$9 = match$9[0];
          var ty = trans(symm(param.Arg0), param.Arg1);
          var match$10 = fmtty_rel_det(ty);
          var jd = match$10[3];
          var dj = match$10[2];
          var ga = match$10[1];
          var ag = match$10[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$9, "Refl");
                      Curry._1(ag, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(ga, "Refl");
                      Curry._1(af$9, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(ed, "Refl");
                      Curry._1(dj, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(jd, "Refl");
                      Curry._1(de, "Refl");
                      return "Refl";
                    })
                ];
      case "Alpha_ty" :
          var match$11 = fmtty_rel_det(param.Arg0);
          var af$10 = match$11[1];
          var fa$10 = match$11[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$10, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$10, "Refl");
                      return "Refl";
                    }),
                  match$11[2],
                  match$11[3]
                ];
      case "Theta_ty" :
          var match$12 = fmtty_rel_det(param.Arg0);
          var af$11 = match$12[1];
          var fa$11 = match$12[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$11, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$11, "Refl");
                      return "Refl";
                    }),
                  match$12[2],
                  match$12[3]
                ];
      case "Any_ty" :
          var match$13 = fmtty_rel_det(param.Arg0);
          var af$12 = match$13[1];
          var fa$12 = match$13[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$12, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$12, "Refl");
                      return "Refl";
                    }),
                  match$13[2],
                  match$13[3]
                ];
      case "Reader_ty" :
          var match$14 = fmtty_rel_det(param.Arg0);
          var de$1 = match$14[3];
          var ed$1 = match$14[2];
          var af$13 = match$14[1];
          var fa$13 = match$14[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$13, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$13, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(ed$1, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(de$1, "Refl");
                      return "Refl";
                    })
                ];
      case "Ignored_reader_ty" :
          var match$15 = fmtty_rel_det(param.Arg0);
          var de$2 = match$15[3];
          var ed$2 = match$15[2];
          var af$14 = match$15[1];
          var fa$14 = match$15[0];
          return /* tuple */[
                  (function (param) {
                      Curry._1(fa$14, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(af$14, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(ed$2, "Refl");
                      return "Refl";
                    }),
                  (function (param) {
                      Curry._1(de$2, "Refl");
                      return "Refl";
                    })
                ];
      
    }
  }
}

function trans(ty1, ty2) {
  var exit = 0;
  if (typeof ty1 === "string") {
    if (typeof ty2 === "string") {
      return "End_of_fmtty";
    } else {
      switch (/* XXX */ty2.tag) {
        case "Format_arg_ty" :
            exit = 6;
            break;
        case "Format_subst_ty" :
            exit = 7;
            break;
        case "Alpha_ty" :
            exit = 1;
            break;
        case "Theta_ty" :
            exit = 2;
            break;
        case "Any_ty" :
            exit = 3;
            break;
        case "Reader_ty" :
            exit = 4;
            break;
        case "Ignored_reader_ty" :
            exit = 5;
            break;
        default:
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "camlinternalFormat.ml",
                  816,
                  23
                ]
              ];
      }
    }
  } else {
    switch (/* XXX */ty1.tag) {
      case "Char_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Char_ty" :
                  return /* constructor */{
                          tag: "Char_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "String_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "String_ty" :
                  return /* constructor */{
                          tag: "String_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Int_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Int_ty" :
                  return /* constructor */{
                          tag: "Int_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Int32_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Int32_ty" :
                  return /* constructor */{
                          tag: "Int32_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Nativeint_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Nativeint_ty" :
                  return /* constructor */{
                          tag: "Nativeint_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Int64_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Int64_ty" :
                  return /* constructor */{
                          tag: "Int64_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Float_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Float_ty" :
                  return /* constructor */{
                          tag: "Float_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Bool_ty" :
          if (typeof ty2 === "string") {
            exit = 8;
          } else {
            switch (/* XXX */ty2.tag) {
              case "Bool_ty" :
                  return /* constructor */{
                          tag: "Bool_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  exit = 7;
                  break;
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case "Format_arg_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    802,
                    26
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Format_arg_ty" :
                  return /* constructor */{
                          tag: "Format_arg_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0),
                          Arg1: trans(ty1.Arg1, ty2.Arg1)
                        };
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        802,
                        26
                      ]
                    ];
            }
          }
          break;
      case "Format_subst_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    812,
                    28
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Format_arg_ty" :
                  exit = 6;
                  break;
              case "Format_subst_ty" :
                  var ty = trans(symm(ty1.Arg1), ty2.Arg0);
                  var match = fmtty_rel_det(ty);
                  Curry._1(match[1], "Refl");
                  Curry._1(match[3], "Refl");
                  return /* constructor */{
                          tag: "Format_subst_ty",
                          Arg0: ty1.Arg0,
                          Arg1: ty2.Arg1,
                          Arg2: trans(ty1.Arg2, ty2.Arg2)
                        };
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  exit = 5;
                  break;
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        812,
                        28
                      ]
                    ];
            }
          }
          break;
      case "Alpha_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    780,
                    21
                  ]
                ];
          } else if (/* XXX */ty2.tag === "Alpha_ty") {
            return /* constructor */{
                    tag: "Alpha_ty",
                    Arg0: trans(ty1.Arg0, ty2.Arg0)
                  };
          } else {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    780,
                    21
                  ]
                ];
          }
      case "Theta_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    784,
                    21
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  return /* constructor */{
                          tag: "Theta_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        784,
                        21
                      ]
                    ];
            }
          }
          break;
      case "Any_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    788,
                    19
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  return /* constructor */{
                          tag: "Any_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        788,
                        19
                      ]
                    ];
            }
          }
          break;
      case "Reader_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    792,
                    22
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  return /* constructor */{
                          tag: "Reader_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        792,
                        22
                      ]
                    ];
            }
          }
          break;
      case "Ignored_reader_ty" :
          if (typeof ty2 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    797,
                    30
                  ]
                ];
          } else {
            switch (/* XXX */ty2.tag) {
              case "Alpha_ty" :
                  exit = 1;
                  break;
              case "Theta_ty" :
                  exit = 2;
                  break;
              case "Any_ty" :
                  exit = 3;
                  break;
              case "Reader_ty" :
                  exit = 4;
                  break;
              case "Ignored_reader_ty" :
                  return /* constructor */{
                          tag: "Ignored_reader_ty",
                          Arg0: trans(ty1.Arg0, ty2.Arg0)
                        };
              default:
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        797,
                        30
                      ]
                    ];
            }
          }
          break;
      
    }
  }
  switch (exit) {
    case 1 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                781,
                21
              ]
            ];
    case 2 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                785,
                21
              ]
            ];
    case 3 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                789,
                19
              ]
            ];
    case 4 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                793,
                22
              ]
            ];
    case 5 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                798,
                30
              ]
            ];
    case 6 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                803,
                26
              ]
            ];
    case 7 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                813,
                28
              ]
            ];
    case 8 :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                817,
                23
              ]
            ];
    
  }
}

function fmtty_of_formatting_gen(formatting_gen) {
  return fmtty_of_fmt(formatting_gen.Arg0.Arg0);
}

function fmtty_of_fmt(_fmtty) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "string") {
      return "End_of_fmtty";
    } else {
      switch (/* XXX */fmtty.tag) {
        case "String" :
        case "Caml_string" :
            break;
        case "Int" :
            var ty_rest = fmtty_of_fmt(fmtty.Arg3);
            var prec_ty = fmtty_of_precision_fmtty(fmtty.Arg2, /* constructor */{
                  tag: "Int_ty",
                  Arg0: ty_rest
                });
            return fmtty_of_padding_fmtty(fmtty.Arg1, prec_ty);
        case "Int32" :
            var ty_rest$1 = fmtty_of_fmt(fmtty.Arg3);
            var prec_ty$1 = fmtty_of_precision_fmtty(fmtty.Arg2, /* constructor */{
                  tag: "Int32_ty",
                  Arg0: ty_rest$1
                });
            return fmtty_of_padding_fmtty(fmtty.Arg1, prec_ty$1);
        case "Nativeint" :
            var ty_rest$2 = fmtty_of_fmt(fmtty.Arg3);
            var prec_ty$2 = fmtty_of_precision_fmtty(fmtty.Arg2, /* constructor */{
                  tag: "Nativeint_ty",
                  Arg0: ty_rest$2
                });
            return fmtty_of_padding_fmtty(fmtty.Arg1, prec_ty$2);
        case "Int64" :
            var ty_rest$3 = fmtty_of_fmt(fmtty.Arg3);
            var prec_ty$3 = fmtty_of_precision_fmtty(fmtty.Arg2, /* constructor */{
                  tag: "Int64_ty",
                  Arg0: ty_rest$3
                });
            return fmtty_of_padding_fmtty(fmtty.Arg1, prec_ty$3);
        case "Float" :
            var ty_rest$4 = fmtty_of_fmt(fmtty.Arg3);
            var prec_ty$4 = fmtty_of_precision_fmtty(fmtty.Arg2, /* constructor */{
                  tag: "Float_ty",
                  Arg0: ty_rest$4
                });
            return fmtty_of_padding_fmtty(fmtty.Arg1, prec_ty$4);
        case "Bool" :
            return /* constructor */{
                    tag: "Bool_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg0)
                  };
        case "Flush" :
            _fmtty = fmtty.Arg0;
            continue ;
        case "Format_arg" :
            return /* constructor */{
                    tag: "Format_arg_ty",
                    Arg0: fmtty.Arg1,
                    Arg1: fmtty_of_fmt(fmtty.Arg2)
                  };
        case "Format_subst" :
            var ty = fmtty.Arg1;
            return /* constructor */{
                    tag: "Format_subst_ty",
                    Arg0: ty,
                    Arg1: ty,
                    Arg2: fmtty_of_fmt(fmtty.Arg2)
                  };
        case "Alpha" :
            return /* constructor */{
                    tag: "Alpha_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg0)
                  };
        case "Theta" :
            return /* constructor */{
                    tag: "Theta_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg0)
                  };
        case "Formatting_gen" :
            return CamlinternalFormatBasics.concat_fmtty(fmtty_of_formatting_gen(fmtty.Arg0), fmtty_of_fmt(fmtty.Arg1));
        case "Reader" :
            return /* constructor */{
                    tag: "Reader_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg0)
                  };
        case "Scan_char_set" :
            return /* constructor */{
                    tag: "String_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg2)
                  };
        case "Scan_get_counter" :
            return /* constructor */{
                    tag: "Int_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg1)
                  };
        case "Char" :
        case "Caml_char" :
        case "Scan_next_char" :
            return /* constructor */{
                    tag: "Char_ty",
                    Arg0: fmtty_of_fmt(fmtty.Arg0)
                  };
        case "Ignored_param" :
            var ign = fmtty.Arg0;
            var fmt = fmtty.Arg1;
            if (typeof ign === "string") {
              if (ign === "Ignored_reader") {
                return /* constructor */{
                        tag: "Ignored_reader_ty",
                        Arg0: fmtty_of_fmt(fmt)
                      };
              } else {
                return fmtty_of_fmt(fmt);
              }
            } else if (/* XXX */ign.tag === "Ignored_format_subst") {
              return CamlinternalFormatBasics.concat_fmtty(ign.Arg1, fmtty_of_fmt(fmt));
            } else {
              return fmtty_of_fmt(fmt);
            }
        case "Custom" :
            return fmtty_of_custom(fmtty.Arg0, fmtty_of_fmt(fmtty.Arg2));
        default:
          _fmtty = fmtty.Arg1;
          continue ;
      }
    }
    return fmtty_of_padding_fmtty(fmtty.Arg0, /* constructor */{
                tag: "String_ty",
                Arg0: fmtty_of_fmt(fmtty.Arg1)
              });
  };
}

function fmtty_of_custom(arity, fmtty) {
  if (arity !== "Custom_zero") {
    return /* constructor */{
            tag: "Any_ty",
            Arg0: fmtty_of_custom(arity.Arg0, fmtty)
          };
  } else {
    return fmtty;
  }
}

function fmtty_of_padding_fmtty(pad, fmtty) {
  if (typeof pad === "string" || /* XXX */pad.tag === "Lit_padding") {
    return fmtty;
  } else {
    return /* constructor */{
            tag: "Int_ty",
            Arg0: fmtty
          };
  }
}

function fmtty_of_precision_fmtty(prec, fmtty) {
  if (typeof prec === "string" && prec !== "No_precision") {
    return /* constructor */{
            tag: "Int_ty",
            Arg0: fmtty
          };
  } else {
    return fmtty;
  }
}

var Type_mismatch = Caml_exceptions.create("CamlinternalFormat.Type_mismatch");

function type_padding(pad, fmtty) {
  if (typeof pad === "string") {
    return /* constructor */{
            tag: "Padding_fmtty_EBB",
            Arg0: "No_padding",
            Arg1: fmtty
          };
  } else if (/* XXX */pad.tag === "Lit_padding") {
    return /* constructor */{
            tag: "Padding_fmtty_EBB",
            Arg0: /* constructor */{
              tag: "Lit_padding",
              Arg0: pad.Arg0,
              Arg1: pad.Arg1
            },
            Arg1: fmtty
          };
  } else if (typeof fmtty === "string") {
    throw Type_mismatch;
  } else if (/* XXX */fmtty.tag === "Int_ty") {
    return /* constructor */{
            tag: "Padding_fmtty_EBB",
            Arg0: /* constructor */{
              tag: "Arg_padding",
              Arg0: pad.Arg0
            },
            Arg1: fmtty.Arg0
          };
  } else {
    throw Type_mismatch;
  }
}

function type_padprec(pad, prec, fmtty) {
  var match = type_padding(pad, fmtty);
  if (typeof prec === "string") {
    if (prec === "No_precision") {
      return /* constructor */{
              tag: "Padprec_fmtty_EBB",
              Arg0: match.Arg0,
              Arg1: "No_precision",
              Arg2: match.Arg1
            };
    } else {
      var match$1 = match.Arg1;
      if (typeof match$1 === "string") {
        throw Type_mismatch;
      } else if (/* XXX */match$1.tag === "Int_ty") {
        return /* constructor */{
                tag: "Padprec_fmtty_EBB",
                Arg0: match.Arg0,
                Arg1: "Arg_precision",
                Arg2: match$1.Arg0
              };
      } else {
        throw Type_mismatch;
      }
    }
  } else {
    return /* constructor */{
            tag: "Padprec_fmtty_EBB",
            Arg0: match.Arg0,
            Arg1: /* constructor */{
              tag: "Lit_precision",
              Arg0: prec.Arg0
            },
            Arg2: match.Arg1
          };
  }
}

function type_ignored_param_one(ign, fmt, fmtty) {
  var match = type_format_gen(fmt, fmtty);
  return /* constructor */{
          tag: "Fmt_fmtty_EBB",
          Arg0: /* constructor */{
            tag: "Ignored_param",
            Arg0: ign,
            Arg1: match.Arg0
          },
          Arg1: match.Arg1
        };
}

function type_format_gen(fmt, fmtty) {
  if (typeof fmt === "string") {
    return /* constructor */{
            tag: "Fmt_fmtty_EBB",
            Arg0: "End_of_format",
            Arg1: fmtty
          };
  } else {
    switch (/* XXX */fmt.tag) {
      case "Char" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Char_ty") {
            var match = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Char",
                      Arg0: match.Arg0
                    },
                    Arg1: match.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Caml_char" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Char_ty") {
            var match$1 = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Caml_char",
                      Arg0: match$1.Arg0
                    },
                    Arg1: match$1.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "String" :
          var match$2 = type_padding(fmt.Arg0, fmtty);
          var match$3 = match$2.Arg1;
          if (typeof match$3 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$3.tag === "String_ty") {
            var match$4 = type_format_gen(fmt.Arg1, match$3.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: match$2.Arg0,
                      Arg1: match$4.Arg0
                    },
                    Arg1: match$4.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Caml_string" :
          var match$5 = type_padding(fmt.Arg0, fmtty);
          var match$6 = match$5.Arg1;
          if (typeof match$6 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$6.tag === "String_ty") {
            var match$7 = type_format_gen(fmt.Arg1, match$6.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Caml_string",
                      Arg0: match$5.Arg0,
                      Arg1: match$7.Arg0
                    },
                    Arg1: match$7.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int" :
          var match$8 = type_padprec(fmt.Arg1, fmt.Arg2, fmtty);
          var match$9 = match$8.Arg2;
          if (typeof match$9 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$9.tag === "Int_ty") {
            var match$10 = type_format_gen(fmt.Arg3, match$9.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Int",
                      Arg0: fmt.Arg0,
                      Arg1: match$8.Arg0,
                      Arg2: match$8.Arg1,
                      Arg3: match$10.Arg0
                    },
                    Arg1: match$10.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int32" :
          var match$11 = type_padprec(fmt.Arg1, fmt.Arg2, fmtty);
          var match$12 = match$11.Arg2;
          if (typeof match$12 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$12.tag === "Int32_ty") {
            var match$13 = type_format_gen(fmt.Arg3, match$12.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Int32",
                      Arg0: fmt.Arg0,
                      Arg1: match$11.Arg0,
                      Arg2: match$11.Arg1,
                      Arg3: match$13.Arg0
                    },
                    Arg1: match$13.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Nativeint" :
          var match$14 = type_padprec(fmt.Arg1, fmt.Arg2, fmtty);
          var match$15 = match$14.Arg2;
          if (typeof match$15 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$15.tag === "Nativeint_ty") {
            var match$16 = type_format_gen(fmt.Arg3, match$15.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Nativeint",
                      Arg0: fmt.Arg0,
                      Arg1: match$14.Arg0,
                      Arg2: match$14.Arg1,
                      Arg3: match$16.Arg0
                    },
                    Arg1: match$16.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int64" :
          var match$17 = type_padprec(fmt.Arg1, fmt.Arg2, fmtty);
          var match$18 = match$17.Arg2;
          if (typeof match$18 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$18.tag === "Int64_ty") {
            var match$19 = type_format_gen(fmt.Arg3, match$18.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Int64",
                      Arg0: fmt.Arg0,
                      Arg1: match$17.Arg0,
                      Arg2: match$17.Arg1,
                      Arg3: match$19.Arg0
                    },
                    Arg1: match$19.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Float" :
          var match$20 = type_padprec(fmt.Arg1, fmt.Arg2, fmtty);
          var match$21 = match$20.Arg2;
          if (typeof match$21 === "string") {
            throw Type_mismatch;
          } else if (/* XXX */match$21.tag === "Float_ty") {
            var match$22 = type_format_gen(fmt.Arg3, match$21.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Float",
                      Arg0: fmt.Arg0,
                      Arg1: match$20.Arg0,
                      Arg2: match$20.Arg1,
                      Arg3: match$22.Arg0
                    },
                    Arg1: match$22.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Bool" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Bool_ty") {
            var match$23 = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Bool",
                      Arg0: match$23.Arg0
                    },
                    Arg1: match$23.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Flush" :
          var match$24 = type_format_gen(fmt.Arg0, fmtty);
          return /* constructor */{
                  tag: "Fmt_fmtty_EBB",
                  Arg0: /* constructor */{
                    tag: "Flush",
                    Arg0: match$24.Arg0
                  },
                  Arg1: match$24.Arg1
                };
      case "String_literal" :
          var match$25 = type_format_gen(fmt.Arg1, fmtty);
          return /* constructor */{
                  tag: "Fmt_fmtty_EBB",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: fmt.Arg0,
                    Arg1: match$25.Arg0
                  },
                  Arg1: match$25.Arg1
                };
      case "Char_literal" :
          var match$26 = type_format_gen(fmt.Arg1, fmtty);
          return /* constructor */{
                  tag: "Fmt_fmtty_EBB",
                  Arg0: /* constructor */{
                    tag: "Char_literal",
                    Arg0: fmt.Arg0,
                    Arg1: match$26.Arg0
                  },
                  Arg1: match$26.Arg1
                };
      case "Format_arg" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Format_arg_ty") {
            var sub_fmtty$prime = fmtty.Arg0;
            if (Caml_obj.caml_notequal(/* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: fmt.Arg1
                  }, /* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: sub_fmtty$prime
                  })) {
              throw Type_mismatch;
            }
            var match$27 = type_format_gen(fmt.Arg2, fmtty.Arg1);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Format_arg",
                      Arg0: fmt.Arg0,
                      Arg1: sub_fmtty$prime,
                      Arg2: match$27.Arg0
                    },
                    Arg1: match$27.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Format_subst" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Format_subst_ty") {
            var sub_fmtty1 = fmtty.Arg0;
            if (Caml_obj.caml_notequal(/* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(fmt.Arg1)
                  }, /* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(sub_fmtty1)
                  })) {
              throw Type_mismatch;
            }
            var match$28 = type_format_gen(fmt.Arg2, CamlinternalFormatBasics.erase_rel(fmtty.Arg2));
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Format_subst",
                      Arg0: fmt.Arg0,
                      Arg1: sub_fmtty1,
                      Arg2: match$28.Arg0
                    },
                    Arg1: match$28.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Alpha" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Alpha_ty") {
            var match$29 = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Alpha",
                      Arg0: match$29.Arg0
                    },
                    Arg1: match$29.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Theta" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Theta_ty") {
            var match$30 = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Theta",
                      Arg0: match$30.Arg0
                    },
                    Arg1: match$30.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Formatting_lit" :
          var match$31 = type_format_gen(fmt.Arg1, fmtty);
          return /* constructor */{
                  tag: "Fmt_fmtty_EBB",
                  Arg0: /* constructor */{
                    tag: "Formatting_lit",
                    Arg0: fmt.Arg0,
                    Arg1: match$31.Arg0
                  },
                  Arg1: match$31.Arg1
                };
      case "Formatting_gen" :
          var formatting_gen = fmt.Arg0;
          var fmt0 = fmt.Arg1;
          var fmtty0 = fmtty;
          if (/* XXX */formatting_gen.tag === "Open_tag") {
            var match$32 = formatting_gen.Arg0;
            var match$33 = type_format_gen(match$32.Arg0, fmtty0);
            var match$34 = type_format_gen(fmt0, match$33.Arg1);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Formatting_gen",
                      Arg0: /* constructor */{
                        tag: "Open_tag",
                        Arg0: /* constructor */{
                          tag: "Format",
                          Arg0: match$33.Arg0,
                          Arg1: match$32.Arg1
                        }
                      },
                      Arg1: match$34.Arg0
                    },
                    Arg1: match$34.Arg1
                  };
          } else {
            var match$35 = formatting_gen.Arg0;
            var match$36 = type_format_gen(match$35.Arg0, fmtty0);
            var match$37 = type_format_gen(fmt0, match$36.Arg1);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Formatting_gen",
                      Arg0: /* constructor */{
                        tag: "Open_box",
                        Arg0: /* constructor */{
                          tag: "Format",
                          Arg0: match$36.Arg0,
                          Arg1: match$35.Arg1
                        }
                      },
                      Arg1: match$37.Arg0
                    },
                    Arg1: match$37.Arg1
                  };
          }
      case "Reader" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Reader_ty") {
            var match$38 = type_format_gen(fmt.Arg0, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Reader",
                      Arg0: match$38.Arg0
                    },
                    Arg1: match$38.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Scan_char_set" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "String_ty") {
            var match$39 = type_format_gen(fmt.Arg2, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Scan_char_set",
                      Arg0: fmt.Arg0,
                      Arg1: fmt.Arg1,
                      Arg2: match$39.Arg0
                    },
                    Arg1: match$39.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Scan_get_counter" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Int_ty") {
            var match$40 = type_format_gen(fmt.Arg1, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmt_fmtty_EBB",
                    Arg0: /* constructor */{
                      tag: "Scan_get_counter",
                      Arg0: fmt.Arg0,
                      Arg1: match$40.Arg0
                    },
                    Arg1: match$40.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Ignored_param" :
          var ign = fmt.Arg0;
          var fmt$1 = fmt.Arg1;
          var fmtty$1 = fmtty;
          if (typeof ign === "string") {
            if (ign === "Ignored_reader") {
              if (typeof fmtty$1 === "string") {
                throw Type_mismatch;
              } else if (/* XXX */fmtty$1.tag === "Ignored_reader_ty") {
                var match$41 = type_format_gen(fmt$1, fmtty$1.Arg0);
                return /* constructor */{
                        tag: "Fmt_fmtty_EBB",
                        Arg0: /* constructor */{
                          tag: "Ignored_param",
                          Arg0: "Ignored_reader",
                          Arg1: match$41.Arg0
                        },
                        Arg1: match$41.Arg1
                      };
              } else {
                throw Type_mismatch;
              }
            } else {
              return type_ignored_param_one(ign, fmt$1, fmtty$1);
            }
          } else {
            switch (/* XXX */ign.tag) {
              case "Ignored_format_arg" :
                  return type_ignored_param_one(/* constructor */{
                              tag: "Ignored_format_arg",
                              Arg0: ign.Arg0,
                              Arg1: ign.Arg1
                            }, fmt$1, fmtty$1);
              case "Ignored_format_subst" :
                  var match$42 = type_ignored_format_substitution(ign.Arg1, fmt$1, fmtty$1);
                  var match$43 = match$42.Arg1;
                  return /* constructor */{
                          tag: "Fmt_fmtty_EBB",
                          Arg0: /* constructor */{
                            tag: "Ignored_param",
                            Arg0: /* constructor */{
                              tag: "Ignored_format_subst",
                              Arg0: ign.Arg0,
                              Arg1: match$42.Arg0
                            },
                            Arg1: match$43.Arg0
                          },
                          Arg1: match$43.Arg1
                        };
              default:
                return type_ignored_param_one(ign, fmt$1, fmtty$1);
            }
          }
      case "Scan_next_char" :
      case "Custom" :
          throw Type_mismatch;
      
    }
  }
}

function type_ignored_format_substitution(sub_fmtty, fmt, fmtty) {
  if (typeof sub_fmtty === "string") {
    return /* constructor */{
            tag: "Fmtty_fmt_EBB",
            Arg0: "End_of_fmtty",
            Arg1: type_format_gen(fmt, fmtty)
          };
  } else {
    switch (/* XXX */sub_fmtty.tag) {
      case "Char_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Char_ty") {
            var match = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Char_ty",
                      Arg0: match.Arg0
                    },
                    Arg1: match.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "String_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "String_ty") {
            var match$1 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "String_ty",
                      Arg0: match$1.Arg0
                    },
                    Arg1: match$1.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Int_ty") {
            var match$2 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Int_ty",
                      Arg0: match$2.Arg0
                    },
                    Arg1: match$2.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int32_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Int32_ty") {
            var match$3 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Int32_ty",
                      Arg0: match$3.Arg0
                    },
                    Arg1: match$3.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Nativeint_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Nativeint_ty") {
            var match$4 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Nativeint_ty",
                      Arg0: match$4.Arg0
                    },
                    Arg1: match$4.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Int64_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Int64_ty") {
            var match$5 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Int64_ty",
                      Arg0: match$5.Arg0
                    },
                    Arg1: match$5.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Float_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Float_ty") {
            var match$6 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Float_ty",
                      Arg0: match$6.Arg0
                    },
                    Arg1: match$6.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Bool_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Bool_ty") {
            var match$7 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Bool_ty",
                      Arg0: match$7.Arg0
                    },
                    Arg1: match$7.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Format_arg_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Format_arg_ty") {
            var sub2_fmtty$prime = fmtty.Arg0;
            if (Caml_obj.caml_notequal(/* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: sub_fmtty.Arg0
                  }, /* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: sub2_fmtty$prime
                  })) {
              throw Type_mismatch;
            }
            var match$8 = type_ignored_format_substitution(sub_fmtty.Arg1, fmt, fmtty.Arg1);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Format_arg_ty",
                      Arg0: sub2_fmtty$prime,
                      Arg1: match$8.Arg0
                    },
                    Arg1: match$8.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Format_subst_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Format_subst_ty") {
            var sub2_fmtty$prime$1 = fmtty.Arg1;
            var sub1_fmtty$prime = fmtty.Arg0;
            if (Caml_obj.caml_notequal(/* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(sub_fmtty.Arg0)
                  }, /* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(sub1_fmtty$prime)
                  })) {
              throw Type_mismatch;
            }
            if (Caml_obj.caml_notequal(/* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(sub_fmtty.Arg1)
                  }, /* constructor */{
                    tag: "Fmtty_EBB",
                    Arg0: CamlinternalFormatBasics.erase_rel(sub2_fmtty$prime$1)
                  })) {
              throw Type_mismatch;
            }
            var sub_fmtty$prime = trans(symm(sub1_fmtty$prime), sub2_fmtty$prime$1);
            var match$9 = fmtty_rel_det(sub_fmtty$prime);
            Curry._1(match$9[1], "Refl");
            Curry._1(match$9[3], "Refl");
            var match$10 = type_ignored_format_substitution(CamlinternalFormatBasics.erase_rel(sub_fmtty.Arg2), fmt, fmtty.Arg2);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Format_subst_ty",
                      Arg0: sub1_fmtty$prime,
                      Arg1: sub2_fmtty$prime$1,
                      Arg2: symm(match$10.Arg0)
                    },
                    Arg1: match$10.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Alpha_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Alpha_ty") {
            var match$11 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Alpha_ty",
                      Arg0: match$11.Arg0
                    },
                    Arg1: match$11.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Theta_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Theta_ty") {
            var match$12 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Theta_ty",
                      Arg0: match$12.Arg0
                    },
                    Arg1: match$12.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Any_ty" :
          throw Type_mismatch;
      case "Reader_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Reader_ty") {
            var match$13 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Reader_ty",
                      Arg0: match$13.Arg0
                    },
                    Arg1: match$13.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      case "Ignored_reader_ty" :
          if (typeof fmtty === "string") {
            throw Type_mismatch;
          } else if (/* XXX */fmtty.tag === "Ignored_reader_ty") {
            var match$14 = type_ignored_format_substitution(sub_fmtty.Arg0, fmt, fmtty.Arg0);
            return /* constructor */{
                    tag: "Fmtty_fmt_EBB",
                    Arg0: /* constructor */{
                      tag: "Ignored_reader_ty",
                      Arg0: match$14.Arg0
                    },
                    Arg1: match$14.Arg1
                  };
          } else {
            throw Type_mismatch;
          }
      
    }
  }
}

function type_format(fmt, fmtty) {
  var match = type_format_gen(fmt, fmtty);
  var tmp = match.Arg1;
  if (typeof tmp === "string") {
    return match.Arg0;
  } else {
    throw Type_mismatch;
  }
}

function recast(fmt, fmtty) {
  return type_format(fmt, CamlinternalFormatBasics.erase_rel(symm(fmtty)));
}

function fix_padding(padty, width, str) {
  var len = str.length;
  var width$1 = Pervasives.abs(width);
  var padty$1 = width < 0 ? "Left" : padty;
  if (width$1 <= len) {
    return str;
  } else {
    var res = Bytes.make(width$1, padty$1 === "Zeros" ? /* "0" */48 : /* " " */32);
    switch (padty$1) {
      case "Left" :
          $$String.blit(str, 0, res, 0, len);
          break;
      case "Right" :
          $$String.blit(str, 0, res, width$1 - len | 0, len);
          break;
      case "Zeros" :
          if (len > 0 && (Caml_string.get(str, 0) === /* "+" */43 || Caml_string.get(str, 0) === /* "-" */45 || Caml_string.get(str, 0) === /* " " */32)) {
            res[0] = Caml_string.get(str, 0);
            $$String.blit(str, 1, res, (width$1 - len | 0) + 1 | 0, len - 1 | 0);
          } else if (len > 1 && Caml_string.get(str, 0) === /* "0" */48 && (Caml_string.get(str, 1) === /* "x" */120 || Caml_string.get(str, 1) === /* "X" */88)) {
            res[1] = Caml_string.get(str, 1);
            $$String.blit(str, 2, res, (width$1 - len | 0) + 2 | 0, len - 2 | 0);
          } else {
            $$String.blit(str, 0, res, width$1 - len | 0, len);
          }
          break;
      
    }
    return Caml_bytes.bytes_to_string(res);
  }
}

function fix_int_precision(prec, str) {
  var prec$1 = Pervasives.abs(prec);
  var len = str.length;
  var c = Caml_string.get(str, 0);
  var exit = 0;
  if (c >= 58) {
    if (c >= 71) {
      if (c > 102 || c < 97) {
        return str;
      } else {
        exit = 2;
      }
    } else if (c >= 65) {
      exit = 2;
    } else {
      return str;
    }
  } else if (c !== 32) {
    if (c >= 43) {
      switch (c - 43 | 0) {
        case 0 :
        case 2 :
            exit = 1;
            break;
        case 1 :
        case 3 :
        case 4 :
            return str;
        case 5 :
            if ((prec$1 + 2 | 0) > len && len > 1 && (Caml_string.get(str, 1) === /* "x" */120 || Caml_string.get(str, 1) === /* "X" */88)) {
              var res = Bytes.make(prec$1 + 2 | 0, /* "0" */48);
              res[1] = Caml_string.get(str, 1);
              $$String.blit(str, 2, res, (prec$1 - len | 0) + 4 | 0, len - 2 | 0);
              return Caml_bytes.bytes_to_string(res);
            } else {
              exit = 2;
            }
            break;
        case 6 :
        case 7 :
        case 8 :
        case 9 :
        case 10 :
        case 11 :
        case 12 :
        case 13 :
        case 14 :
            exit = 2;
            break;
        
      }
    } else {
      return str;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        if ((prec$1 + 1 | 0) > len) {
          var res$1 = Bytes.make(prec$1 + 1 | 0, /* "0" */48);
          res$1[0] = c;
          $$String.blit(str, 1, res$1, (prec$1 - len | 0) + 2 | 0, len - 1 | 0);
          return Caml_bytes.bytes_to_string(res$1);
        } else {
          return str;
        }
    case 2 :
        if (prec$1 > len) {
          var res$2 = Bytes.make(prec$1, /* "0" */48);
          $$String.blit(str, 0, res$2, prec$1 - len | 0, len);
          return Caml_bytes.bytes_to_string(res$2);
        } else {
          return str;
        }
    
  }
}

function string_to_caml_string(str) {
  return $$String.concat($$String.escaped(str), /* constructor */{
              tag: "::",
              Arg0: "\"",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "\"",
                Arg1: "[]"
              }
            });
}

function format_of_iconv(iconv) {
  switch (iconv) {
    case "Int_d" :
        return "%d";
    case "Int_pd" :
        return "%+d";
    case "Int_sd" :
        return "% d";
    case "Int_i" :
        return "%i";
    case "Int_pi" :
        return "%+i";
    case "Int_si" :
        return "% i";
    case "Int_x" :
        return "%x";
    case "Int_Cx" :
        return "%#x";
    case "Int_X" :
        return "%X";
    case "Int_CX" :
        return "%#X";
    case "Int_o" :
        return "%o";
    case "Int_Co" :
        return "%#o";
    case "Int_u" :
        return "%u";
    
  }
}

function format_of_aconv(iconv, c) {
  var seps;
  switch (iconv) {
    case "Int_d" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "d",
            Arg1: "[]"
          }
        };
        break;
    case "Int_pd" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%+",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "d",
            Arg1: "[]"
          }
        };
        break;
    case "Int_sd" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "% ",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "d",
            Arg1: "[]"
          }
        };
        break;
    case "Int_i" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "i",
            Arg1: "[]"
          }
        };
        break;
    case "Int_pi" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%+",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "i",
            Arg1: "[]"
          }
        };
        break;
    case "Int_si" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "% ",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "i",
            Arg1: "[]"
          }
        };
        break;
    case "Int_x" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "x",
            Arg1: "[]"
          }
        };
        break;
    case "Int_Cx" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%#",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "x",
            Arg1: "[]"
          }
        };
        break;
    case "Int_X" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "X",
            Arg1: "[]"
          }
        };
        break;
    case "Int_CX" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%#",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "X",
            Arg1: "[]"
          }
        };
        break;
    case "Int_o" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "o",
            Arg1: "[]"
          }
        };
        break;
    case "Int_Co" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%#",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "o",
            Arg1: "[]"
          }
        };
        break;
    case "Int_u" :
        seps = /* constructor */{
          tag: "::",
          Arg0: "%",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "u",
            Arg1: "[]"
          }
        };
        break;
    
  }
  return $$String.concat(Caml_bytes.bytes_to_string(Bytes.make(1, c)), seps);
}

function format_of_fconv(fconv, prec) {
  if (fconv === "Float_F") {
    return "%.12g";
  } else {
    var prec$1 = Pervasives.abs(prec);
    var symb = char_of_fconv(fconv);
    var buf = /* record */[
      /* ind */0,
      /* bytes */Caml_bytes.caml_create_bytes(16)
    ];
    buffer_add_char(buf, /* "%" */37);
    bprint_fconv_flag(buf, fconv);
    buffer_add_char(buf, /* "." */46);
    buffer_add_string(buf, String(prec$1));
    buffer_add_char(buf, symb);
    return buffer_contents(buf);
  }
}

function convert_int(iconv, n) {
  return Caml_format.caml_format_int(format_of_iconv(iconv), n);
}

function convert_int32(iconv, n) {
  return Caml_format.caml_int32_format(format_of_aconv(iconv, /* "l" */108), n);
}

function convert_nativeint(iconv, n) {
  return Caml_format.caml_nativeint_format(format_of_aconv(iconv, /* "n" */110), n);
}

function convert_int64(iconv, n) {
  return Caml_format.caml_int64_format(format_of_aconv(iconv, /* "L" */76), n);
}

function convert_float(fconv, prec, x) {
  var prec$1 = Pervasives.abs(prec);
  var str = Caml_format.caml_format_float(format_of_fconv(fconv, prec$1), x);
  if (fconv !== "Float_F") {
    return str;
  } else {
    var len = str.length;
    var is_valid = function (_i) {
      while(true) {
        var i = _i;
        if (i === len) {
          return false;
        } else {
          var match = Caml_string.get(str, i);
          var switcher = match - 46 | 0;
          if (switcher > 23 || switcher < 0) {
            if (switcher !== 55) {
              _i = i + 1 | 0;
              continue ;
            } else {
              return true;
            }
          } else if (switcher > 22 || switcher < 1) {
            return true;
          } else {
            _i = i + 1 | 0;
            continue ;
          }
        }
      };
    };
    var match = Pervasives.classify_float(x);
    switch (match) {
      case "FP_infinite" :
          if (x < 0.0) {
            return "neg_infinity";
          } else {
            return "infinity";
          }
      case "FP_nan" :
          return "nan";
      default:
        if (is_valid(0)) {
          return str;
        } else {
          return str + ".";
        }
    }
  }
}

function format_caml_char(c) {
  return $$String.concat(Char.escaped(c), /* constructor */{
              tag: "::",
              Arg0: "'",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "'",
                Arg1: "[]"
              }
            });
}

function string_of_fmtty(fmtty) {
  var buf = /* record */[
    /* ind */0,
    /* bytes */Caml_bytes.caml_create_bytes(16)
  ];
  bprint_fmtty(buf, fmtty);
  return buffer_contents(buf);
}

function make_printf(_k, o, _acc, _fmt) {
  while(true) {
    var fmt = _fmt;
    var acc = _acc;
    var k = _k;
    if (typeof fmt === "string") {
      return Curry._2(k, o, acc);
    } else {
      switch (/* XXX */fmt.tag) {
        case "Char" :
            var rest = fmt.Arg0;
            return (function(k,acc,rest){
            return function (c) {
              var new_acc = /* constructor */{
                tag: "Acc_data_char",
                Arg0: acc,
                Arg1: c
              };
              return make_printf(k, o, new_acc, rest);
            }
            }(k,acc,rest));
        case "Caml_char" :
            var rest$1 = fmt.Arg0;
            return (function(k,acc,rest$1){
            return function (c) {
              var new_acc = /* constructor */{
                tag: "Acc_data_string",
                Arg0: acc,
                Arg1: format_caml_char(c)
              };
              return make_printf(k, o, new_acc, rest$1);
            }
            }(k,acc,rest$1));
        case "String" :
            return make_string_padding(k, o, acc, fmt.Arg1, fmt.Arg0, (function (str) {
                          return str;
                        }));
        case "Caml_string" :
            return make_string_padding(k, o, acc, fmt.Arg1, fmt.Arg0, string_to_caml_string);
        case "Int" :
            return make_int_padding_precision(k, o, acc, fmt.Arg3, fmt.Arg1, fmt.Arg2, convert_int, fmt.Arg0);
        case "Int32" :
            return make_int_padding_precision(k, o, acc, fmt.Arg3, fmt.Arg1, fmt.Arg2, convert_int32, fmt.Arg0);
        case "Nativeint" :
            return make_int_padding_precision(k, o, acc, fmt.Arg3, fmt.Arg1, fmt.Arg2, convert_nativeint, fmt.Arg0);
        case "Int64" :
            return make_int_padding_precision(k, o, acc, fmt.Arg3, fmt.Arg1, fmt.Arg2, convert_int64, fmt.Arg0);
        case "Float" :
            var k$1 = k;
            var o$1 = o;
            var acc$1 = acc;
            var fmt$1 = fmt.Arg3;
            var pad = fmt.Arg1;
            var prec = fmt.Arg2;
            var fconv = fmt.Arg0;
            if (typeof pad === "string") {
              if (typeof prec === "string") {
                if (prec === "No_precision") {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv){
                  return function (x) {
                    var str = convert_float(fconv, 6, x);
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv));
                } else {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv){
                  return function (p, x) {
                    var str = convert_float(fconv, p, x);
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv));
                }
              } else {
                var p = prec.Arg0;
                return (function(k$1,o$1,acc$1,fmt$1,fconv,p){
                return function (x) {
                  var str = convert_float(fconv, p, x);
                  return make_printf(k$1, o$1, /* constructor */{
                              tag: "Acc_data_string",
                              Arg0: acc$1,
                              Arg1: str
                            }, fmt$1);
                }
                }(k$1,o$1,acc$1,fmt$1,fconv,p));
              }
            } else if (/* XXX */pad.tag === "Lit_padding") {
              var w = pad.Arg1;
              var padty = pad.Arg0;
              if (typeof prec === "string") {
                if (prec === "No_precision") {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv,padty,w){
                  return function (x) {
                    var str = convert_float(fconv, 6, x);
                    var str$prime = fix_padding(padty, w, str);
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str$prime
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv,padty,w));
                } else {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv,padty,w){
                  return function (p, x) {
                    var str = fix_padding(padty, w, convert_float(fconv, p, x));
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv,padty,w));
                }
              } else {
                var p$1 = prec.Arg0;
                return (function(k$1,o$1,acc$1,fmt$1,fconv,padty,w,p$1){
                return function (x) {
                  var str = fix_padding(padty, w, convert_float(fconv, p$1, x));
                  return make_printf(k$1, o$1, /* constructor */{
                              tag: "Acc_data_string",
                              Arg0: acc$1,
                              Arg1: str
                            }, fmt$1);
                }
                }(k$1,o$1,acc$1,fmt$1,fconv,padty,w,p$1));
              }
            } else {
              var padty$1 = pad.Arg0;
              if (typeof prec === "string") {
                if (prec === "No_precision") {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv,padty$1){
                  return function (w, x) {
                    var str = convert_float(fconv, 6, x);
                    var str$prime = fix_padding(padty$1, w, str);
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str$prime
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv,padty$1));
                } else {
                  return (function(k$1,o$1,acc$1,fmt$1,fconv,padty$1){
                  return function (w, p, x) {
                    var str = fix_padding(padty$1, w, convert_float(fconv, p, x));
                    return make_printf(k$1, o$1, /* constructor */{
                                tag: "Acc_data_string",
                                Arg0: acc$1,
                                Arg1: str
                              }, fmt$1);
                  }
                  }(k$1,o$1,acc$1,fmt$1,fconv,padty$1));
                }
              } else {
                var p$2 = prec.Arg0;
                return (function(k$1,o$1,acc$1,fmt$1,fconv,padty$1,p$2){
                return function (w, x) {
                  var str = fix_padding(padty$1, w, convert_float(fconv, p$2, x));
                  return make_printf(k$1, o$1, /* constructor */{
                              tag: "Acc_data_string",
                              Arg0: acc$1,
                              Arg1: str
                            }, fmt$1);
                }
                }(k$1,o$1,acc$1,fmt$1,fconv,padty$1,p$2));
              }
            }
        case "Bool" :
            var rest$2 = fmt.Arg0;
            return (function(k,acc,rest$2){
            return function (b) {
              return make_printf(k, o, /* constructor */{
                          tag: "Acc_data_string",
                          Arg0: acc,
                          Arg1: b ? "true" : "false"
                        }, rest$2);
            }
            }(k,acc,rest$2));
        case "Flush" :
            _fmt = fmt.Arg0;
            _acc = /* constructor */{
              tag: "Acc_flush",
              Arg0: acc
            };
            continue ;
        case "String_literal" :
            _fmt = fmt.Arg1;
            _acc = /* constructor */{
              tag: "Acc_string_literal",
              Arg0: acc,
              Arg1: fmt.Arg0
            };
            continue ;
        case "Char_literal" :
            _fmt = fmt.Arg1;
            _acc = /* constructor */{
              tag: "Acc_char_literal",
              Arg0: acc,
              Arg1: fmt.Arg0
            };
            continue ;
        case "Format_arg" :
            var rest$3 = fmt.Arg2;
            var ty = string_of_fmtty(fmt.Arg1);
            return (function(k,acc,rest$3,ty){
            return function (str) {
              return make_printf(k, o, /* constructor */{
                          tag: "Acc_data_string",
                          Arg0: acc,
                          Arg1: ty
                        }, rest$3);
            }
            }(k,acc,rest$3,ty));
        case "Format_subst" :
            var rest$4 = fmt.Arg2;
            var fmtty = fmt.Arg1;
            return (function(k,acc,fmtty,rest$4){
            return function (param) {
              return make_printf(k, o, acc, CamlinternalFormatBasics.concat_fmt(recast(param.Arg0, fmtty), rest$4));
            }
            }(k,acc,fmtty,rest$4));
        case "Alpha" :
            var rest$5 = fmt.Arg0;
            return (function(k,acc,rest$5){
            return function (f, x) {
              return make_printf(k, o, /* constructor */{
                          tag: "Acc_delay",
                          Arg0: acc,
                          Arg1: (function (o) {
                              return Curry._2(f, o, x);
                            })
                        }, rest$5);
            }
            }(k,acc,rest$5));
        case "Theta" :
            var rest$6 = fmt.Arg0;
            return (function(k,acc,rest$6){
            return function (f) {
              return make_printf(k, o, /* constructor */{
                          tag: "Acc_delay",
                          Arg0: acc,
                          Arg1: f
                        }, rest$6);
            }
            }(k,acc,rest$6));
        case "Formatting_lit" :
            _fmt = fmt.Arg1;
            _acc = /* constructor */{
              tag: "Acc_formatting_lit",
              Arg0: acc,
              Arg1: fmt.Arg0
            };
            continue ;
        case "Formatting_gen" :
            var match = fmt.Arg0;
            if (/* XXX */match.tag === "Open_tag") {
              var rest$7 = fmt.Arg1;
              var k$prime = (function(k,acc,rest$7){
              return function k$prime(koc, kacc) {
                return make_printf(k, koc, /* constructor */{
                            tag: "Acc_formatting_gen",
                            Arg0: acc,
                            Arg1: /* constructor */{
                              tag: "Acc_open_tag",
                              Arg0: kacc
                            }
                          }, rest$7);
              }
              }(k,acc,rest$7));
              _fmt = match.Arg0.Arg0;
              _acc = "End_of_acc";
              _k = k$prime;
              continue ;
            } else {
              var rest$8 = fmt.Arg1;
              var k$prime$1 = (function(k,acc,rest$8){
              return function k$prime$1(koc, kacc) {
                return make_printf(k, koc, /* constructor */{
                            tag: "Acc_formatting_gen",
                            Arg0: acc,
                            Arg1: /* constructor */{
                              tag: "Acc_open_box",
                              Arg0: kacc
                            }
                          }, rest$8);
              }
              }(k,acc,rest$8));
              _fmt = match.Arg0.Arg0;
              _acc = "End_of_acc";
              _k = k$prime$1;
              continue ;
            }
        case "Reader" :
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    1449,
                    4
                  ]
                ];
        case "Scan_char_set" :
            var rest$9 = fmt.Arg2;
            var new_acc = /* constructor */{
              tag: "Acc_invalid_arg",
              Arg0: acc,
              Arg1: "Printf: bad conversion %["
            };
            return (function(k,rest$9,new_acc){
            return function (param) {
              return make_printf(k, o, new_acc, rest$9);
            }
            }(k,rest$9,new_acc));
        case "Scan_get_counter" :
            var rest$10 = fmt.Arg1;
            return (function(k,acc,rest$10){
            return function (n) {
              var new_acc = /* constructor */{
                tag: "Acc_data_string",
                Arg0: acc,
                Arg1: Caml_format.caml_format_int("%u", n)
              };
              return make_printf(k, o, new_acc, rest$10);
            }
            }(k,acc,rest$10));
        case "Scan_next_char" :
            var rest$11 = fmt.Arg0;
            return (function(k,acc,rest$11){
            return function (c) {
              var new_acc = /* constructor */{
                tag: "Acc_data_char",
                Arg0: acc,
                Arg1: c
              };
              return make_printf(k, o, new_acc, rest$11);
            }
            }(k,acc,rest$11));
        case "Ignored_param" :
            var k$2 = k;
            var o$2 = o;
            var acc$2 = acc;
            var ign = fmt.Arg0;
            var fmt$2 = fmt.Arg1;
            if (typeof ign === "string") {
              if (ign === "Ignored_reader") {
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "camlinternalFormat.ml",
                        1517,
                        39
                      ]
                    ];
              } else {
                return make_invalid_arg(k$2, o$2, acc$2, fmt$2);
              }
            } else if (/* XXX */ign.tag === "Ignored_format_subst") {
              return make_from_fmtty(k$2, o$2, acc$2, ign.Arg1, fmt$2);
            } else {
              return make_invalid_arg(k$2, o$2, acc$2, fmt$2);
            }
        case "Custom" :
            return make_custom(k, o, acc, fmt.Arg2, fmt.Arg0, Curry._1(fmt.Arg1, /* () */0));
        
      }
    }
  };
}

function make_from_fmtty(k, o, acc, fmtty, fmt) {
  if (typeof fmtty === "string") {
    return make_invalid_arg(k, o, acc, fmt);
  } else {
    switch (/* XXX */fmtty.tag) {
      case "Char_ty" :
          var rest = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest, fmt);
            });
      case "String_ty" :
          var rest$1 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$1, fmt);
            });
      case "Int_ty" :
          var rest$2 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$2, fmt);
            });
      case "Int32_ty" :
          var rest$3 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$3, fmt);
            });
      case "Nativeint_ty" :
          var rest$4 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$4, fmt);
            });
      case "Int64_ty" :
          var rest$5 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$5, fmt);
            });
      case "Float_ty" :
          var rest$6 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$6, fmt);
            });
      case "Bool_ty" :
          var rest$7 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$7, fmt);
            });
      case "Format_arg_ty" :
          var rest$8 = fmtty.Arg1;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$8, fmt);
            });
      case "Format_subst_ty" :
          var rest$9 = fmtty.Arg2;
          var ty = trans(symm(fmtty.Arg0), fmtty.Arg1);
          return (function (param) {
              return make_from_fmtty(k, o, acc, CamlinternalFormatBasics.concat_fmtty(ty, rest$9), fmt);
            });
      case "Alpha_ty" :
          var rest$10 = fmtty.Arg0;
          return (function (param, param$1) {
              return make_from_fmtty(k, o, acc, rest$10, fmt);
            });
      case "Theta_ty" :
          var rest$11 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$11, fmt);
            });
      case "Any_ty" :
          var rest$12 = fmtty.Arg0;
          return (function (param) {
              return make_from_fmtty(k, o, acc, rest$12, fmt);
            });
      case "Reader_ty" :
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "camlinternalFormat.ml",
                  1540,
                  31
                ]
              ];
      case "Ignored_reader_ty" :
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "camlinternalFormat.ml",
                  1541,
                  31
                ]
              ];
      
    }
  }
}

function make_invalid_arg(k, o, acc, fmt) {
  return make_printf(k, o, /* constructor */{
              tag: "Acc_invalid_arg",
              Arg0: acc,
              Arg1: "Printf: bad conversion %_"
            }, fmt);
}

function make_string_padding(k, o, acc, fmt, pad, trans) {
  if (typeof pad === "string") {
    return (function (x) {
        var new_acc = /* constructor */{
          tag: "Acc_data_string",
          Arg0: acc,
          Arg1: Curry._1(trans, x)
        };
        return make_printf(k, o, new_acc, fmt);
      });
  } else if (/* XXX */pad.tag === "Lit_padding") {
    var width = pad.Arg1;
    var padty = pad.Arg0;
    return (function (x) {
        var new_acc = /* constructor */{
          tag: "Acc_data_string",
          Arg0: acc,
          Arg1: fix_padding(padty, width, Curry._1(trans, x))
        };
        return make_printf(k, o, new_acc, fmt);
      });
  } else {
    var padty$1 = pad.Arg0;
    return (function (w, x) {
        var new_acc = /* constructor */{
          tag: "Acc_data_string",
          Arg0: acc,
          Arg1: fix_padding(padty$1, w, Curry._1(trans, x))
        };
        return make_printf(k, o, new_acc, fmt);
      });
  }
}

function make_int_padding_precision(k, o, acc, fmt, pad, prec, trans, iconv) {
  if (typeof pad === "string") {
    if (typeof prec === "string") {
      if (prec === "No_precision") {
        return (function (x) {
            var str = Curry._2(trans, iconv, x);
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      } else {
        return (function (p, x) {
            var str = fix_int_precision(p, Curry._2(trans, iconv, x));
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      }
    } else {
      var p = prec.Arg0;
      return (function (x) {
          var str = fix_int_precision(p, Curry._2(trans, iconv, x));
          return make_printf(k, o, /* constructor */{
                      tag: "Acc_data_string",
                      Arg0: acc,
                      Arg1: str
                    }, fmt);
        });
    }
  } else if (/* XXX */pad.tag === "Lit_padding") {
    var w = pad.Arg1;
    var padty = pad.Arg0;
    if (typeof prec === "string") {
      if (prec === "No_precision") {
        return (function (x) {
            var str = fix_padding(padty, w, Curry._2(trans, iconv, x));
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      } else {
        return (function (p, x) {
            var str = fix_padding(padty, w, fix_int_precision(p, Curry._2(trans, iconv, x)));
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      }
    } else {
      var p$1 = prec.Arg0;
      return (function (x) {
          var str = fix_padding(padty, w, fix_int_precision(p$1, Curry._2(trans, iconv, x)));
          return make_printf(k, o, /* constructor */{
                      tag: "Acc_data_string",
                      Arg0: acc,
                      Arg1: str
                    }, fmt);
        });
    }
  } else {
    var padty$1 = pad.Arg0;
    if (typeof prec === "string") {
      if (prec === "No_precision") {
        return (function (w, x) {
            var str = fix_padding(padty$1, w, Curry._2(trans, iconv, x));
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      } else {
        return (function (w, p, x) {
            var str = fix_padding(padty$1, w, fix_int_precision(p, Curry._2(trans, iconv, x)));
            return make_printf(k, o, /* constructor */{
                        tag: "Acc_data_string",
                        Arg0: acc,
                        Arg1: str
                      }, fmt);
          });
      }
    } else {
      var p$2 = prec.Arg0;
      return (function (w, x) {
          var str = fix_padding(padty$1, w, fix_int_precision(p$2, Curry._2(trans, iconv, x)));
          return make_printf(k, o, /* constructor */{
                      tag: "Acc_data_string",
                      Arg0: acc,
                      Arg1: str
                    }, fmt);
        });
    }
  }
}

function make_custom(k, o, acc, rest, arity, f) {
  if (arity !== "Custom_zero") {
    var arity$1 = arity.Arg0;
    return (function (x) {
        return make_custom(k, o, acc, rest, arity$1, Curry._1(f, x));
      });
  } else {
    return make_printf(k, o, /* constructor */{
                tag: "Acc_data_string",
                Arg0: acc,
                Arg1: f
              }, rest);
  }
}

function output_acc(o, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "string") {
      return /* () */0;
    } else {
      switch (/* XXX */acc.tag) {
        case "Acc_formatting_lit" :
            var s = string_of_formatting_lit(acc.Arg1);
            output_acc(o, acc.Arg0);
            return Pervasives.output_string(o, s);
        case "Acc_formatting_gen" :
            var match = acc.Arg1;
            var p = acc.Arg0;
            output_acc(o, p);
            if (/* XXX */match.tag === "Acc_open_tag") {
              Pervasives.output_string(o, "@{");
              _acc = match.Arg0;
              continue ;
            } else {
              Pervasives.output_string(o, "@[");
              _acc = match.Arg0;
              continue ;
            }
        case "Acc_string_literal" :
        case "Acc_data_string" :
            exit = 1;
            break;
        case "Acc_char_literal" :
        case "Acc_data_char" :
            exit = 2;
            break;
        case "Acc_delay" :
            output_acc(o, acc.Arg0);
            return Curry._1(acc.Arg1, o);
        case "Acc_flush" :
            output_acc(o, acc.Arg0);
            return Caml_io.caml_ml_flush(o);
        case "Acc_invalid_arg" :
            output_acc(o, acc.Arg0);
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  acc.Arg1
                ];
        
      }
    }
    switch (exit) {
      case 1 :
          output_acc(o, acc.Arg0);
          return Pervasives.output_string(o, acc.Arg1);
      case 2 :
          output_acc(o, acc.Arg0);
          return Caml_io.caml_ml_output_char(o, acc.Arg1);
      
    }
  };
}

function bufput_acc(b, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "string") {
      return /* () */0;
    } else {
      switch (/* XXX */acc.tag) {
        case "Acc_formatting_lit" :
            var s = string_of_formatting_lit(acc.Arg1);
            bufput_acc(b, acc.Arg0);
            return $$Buffer.add_string(b, s);
        case "Acc_formatting_gen" :
            var match = acc.Arg1;
            var p = acc.Arg0;
            bufput_acc(b, p);
            if (/* XXX */match.tag === "Acc_open_tag") {
              $$Buffer.add_string(b, "@{");
              _acc = match.Arg0;
              continue ;
            } else {
              $$Buffer.add_string(b, "@[");
              _acc = match.Arg0;
              continue ;
            }
        case "Acc_string_literal" :
        case "Acc_data_string" :
            exit = 1;
            break;
        case "Acc_char_literal" :
        case "Acc_data_char" :
            exit = 2;
            break;
        case "Acc_delay" :
            bufput_acc(b, acc.Arg0);
            return Curry._1(acc.Arg1, b);
        case "Acc_flush" :
            _acc = acc.Arg0;
            continue ;
        case "Acc_invalid_arg" :
            bufput_acc(b, acc.Arg0);
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  acc.Arg1
                ];
        
      }
    }
    switch (exit) {
      case 1 :
          bufput_acc(b, acc.Arg0);
          return $$Buffer.add_string(b, acc.Arg1);
      case 2 :
          bufput_acc(b, acc.Arg0);
          return $$Buffer.add_char(b, acc.Arg1);
      
    }
  };
}

function strput_acc(b, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "string") {
      return /* () */0;
    } else {
      switch (/* XXX */acc.tag) {
        case "Acc_formatting_lit" :
            var s = string_of_formatting_lit(acc.Arg1);
            strput_acc(b, acc.Arg0);
            return $$Buffer.add_string(b, s);
        case "Acc_formatting_gen" :
            var match = acc.Arg1;
            var p = acc.Arg0;
            strput_acc(b, p);
            if (/* XXX */match.tag === "Acc_open_tag") {
              $$Buffer.add_string(b, "@{");
              _acc = match.Arg0;
              continue ;
            } else {
              $$Buffer.add_string(b, "@[");
              _acc = match.Arg0;
              continue ;
            }
        case "Acc_string_literal" :
        case "Acc_data_string" :
            exit = 1;
            break;
        case "Acc_char_literal" :
        case "Acc_data_char" :
            exit = 2;
            break;
        case "Acc_delay" :
            strput_acc(b, acc.Arg0);
            return $$Buffer.add_string(b, Curry._1(acc.Arg1, /* () */0));
        case "Acc_flush" :
            _acc = acc.Arg0;
            continue ;
        case "Acc_invalid_arg" :
            strput_acc(b, acc.Arg0);
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  acc.Arg1
                ];
        
      }
    }
    switch (exit) {
      case 1 :
          strput_acc(b, acc.Arg0);
          return $$Buffer.add_string(b, acc.Arg1);
      case 2 :
          strput_acc(b, acc.Arg0);
          return $$Buffer.add_char(b, acc.Arg1);
      
    }
  };
}

function failwith_message(param) {
  var buf = $$Buffer.create(256);
  var k = function (param, acc) {
    strput_acc(buf, acc);
    var s = $$Buffer.contents(buf);
    throw [
          Caml_builtin_exceptions.failure,
          s
        ];
  };
  return make_printf(k, /* () */0, "End_of_acc", param.Arg0);
}

function open_box_of_string(str) {
  if (str === "") {
    return /* tuple */[
            0,
            "Pp_box"
          ];
  } else {
    var len = str.length;
    var invalid_box = function (param) {
      return Curry._1(failwith_message(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "invalid box description ",
                        Arg1: /* constructor */{
                          tag: "Caml_string",
                          Arg0: "No_padding",
                          Arg1: "End_of_format"
                        }
                      },
                      Arg1: "invalid box description %S"
                    }), str);
    };
    var parse_spaces = function (_i) {
      while(true) {
        var i = _i;
        if (i === len) {
          return i;
        } else {
          var match = Caml_string.get(str, i);
          if (match !== 9) {
            if (match !== 32) {
              return i;
            } else {
              _i = i + 1 | 0;
              continue ;
            }
          } else {
            _i = i + 1 | 0;
            continue ;
          }
        }
      };
    };
    var parse_lword = function (i, _j) {
      while(true) {
        var j = _j;
        if (j === len) {
          return j;
        } else {
          var match = Caml_string.get(str, j);
          if (match > 122 || match < 97) {
            return j;
          } else {
            _j = j + 1 | 0;
            continue ;
          }
        }
      };
    };
    var parse_int = function (i, _j) {
      while(true) {
        var j = _j;
        if (j === len) {
          return j;
        } else {
          var match = Caml_string.get(str, j);
          if (match >= 48) {
            if (match >= 58) {
              return j;
            } else {
              _j = j + 1 | 0;
              continue ;
            }
          } else if (match !== 45) {
            return j;
          } else {
            _j = j + 1 | 0;
            continue ;
          }
        }
      };
    };
    var wstart = parse_spaces(0);
    var wend = parse_lword(wstart, wstart);
    var box_name = $$String.sub(str, wstart, wend - wstart | 0);
    var nstart = parse_spaces(wend);
    var nend = parse_int(nstart, nstart);
    var indent;
    if (nstart === nend) {
      indent = 0;
    } else {
      try {
        indent = Caml_format.caml_int_of_string($$String.sub(str, nstart, nend - nstart | 0));
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn[0] === Caml_builtin_exceptions.failure) {
          indent = invalid_box(/* () */0);
        } else {
          throw exn;
        }
      }
    }
    var exp_end = parse_spaces(nend);
    if (exp_end !== len) {
      invalid_box(/* () */0);
    }
    var box_type;
    switch (box_name) {
      case "" :
      case "b" :
          box_type = "Pp_box";
          break;
      case "h" :
          box_type = "Pp_hbox";
          break;
      case "hov" :
          box_type = "Pp_hovbox";
          break;
      case "hv" :
          box_type = "Pp_hvbox";
          break;
      case "v" :
          box_type = "Pp_vbox";
          break;
      default:
        box_type = invalid_box(/* () */0);
    }
    return /* tuple */[
            indent,
            box_type
          ];
  }
}

function make_padding_fmt_ebb(pad, fmt) {
  if (typeof pad === "string") {
    return /* constructor */{
            tag: "Padding_fmt_EBB",
            Arg0: "No_padding",
            Arg1: fmt
          };
  } else if (/* XXX */pad.tag === "Lit_padding") {
    return /* constructor */{
            tag: "Padding_fmt_EBB",
            Arg0: /* constructor */{
              tag: "Lit_padding",
              Arg0: pad.Arg0,
              Arg1: pad.Arg1
            },
            Arg1: fmt
          };
  } else {
    return /* constructor */{
            tag: "Padding_fmt_EBB",
            Arg0: /* constructor */{
              tag: "Arg_padding",
              Arg0: pad.Arg0
            },
            Arg1: fmt
          };
  }
}

function make_precision_fmt_ebb(prec, fmt) {
  if (typeof prec === "string") {
    if (prec === "No_precision") {
      return /* constructor */{
              tag: "Precision_fmt_EBB",
              Arg0: "No_precision",
              Arg1: fmt
            };
    } else {
      return /* constructor */{
              tag: "Precision_fmt_EBB",
              Arg0: "Arg_precision",
              Arg1: fmt
            };
    }
  } else {
    return /* constructor */{
            tag: "Precision_fmt_EBB",
            Arg0: /* constructor */{
              tag: "Lit_precision",
              Arg0: prec.Arg0
            },
            Arg1: fmt
          };
  }
}

function make_padprec_fmt_ebb(pad, prec, fmt) {
  var match = make_precision_fmt_ebb(prec, fmt);
  var fmt$prime = match.Arg1;
  var prec$1 = match.Arg0;
  if (typeof pad === "string") {
    return /* constructor */{
            tag: "Padprec_fmt_EBB",
            Arg0: "No_padding",
            Arg1: prec$1,
            Arg2: fmt$prime
          };
  } else if (/* XXX */pad.tag === "Lit_padding") {
    return /* constructor */{
            tag: "Padprec_fmt_EBB",
            Arg0: /* constructor */{
              tag: "Lit_padding",
              Arg0: pad.Arg0,
              Arg1: pad.Arg1
            },
            Arg1: prec$1,
            Arg2: fmt$prime
          };
  } else {
    return /* constructor */{
            tag: "Padprec_fmt_EBB",
            Arg0: /* constructor */{
              tag: "Arg_padding",
              Arg0: pad.Arg0
            },
            Arg1: prec$1,
            Arg2: fmt$prime
          };
  }
}

function fmt_ebb_of_string(legacy_behavior, str) {
  var legacy_behavior$1 = legacy_behavior !== undefined ? legacy_behavior : true;
  var invalid_format_message = function (str_ind, msg) {
    return Curry._3(failwith_message(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "invalid format ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: ": at character number ",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ", ",
                              Arg1: /* constructor */{
                                tag: "String",
                                Arg0: "No_padding",
                                Arg1: "End_of_format"
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "invalid format %S: at character number %d, %s"
                  }), str, str_ind, msg);
  };
  var invalid_format_without = function (str_ind, c, s) {
    return Curry._4(failwith_message(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "invalid format ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: ": at character number ",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ", '",
                              Arg1: /* constructor */{
                                tag: "Char",
                                Arg0: /* constructor */{
                                  tag: "String_literal",
                                  Arg0: "' without ",
                                  Arg1: /* constructor */{
                                    tag: "String",
                                    Arg0: "No_padding",
                                    Arg1: "End_of_format"
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "invalid format %S: at character number %d, '%c' without %s"
                  }), str, str_ind, c, s);
  };
  var expected_character = function (str_ind, expected, read) {
    return Curry._4(failwith_message(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "invalid format ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: ": at character number ",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ", ",
                              Arg1: /* constructor */{
                                tag: "String",
                                Arg0: "No_padding",
                                Arg1: /* constructor */{
                                  tag: "String_literal",
                                  Arg0: " expected, read ",
                                  Arg1: /* constructor */{
                                    tag: "Caml_char",
                                    Arg0: "End_of_format"
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "invalid format %S: at character number %d, %s expected, read %C"
                  }), str, str_ind, expected, read);
  };
  var compute_int_conv = function (pct_ind, str_ind, _plus, _sharp, _space, symb) {
    while(true) {
      var space = _space;
      var sharp = _sharp;
      var plus = _plus;
      var exit = 0;
      if (plus) {
        if (sharp) {
          exit = 2;
        } else if (!space) {
          if (symb !== 100) {
            if (symb === 105) {
              return "Int_pi";
            }
            
          } else {
            return "Int_pd";
          }
        }
        
      } else if (sharp) {
        if (space) {
          exit = 2;
        } else if (symb !== 88) {
          if (symb !== 111) {
            if (symb !== 120) {
              exit = 2;
            } else {
              return "Int_Cx";
            }
          } else {
            return "Int_Co";
          }
        } else {
          return "Int_CX";
        }
      } else if (space) {
        if (symb !== 100) {
          if (symb === 105) {
            return "Int_si";
          }
          
        } else {
          return "Int_sd";
        }
      } else {
        switch (symb) {
          case 88 :
              return "Int_X";
          case 100 :
              return "Int_d";
          case 105 :
              return "Int_i";
          case 111 :
              return "Int_o";
          case 117 :
              return "Int_u";
          case 89 :
          case 90 :
          case 91 :
          case 92 :
          case 93 :
          case 94 :
          case 95 :
          case 96 :
          case 97 :
          case 98 :
          case 99 :
          case 101 :
          case 102 :
          case 103 :
          case 104 :
          case 106 :
          case 107 :
          case 108 :
          case 109 :
          case 110 :
          case 112 :
          case 113 :
          case 114 :
          case 115 :
          case 116 :
          case 118 :
          case 119 :
              break;
          case 120 :
              return "Int_x";
          default:
            
        }
      }
      if (exit === 2) {
        var exit$1 = 0;
        switch (symb) {
          case 88 :
              if (legacy_behavior$1) {
                return "Int_CX";
              }
              break;
          case 111 :
              if (legacy_behavior$1) {
                return "Int_Co";
              }
              break;
          case 100 :
          case 105 :
          case 117 :
              exit$1 = 3;
              break;
          case 89 :
          case 90 :
          case 91 :
          case 92 :
          case 93 :
          case 94 :
          case 95 :
          case 96 :
          case 97 :
          case 98 :
          case 99 :
          case 101 :
          case 102 :
          case 103 :
          case 104 :
          case 106 :
          case 107 :
          case 108 :
          case 109 :
          case 110 :
          case 112 :
          case 113 :
          case 114 :
          case 115 :
          case 116 :
          case 118 :
          case 119 :
              break;
          case 120 :
              if (legacy_behavior$1) {
                return "Int_Cx";
              }
              break;
          default:
            
        }
        if (exit$1 === 3) {
          if (legacy_behavior$1) {
            _sharp = false;
            continue ;
          } else {
            return incompatible_flag(pct_ind, str_ind, symb, "'#'");
          }
        }
        
      }
      if (plus) {
        if (space) {
          if (legacy_behavior$1) {
            _space = false;
            continue ;
          } else {
            return incompatible_flag(pct_ind, str_ind, /* " " */32, "'+'");
          }
        } else if (legacy_behavior$1) {
          _plus = false;
          continue ;
        } else {
          return incompatible_flag(pct_ind, str_ind, symb, "'+'");
        }
      } else if (space) {
        if (legacy_behavior$1) {
          _space = false;
          continue ;
        } else {
          return incompatible_flag(pct_ind, str_ind, symb, "' '");
        }
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                2719,
                28
              ]
            ];
      }
    };
  };
  var incompatible_flag = function (pct_ind, str_ind, symb, option) {
    var subfmt = $$String.sub(str, pct_ind, str_ind - pct_ind | 0);
    return Curry._5(failwith_message(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "invalid format ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: ": at character number ",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ", ",
                              Arg1: /* constructor */{
                                tag: "String",
                                Arg0: "No_padding",
                                Arg1: /* constructor */{
                                  tag: "String_literal",
                                  Arg0: " is incompatible with '",
                                  Arg1: /* constructor */{
                                    tag: "Char",
                                    Arg0: /* constructor */{
                                      tag: "String_literal",
                                      Arg0: "' in sub-format ",
                                      Arg1: /* constructor */{
                                        tag: "Caml_string",
                                        Arg0: "No_padding",
                                        Arg1: "End_of_format"
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "invalid format %S: at character number %d, %s is incompatible with '%c' in sub-format %S"
                  }), str, pct_ind, option, symb, subfmt);
  };
  var parse_positive = function (_str_ind, end_ind, _acc) {
    while(true) {
      var acc = _acc;
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var c = Caml_string.get(str, str_ind);
      if (c > 57 || c < 48) {
        return /* tuple */[
                str_ind,
                acc
              ];
      } else {
        var new_acc = Caml_int32.imul(acc, 10) + (c - /* "0" */48 | 0) | 0;
        _acc = new_acc;
        _str_ind = str_ind + 1 | 0;
        continue ;
      }
    };
  };
  var parse_after_precision = function (pct_ind, str_ind, end_ind, minus, plus, sharp, space, ign, pad, prec) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var parse_conv = function (padprec) {
      return parse_conversion(pct_ind, str_ind + 1 | 0, end_ind, plus, sharp, space, ign, pad, prec, padprec, Caml_string.get(str, str_ind));
    };
    if (typeof pad === "string") {
      if (typeof prec === "string" && prec === "No_precision") {
        return parse_conv("No_padding");
      }
      if (minus) {
        if (typeof prec === "string") {
          return parse_conv(/* constructor */{
                      tag: "Arg_padding",
                      Arg0: "Left"
                    });
        } else {
          return parse_conv(/* constructor */{
                      tag: "Lit_padding",
                      Arg0: "Left",
                      Arg1: prec.Arg0
                    });
        }
      } else if (typeof prec === "string") {
        return parse_conv(/* constructor */{
                    tag: "Arg_padding",
                    Arg0: "Right"
                  });
      } else {
        return parse_conv(/* constructor */{
                    tag: "Lit_padding",
                    Arg0: "Right",
                    Arg1: prec.Arg0
                  });
      }
    } else {
      return parse_conv(pad);
    }
  };
  var parse_after_padding = function (pct_ind, str_ind, end_ind, minus, plus, sharp, space, ign, pad) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var symb = Caml_string.get(str, str_ind);
    if (symb !== 46) {
      return parse_conversion(pct_ind, str_ind + 1 | 0, end_ind, plus, sharp, space, ign, pad, "No_precision", pad, symb);
    } else {
      var pct_ind$1 = pct_ind;
      var str_ind$1 = str_ind + 1 | 0;
      var end_ind$1 = end_ind;
      var minus$1 = minus;
      var plus$1 = plus;
      var sharp$1 = sharp;
      var space$1 = space;
      var ign$1 = ign;
      var pad$1 = pad;
      if (str_ind$1 === end_ind$1) {
        invalid_format_message(end_ind$1, "unexpected end of format");
      }
      var parse_literal = function (minus, str_ind) {
        var match = parse_positive(str_ind, end_ind$1, 0);
        return parse_after_precision(pct_ind$1, match[0], end_ind$1, minus, plus$1, sharp$1, space$1, ign$1, pad$1, /* constructor */{
                    tag: "Lit_precision",
                    Arg0: match[1]
                  });
      };
      var symb$1 = Caml_string.get(str, str_ind$1);
      var exit = 0;
      if (symb$1 >= 48) {
        if (symb$1 < 58) {
          return parse_literal(minus$1, str_ind$1);
        }
        
      } else if (symb$1 >= 42) {
        switch (symb$1 - 42 | 0) {
          case 0 :
              return parse_after_precision(pct_ind$1, str_ind$1 + 1 | 0, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, pad$1, "Arg_precision");
          case 1 :
          case 3 :
              exit = 2;
              break;
          case 2 :
          case 4 :
          case 5 :
              break;
          
        }
      }
      if (exit === 2 && legacy_behavior$1) {
        return parse_literal(minus$1 || symb$1 === /* "-" */45, str_ind$1 + 1 | 0);
      }
      if (legacy_behavior$1) {
        return parse_after_precision(pct_ind$1, str_ind$1, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, pad$1, /* constructor */{
                    tag: "Lit_precision",
                    Arg0: 0
                  });
      } else {
        return invalid_format_without(str_ind$1 - 1 | 0, /* "." */46, "precision");
      }
    }
  };
  var parse_literal = function (lit_start, _str_ind, end_ind) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        return add_literal(lit_start, str_ind, "End_of_format");
      } else {
        var match = Caml_string.get(str, str_ind);
        if (match !== 37) {
          if (match !== 64) {
            _str_ind = str_ind + 1 | 0;
            continue ;
          } else {
            var match$1 = parse_after_at(str_ind + 1 | 0, end_ind);
            return add_literal(lit_start, str_ind, match$1.Arg0);
          }
        } else {
          var match$2 = parse_format(str_ind, end_ind);
          return add_literal(lit_start, str_ind, match$2.Arg0);
        }
      }
    };
  };
  var parse_format = function (pct_ind, end_ind) {
    var pct_ind$1 = pct_ind;
    var str_ind = pct_ind + 1 | 0;
    var end_ind$1 = end_ind;
    if (str_ind === end_ind$1) {
      invalid_format_message(end_ind$1, "unexpected end of format");
    }
    var match = Caml_string.get(str, str_ind);
    if (match !== 95) {
      return parse_flags(pct_ind$1, str_ind, end_ind$1, false);
    } else {
      return parse_flags(pct_ind$1, str_ind + 1 | 0, end_ind$1, true);
    }
  };
  var parse_after_at = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      return /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Char_literal",
                Arg0: /* "@" */64,
                Arg1: "End_of_format"
              }
            };
    } else {
      var c = Caml_string.get(str, str_ind);
      if (c >= 65) {
        if (c >= 94) {
          switch (c) {
            case 123 :
                return parse_tag(true, str_ind + 1 | 0, end_ind);
            case 124 :
                break;
            case 125 :
                var beg_ind = str_ind + 1 | 0;
                var match = parse_literal(beg_ind, beg_ind, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: "Close_tag",
                          Arg1: match.Arg0
                        }
                      };
            default:
              
          }
        } else if (c >= 91) {
          switch (c - 91 | 0) {
            case 0 :
                return parse_tag(false, str_ind + 1 | 0, end_ind);
            case 1 :
                break;
            case 2 :
                var beg_ind$1 = str_ind + 1 | 0;
                var match$1 = parse_literal(beg_ind$1, beg_ind$1, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: "Close_box",
                          Arg1: match$1.Arg0
                        }
                      };
            
          }
        }
        
      } else if (c !== 10) {
        if (c >= 32) {
          switch (c - 32 | 0) {
            case 0 :
                var beg_ind$2 = str_ind + 1 | 0;
                var match$2 = parse_literal(beg_ind$2, beg_ind$2, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: /* constructor */{
                            tag: "Break",
                            Arg0: "@ ",
                            Arg1: 1,
                            Arg2: 0
                          },
                          Arg1: match$2.Arg0
                        }
                      };
            case 5 :
                if ((str_ind + 1 | 0) < end_ind && Caml_string.get(str, str_ind + 1 | 0) === /* "%" */37) {
                  var beg_ind$3 = str_ind + 2 | 0;
                  var match$3 = parse_literal(beg_ind$3, beg_ind$3, end_ind);
                  return /* constructor */{
                          tag: "Fmt_EBB",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: "Escaped_percent",
                            Arg1: match$3.Arg0
                          }
                        };
                } else {
                  var match$4 = parse_literal(str_ind, str_ind, end_ind);
                  return /* constructor */{
                          tag: "Fmt_EBB",
                          Arg0: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* "@" */64,
                            Arg1: match$4.Arg0
                          }
                        };
                }
            case 12 :
                var beg_ind$4 = str_ind + 1 | 0;
                var match$5 = parse_literal(beg_ind$4, beg_ind$4, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: /* constructor */{
                            tag: "Break",
                            Arg0: "@,",
                            Arg1: 0,
                            Arg2: 0
                          },
                          Arg1: match$5.Arg0
                        }
                      };
            case 14 :
                var beg_ind$5 = str_ind + 1 | 0;
                var match$6 = parse_literal(beg_ind$5, beg_ind$5, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: "Flush_newline",
                          Arg1: match$6.Arg0
                        }
                      };
            case 27 :
                var str_ind$1 = str_ind + 1 | 0;
                var end_ind$1 = end_ind;
                var match$7;
                try {
                  if (str_ind$1 === end_ind$1 || Caml_string.get(str, str_ind$1) !== /* "<" */60) {
                    throw Caml_builtin_exceptions.not_found;
                  }
                  var str_ind_1 = parse_spaces(str_ind$1 + 1 | 0, end_ind$1);
                  var match$8 = Caml_string.get(str, str_ind_1);
                  var exit = 0;
                  if (match$8 >= 48) {
                    if (match$8 >= 58) {
                      throw Caml_builtin_exceptions.not_found;
                    }
                    exit = 1;
                  } else {
                    if (match$8 !== 45) {
                      throw Caml_builtin_exceptions.not_found;
                    }
                    exit = 1;
                  }
                  if (exit === 1) {
                    var match$9 = parse_integer(str_ind_1, end_ind$1);
                    var width = match$9[1];
                    var str_ind_3 = parse_spaces(match$9[0], end_ind$1);
                    var match$10 = Caml_string.get(str, str_ind_3);
                    var switcher = match$10 - 45 | 0;
                    if (switcher > 12 || switcher < 0) {
                      if (switcher !== 17) {
                        throw Caml_builtin_exceptions.not_found;
                      }
                      var s = $$String.sub(str, str_ind$1 - 2 | 0, (str_ind_3 - str_ind$1 | 0) + 3 | 0);
                      match$7 = /* tuple */[
                        str_ind_3 + 1 | 0,
                        /* constructor */{
                          tag: "Break",
                          Arg0: s,
                          Arg1: width,
                          Arg2: 0
                        }
                      ];
                    } else if (switcher === 2 || switcher === 1) {
                      throw Caml_builtin_exceptions.not_found;
                    } else {
                      var match$11 = parse_integer(str_ind_3, end_ind$1);
                      var str_ind_5 = parse_spaces(match$11[0], end_ind$1);
                      if (Caml_string.get(str, str_ind_5) !== /* ">" */62) {
                        throw Caml_builtin_exceptions.not_found;
                      }
                      var s$1 = $$String.sub(str, str_ind$1 - 2 | 0, (str_ind_5 - str_ind$1 | 0) + 3 | 0);
                      match$7 = /* tuple */[
                        str_ind_5 + 1 | 0,
                        /* constructor */{
                          tag: "Break",
                          Arg0: s$1,
                          Arg1: width,
                          Arg2: match$11[1]
                        }
                      ];
                    }
                  }
                  
                }
                catch (raw_exn){
                  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                  if (exn === Caml_builtin_exceptions.not_found) {
                    match$7 = /* tuple */[
                      str_ind$1,
                      /* constructor */{
                        tag: "Break",
                        Arg0: "@;",
                        Arg1: 1,
                        Arg2: 0
                      }
                    ];
                  } else if (exn[0] === Caml_builtin_exceptions.failure) {
                    match$7 = /* tuple */[
                      str_ind$1,
                      /* constructor */{
                        tag: "Break",
                        Arg0: "@;",
                        Arg1: 1,
                        Arg2: 0
                      }
                    ];
                  } else {
                    throw exn;
                  }
                }
                var next_ind = match$7[0];
                var match$12 = parse_literal(next_ind, next_ind, end_ind$1);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: match$7[1],
                          Arg1: match$12.Arg0
                        }
                      };
            case 28 :
                var str_ind$2 = str_ind + 1 | 0;
                var end_ind$2 = end_ind;
                var match$13;
                try {
                  var str_ind_1$1 = parse_spaces(str_ind$2, end_ind$2);
                  var match$14 = Caml_string.get(str, str_ind_1$1);
                  var exit$1 = 0;
                  if (match$14 >= 48) {
                    if (match$14 >= 58) {
                      match$13 = undefined;
                    } else {
                      exit$1 = 1;
                    }
                  } else if (match$14 !== 45) {
                    match$13 = undefined;
                  } else {
                    exit$1 = 1;
                  }
                  if (exit$1 === 1) {
                    var match$15 = parse_integer(str_ind_1$1, end_ind$2);
                    var str_ind_3$1 = parse_spaces(match$15[0], end_ind$2);
                    if (Caml_string.get(str, str_ind_3$1) !== /* ">" */62) {
                      throw Caml_builtin_exceptions.not_found;
                    }
                    var s$2 = $$String.sub(str, str_ind$2 - 2 | 0, (str_ind_3$1 - str_ind$2 | 0) + 3 | 0);
                    match$13 = /* tuple */[
                      str_ind_3$1 + 1 | 0,
                      /* constructor */{
                        tag: "Magic_size",
                        Arg0: s$2,
                        Arg1: match$15[1]
                      }
                    ];
                  }
                  
                }
                catch (raw_exn$1){
                  var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                  if (exn$1 === Caml_builtin_exceptions.not_found || exn$1[0] === Caml_builtin_exceptions.failure) {
                    match$13 = undefined;
                  } else {
                    throw exn$1;
                  }
                }
                if (match$13 !== undefined) {
                  var match$16 = match$13;
                  var next_ind$1 = match$16[0];
                  var match$17 = parse_literal(next_ind$1, next_ind$1, end_ind$2);
                  return /* constructor */{
                          tag: "Fmt_EBB",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: match$16[1],
                            Arg1: match$17.Arg0
                          }
                        };
                } else {
                  var match$18 = parse_literal(str_ind$2, str_ind$2, end_ind$2);
                  return /* constructor */{
                          tag: "Fmt_EBB",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: /* constructor */{
                              tag: "Scan_indic",
                              Arg0: /* "<" */60
                            },
                            Arg1: match$18.Arg0
                          }
                        };
                }
            case 1 :
            case 2 :
            case 3 :
            case 4 :
            case 6 :
            case 7 :
            case 8 :
            case 9 :
            case 10 :
            case 11 :
            case 13 :
            case 15 :
            case 16 :
            case 17 :
            case 18 :
            case 19 :
            case 20 :
            case 21 :
            case 22 :
            case 23 :
            case 24 :
            case 25 :
            case 26 :
            case 29 :
            case 30 :
                break;
            case 31 :
                var beg_ind$6 = str_ind + 1 | 0;
                var match$19 = parse_literal(beg_ind$6, beg_ind$6, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: "FFlush",
                          Arg1: match$19.Arg0
                        }
                      };
            case 32 :
                var beg_ind$7 = str_ind + 1 | 0;
                var match$20 = parse_literal(beg_ind$7, beg_ind$7, end_ind);
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Formatting_lit",
                          Arg0: "Escaped_at",
                          Arg1: match$20.Arg0
                        }
                      };
            
          }
        }
        
      } else {
        var beg_ind$8 = str_ind + 1 | 0;
        var match$21 = parse_literal(beg_ind$8, beg_ind$8, end_ind);
        return /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Formatting_lit",
                  Arg0: "Force_newline",
                  Arg1: match$21.Arg0
                }
              };
      }
      var beg_ind$9 = str_ind + 1 | 0;
      var match$22 = parse_literal(beg_ind$9, beg_ind$9, end_ind);
      return /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Formatting_lit",
                Arg0: /* constructor */{
                  tag: "Scan_indic",
                  Arg0: c
                },
                Arg1: match$22.Arg0
              }
            };
    }
  };
  var add_literal = function (lit_start, str_ind, fmt) {
    var size = str_ind - lit_start | 0;
    if (size !== 0) {
      if (size !== 1) {
        return /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: $$String.sub(str, lit_start, size),
                  Arg1: fmt
                }
              };
      } else {
        return /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Char_literal",
                  Arg0: Caml_string.get(str, lit_start),
                  Arg1: fmt
                }
              };
      }
    } else {
      return /* constructor */{
              tag: "Fmt_EBB",
              Arg0: fmt
            };
    }
  };
  var parse_spaces = function (_str_ind, end_ind) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      if (Caml_string.get(str, str_ind) === /* " " */32) {
        _str_ind = str_ind + 1 | 0;
        continue ;
      } else {
        return str_ind;
      }
    };
  };
  var parse_integer = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var match = Caml_string.get(str, str_ind);
    if (match >= 48) {
      if (match >= 58) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                2624,
                11
              ]
            ];
      }
      return parse_positive(str_ind, end_ind, 0);
    } else {
      if (match !== 45) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                2624,
                11
              ]
            ];
      }
      if ((str_ind + 1 | 0) === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var c = Caml_string.get(str, str_ind + 1 | 0);
      if (c > 57 || c < 48) {
        return expected_character(str_ind + 1 | 0, "digit", c);
      } else {
        var match$1 = parse_positive(str_ind + 1 | 0, end_ind, 0);
        return /* tuple */[
                match$1[0],
                -match$1[1] | 0
              ];
      }
    }
  };
  var compute_float_conv = function (pct_ind, str_ind, _plus, _space, symb) {
    while(true) {
      var space = _space;
      var plus = _plus;
      if (plus) {
        if (space) {
          if (legacy_behavior$1) {
            _space = false;
            continue ;
          } else {
            return incompatible_flag(pct_ind, str_ind, /* " " */32, "'+'");
          }
        } else {
          if (symb >= 72) {
            switch (symb) {
              case 101 :
                  return "Float_pe";
              case 102 :
                  return "Float_pf";
              case 103 :
                  return "Float_pg";
              default:
                
            }
          } else if (symb >= 69) {
            switch (symb - 69 | 0) {
              case 0 :
                  return "Float_pE";
              case 1 :
                  break;
              case 2 :
                  return "Float_pG";
              
            }
          }
          if (legacy_behavior$1) {
            _plus = false;
            continue ;
          } else {
            return incompatible_flag(pct_ind, str_ind, symb, "'+'");
          }
        }
      } else if (space) {
        if (symb >= 72) {
          switch (symb) {
            case 101 :
                return "Float_se";
            case 102 :
                return "Float_sf";
            case 103 :
                return "Float_sg";
            default:
              
          }
        } else if (symb >= 69) {
          switch (symb - 69 | 0) {
            case 0 :
                return "Float_sE";
            case 1 :
                break;
            case 2 :
                return "Float_sG";
            
          }
        }
        if (legacy_behavior$1) {
          _space = false;
          continue ;
        } else {
          return incompatible_flag(pct_ind, str_ind, symb, "' '");
        }
      } else if (symb >= 72) {
        switch (symb) {
          case 101 :
              return "Float_e";
          case 102 :
              return "Float_f";
          case 103 :
              return "Float_g";
          default:
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "camlinternalFormat.ml",
                    2747,
                    25
                  ]
                ];
        }
      } else if (symb >= 69) {
        switch (symb - 69 | 0) {
          case 0 :
              return "Float_E";
          case 1 :
              return "Float_F";
          case 2 :
              return "Float_G";
          
        }
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "camlinternalFormat.ml",
                2747,
                25
              ]
            ];
      }
    };
  };
  var search_subformat_end = function (_str_ind, end_ind, c) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        Curry._3(failwith_message(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "invalid format ",
                    Arg1: /* constructor */{
                      tag: "Caml_string",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ": unclosed sub-format, expected \"",
                        Arg1: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* "%" */37,
                          Arg1: /* constructor */{
                            tag: "Char",
                            Arg0: /* constructor */{
                              tag: "String_literal",
                              Arg0: "\" at character number ",
                              Arg1: /* constructor */{
                                tag: "Int",
                                Arg0: "Int_d",
                                Arg1: "No_padding",
                                Arg2: "No_precision",
                                Arg3: "End_of_format"
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  Arg1: "invalid format %S: unclosed sub-format, expected \"%%%c\" at character number %d"
                }), str, c, end_ind);
      }
      var match = Caml_string.get(str, str_ind);
      if (match !== 37) {
        _str_ind = str_ind + 1 | 0;
        continue ;
      } else {
        if ((str_ind + 1 | 0) === end_ind) {
          invalid_format_message(end_ind, "unexpected end of format");
        }
        if (Caml_string.get(str, str_ind + 1 | 0) === c) {
          return str_ind;
        } else {
          var match$1 = Caml_string.get(str, str_ind + 1 | 0);
          if (match$1 >= 95) {
            if (match$1 >= 123) {
              if (match$1 < 126) {
                switch (match$1 - 123 | 0) {
                  case 0 :
                      var sub_end = search_subformat_end(str_ind + 2 | 0, end_ind, /* "}" */125);
                      _str_ind = sub_end + 2 | 0;
                      continue ;
                  case 1 :
                      break;
                  case 2 :
                      return expected_character(str_ind + 1 | 0, "character ')'", /* "}" */125);
                  
                }
              }
              
            } else if (match$1 < 96) {
              if ((str_ind + 2 | 0) === end_ind) {
                invalid_format_message(end_ind, "unexpected end of format");
              }
              var match$2 = Caml_string.get(str, str_ind + 2 | 0);
              if (match$2 !== 40) {
                if (match$2 !== 123) {
                  _str_ind = str_ind + 3 | 0;
                  continue ;
                } else {
                  var sub_end$1 = search_subformat_end(str_ind + 3 | 0, end_ind, /* "}" */125);
                  _str_ind = sub_end$1 + 2 | 0;
                  continue ;
                }
              } else {
                var sub_end$2 = search_subformat_end(str_ind + 3 | 0, end_ind, /* ")" */41);
                _str_ind = sub_end$2 + 2 | 0;
                continue ;
              }
            }
            
          } else if (match$1 !== 40) {
            if (match$1 === 41) {
              return expected_character(str_ind + 1 | 0, "character '}'", /* ")" */41);
            }
            
          } else {
            var sub_end$3 = search_subformat_end(str_ind + 2 | 0, end_ind, /* ")" */41);
            _str_ind = sub_end$3 + 2 | 0;
            continue ;
          }
          _str_ind = str_ind + 2 | 0;
          continue ;
        }
      }
    };
  };
  var parse_conversion = function (pct_ind, str_ind, end_ind, plus, sharp, space, ign, pad, prec, padprec, symb) {
    var plus_used = false;
    var sharp_used = false;
    var space_used = false;
    var ign_used = /* record */[/* contents */false];
    var pad_used = false;
    var prec_used = /* record */[/* contents */false];
    var check_no_0 = function (symb, pad) {
      if (typeof pad === "string") {
        return pad;
      } else if (/* XXX */pad.tag === "Lit_padding") {
        switch (pad.Arg0) {
          case "Left" :
          case "Right" :
              return pad;
          case "Zeros" :
              if (legacy_behavior$1) {
                return /* constructor */{
                        tag: "Lit_padding",
                        Arg0: "Right",
                        Arg1: pad.Arg1
                      };
              } else {
                return incompatible_flag(pct_ind, str_ind, symb, "0");
              }
          
        }
      } else {
        switch (pad.Arg0) {
          case "Left" :
          case "Right" :
              return pad;
          case "Zeros" :
              if (legacy_behavior$1) {
                return /* constructor */{
                        tag: "Arg_padding",
                        Arg0: "Right"
                      };
              } else {
                return incompatible_flag(pct_ind, str_ind, symb, "0");
              }
          
        }
      }
    };
    var opt_of_pad = function (c, pad) {
      if (typeof pad === "string") {
        return ;
      } else if (/* XXX */pad.tag === "Lit_padding") {
        switch (pad.Arg0) {
          case "Left" :
              if (legacy_behavior$1) {
                return pad.Arg1;
              } else {
                return incompatible_flag(pct_ind, str_ind, c, "'-'");
              }
          case "Right" :
              return pad.Arg1;
          case "Zeros" :
              if (legacy_behavior$1) {
                return pad.Arg1;
              } else {
                return incompatible_flag(pct_ind, str_ind, c, "'0'");
              }
          
        }
      } else {
        return incompatible_flag(pct_ind, str_ind, c, "'*'");
      }
    };
    var get_prec_opt = function (param) {
      prec_used[0] = true;
      if (typeof prec === "string") {
        if (prec === "No_precision") {
          return ;
        } else {
          return incompatible_flag(pct_ind, str_ind, /* "_" */95, "'*'");
        }
      } else {
        return prec.Arg0;
      }
    };
    var fmt_result;
    var exit = 0;
    var exit$1 = 0;
    var exit$2 = 0;
    if (symb >= 124) {
      exit$1 = 6;
    } else {
      switch (symb) {
        case 33 :
            var match = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Flush",
                Arg0: match.Arg0
              }
            };
            break;
        case 40 :
            var sub_end = search_subformat_end(str_ind, end_ind, /* ")" */41);
            var beg_ind = sub_end + 2 | 0;
            var match$1 = parse_literal(beg_ind, beg_ind, end_ind);
            var fmt_rest = match$1.Arg0;
            var match$2 = parse_literal(str_ind, str_ind, sub_end);
            var sub_fmtty = fmtty_of_fmt(match$2.Arg0);
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored = /* constructor */{
                tag: "Ignored_format_subst",
                Arg0: opt_of_pad(/* "_" */95, pad),
                Arg1: sub_fmtty
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored,
                  Arg1: fmt_rest
                }
              };
            } else {
              pad_used = true;
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Format_subst",
                  Arg0: opt_of_pad(/* "(" */40, pad),
                  Arg1: sub_fmtty,
                  Arg2: fmt_rest
                }
              };
            }
            break;
        case 44 :
            fmt_result = parse_literal(str_ind, str_ind, end_ind);
            break;
        case 37 :
        case 64 :
            exit$1 = 4;
            break;
        case 67 :
            var match$3 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$1 = match$3.Arg0;
            fmt_result = (ign_used[0] = true, ign) ? /* constructor */({
                  tag: "Fmt_EBB",
                  Arg0: /* constructor */{
                    tag: "Ignored_param",
                    Arg0: "Ignored_caml_char",
                    Arg1: fmt_rest$1
                  }
                }) : /* constructor */({
                  tag: "Fmt_EBB",
                  Arg0: /* constructor */{
                    tag: "Caml_char",
                    Arg0: fmt_rest$1
                  }
                });
            break;
        case 78 :
            var match$4 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$2 = match$4.Arg0;
            if (ign_used[0] = true, ign) {
              var ignored$1 = /* constructor */{
                tag: "Ignored_scan_get_counter",
                Arg0: "Token_counter"
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$1,
                  Arg1: fmt_rest$2
                }
              };
            } else {
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Scan_get_counter",
                  Arg0: "Token_counter",
                  Arg1: fmt_rest$2
                }
              };
            }
            break;
        case 83 :
            pad_used = true;
            var pad$1 = check_no_0(symb, padprec);
            var match$5 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$3 = match$5.Arg0;
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored$2 = /* constructor */{
                tag: "Ignored_caml_string",
                Arg0: opt_of_pad(/* "_" */95, padprec)
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$2,
                  Arg1: fmt_rest$3
                }
              };
            } else {
              var match$6 = make_padding_fmt_ebb(pad$1, fmt_rest$3);
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Caml_string",
                  Arg0: match$6.Arg0,
                  Arg1: match$6.Arg1
                }
              };
            }
            break;
        case 91 :
            var match$7 = parse_char_set(str_ind, end_ind);
            var char_set = match$7[1];
            var next_ind = match$7[0];
            var match$8 = parse_literal(next_ind, next_ind, end_ind);
            var fmt_rest$4 = match$8.Arg0;
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored$3 = /* constructor */{
                tag: "Ignored_scan_char_set",
                Arg0: opt_of_pad(/* "_" */95, pad),
                Arg1: char_set
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$3,
                  Arg1: fmt_rest$4
                }
              };
            } else {
              pad_used = true;
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Scan_char_set",
                  Arg0: opt_of_pad(/* "[" */91, pad),
                  Arg1: char_set,
                  Arg2: fmt_rest$4
                }
              };
            }
            break;
        case 32 :
        case 35 :
        case 43 :
        case 45 :
        case 95 :
            exit$1 = 5;
            break;
        case 97 :
            var match$9 = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Alpha",
                Arg0: match$9.Arg0
              }
            };
            break;
        case 66 :
        case 98 :
            exit$1 = 3;
            break;
        case 99 :
            var char_format = function (fmt_rest) {
              if (ign_used[0] = true, ign) {
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Ignored_param",
                          Arg0: "Ignored_char",
                          Arg1: fmt_rest
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Char",
                          Arg0: fmt_rest
                        }
                      };
              }
            };
            var scan_format = function (fmt_rest) {
              if (ign_used[0] = true, ign) {
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Ignored_param",
                          Arg0: "Ignored_scan_next_char",
                          Arg1: fmt_rest
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Scan_next_char",
                          Arg0: fmt_rest
                        }
                      };
              }
            };
            var match$10 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$5 = match$10.Arg0;
            pad_used = true;
            var match$11 = opt_of_pad(/* "c" */99, pad);
            fmt_result = match$11 !== undefined ? (
                match$11 !== 0 ? (
                    legacy_behavior$1 ? char_format(fmt_rest$5) : invalid_format_message(str_ind, "non-zero widths are unsupported for %c conversions")
                  ) : scan_format(fmt_rest$5)
              ) : char_format(fmt_rest$5);
            break;
        case 69 :
        case 70 :
        case 71 :
        case 101 :
        case 102 :
        case 103 :
            exit$1 = 2;
            break;
        case 76 :
        case 108 :
        case 110 :
            exit$2 = 8;
            break;
        case 114 :
            var match$12 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$6 = match$12.Arg0;
            fmt_result = (ign_used[0] = true, ign) ? /* constructor */({
                  tag: "Fmt_EBB",
                  Arg0: /* constructor */{
                    tag: "Ignored_param",
                    Arg0: "Ignored_reader",
                    Arg1: fmt_rest$6
                  }
                }) : /* constructor */({
                  tag: "Fmt_EBB",
                  Arg0: /* constructor */{
                    tag: "Reader",
                    Arg0: fmt_rest$6
                  }
                });
            break;
        case 115 :
            pad_used = true;
            var pad$2 = check_no_0(symb, padprec);
            var match$13 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$7 = match$13.Arg0;
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored$4 = /* constructor */{
                tag: "Ignored_string",
                Arg0: opt_of_pad(/* "_" */95, padprec)
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$4,
                  Arg1: fmt_rest$7
                }
              };
            } else {
              var match$14 = make_padding_fmt_ebb(pad$2, fmt_rest$7);
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "String",
                  Arg0: match$14.Arg0,
                  Arg1: match$14.Arg1
                }
              };
            }
            break;
        case 116 :
            var match$15 = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Theta",
                Arg0: match$15.Arg0
              }
            };
            break;
        case 88 :
        case 100 :
        case 105 :
        case 111 :
        case 117 :
        case 120 :
            exit$2 = 7;
            break;
        case 0 :
        case 1 :
        case 2 :
        case 3 :
        case 4 :
        case 5 :
        case 6 :
        case 7 :
        case 8 :
        case 9 :
        case 10 :
        case 11 :
        case 12 :
        case 13 :
        case 14 :
        case 15 :
        case 16 :
        case 17 :
        case 18 :
        case 19 :
        case 20 :
        case 21 :
        case 22 :
        case 23 :
        case 24 :
        case 25 :
        case 26 :
        case 27 :
        case 28 :
        case 29 :
        case 30 :
        case 31 :
        case 34 :
        case 36 :
        case 38 :
        case 39 :
        case 41 :
        case 42 :
        case 46 :
        case 47 :
        case 48 :
        case 49 :
        case 50 :
        case 51 :
        case 52 :
        case 53 :
        case 54 :
        case 55 :
        case 56 :
        case 57 :
        case 58 :
        case 59 :
        case 60 :
        case 61 :
        case 62 :
        case 63 :
        case 65 :
        case 68 :
        case 72 :
        case 73 :
        case 74 :
        case 75 :
        case 77 :
        case 79 :
        case 80 :
        case 81 :
        case 82 :
        case 84 :
        case 85 :
        case 86 :
        case 87 :
        case 89 :
        case 90 :
        case 92 :
        case 93 :
        case 94 :
        case 96 :
        case 104 :
        case 106 :
        case 107 :
        case 109 :
        case 112 :
        case 113 :
        case 118 :
        case 119 :
        case 121 :
        case 122 :
            exit$1 = 6;
            break;
        case 123 :
            var sub_end$1 = search_subformat_end(str_ind, end_ind, /* "}" */125);
            var match$16 = parse_literal(str_ind, str_ind, sub_end$1);
            var beg_ind$1 = sub_end$1 + 2 | 0;
            var match$17 = parse_literal(beg_ind$1, beg_ind$1, end_ind);
            var fmt_rest$8 = match$17.Arg0;
            var sub_fmtty$1 = fmtty_of_fmt(match$16.Arg0);
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored$5 = /* constructor */{
                tag: "Ignored_format_arg",
                Arg0: opt_of_pad(/* "_" */95, pad),
                Arg1: sub_fmtty$1
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$5,
                  Arg1: fmt_rest$8
                }
              };
            } else {
              pad_used = true;
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Format_arg",
                  Arg0: opt_of_pad(/* "{" */123, pad),
                  Arg1: sub_fmtty$1,
                  Arg2: fmt_rest$8
                }
              };
            }
            break;
        
      }
    }
    switch (exit$2) {
      case 7 :
          plus_used = true;
          sharp_used = true;
          space_used = true;
          var iconv = compute_int_conv(pct_ind, str_ind, plus, sharp, space, symb);
          var match$18 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$9 = match$18.Arg0;
          if (ign_used[0] = true, ign) {
            pad_used = true;
            var ignored$6 = /* constructor */{
              tag: "Ignored_int",
              Arg0: iconv,
              Arg1: opt_of_pad(/* "_" */95, pad)
            };
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Ignored_param",
                Arg0: ignored$6,
                Arg1: fmt_rest$9
              }
            };
          } else {
            pad_used = true;
            prec_used[0] = true;
            var pad$3;
            var exit$3 = 0;
            if (typeof prec === "string" && prec === "No_precision") {
              pad$3 = pad;
            } else {
              exit$3 = 9;
            }
            if (exit$3 === 9) {
              if (typeof pad === "string") {
                pad$3 = "No_padding";
              } else if (/* XXX */pad.tag === "Lit_padding") {
                switch (pad.Arg0) {
                  case "Left" :
                  case "Right" :
                      pad$3 = pad;
                      break;
                  case "Zeros" :
                      pad$3 = legacy_behavior$1 ? /* constructor */({
                            tag: "Lit_padding",
                            Arg0: "Right",
                            Arg1: pad.Arg1
                          }) : incompatible_flag(pct_ind, str_ind, /* "0" */48, "precision");
                      break;
                  
                }
              } else {
                switch (pad.Arg0) {
                  case "Left" :
                  case "Right" :
                      pad$3 = pad;
                      break;
                  case "Zeros" :
                      pad$3 = legacy_behavior$1 ? /* constructor */({
                            tag: "Arg_padding",
                            Arg0: "Right"
                          }) : incompatible_flag(pct_ind, str_ind, /* "0" */48, "precision");
                      break;
                  
                }
              }
            }
            var match$19 = make_padprec_fmt_ebb(pad$3, (prec_used[0] = true, prec), fmt_rest$9);
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Int",
                Arg0: iconv,
                Arg1: match$19.Arg0,
                Arg2: match$19.Arg1,
                Arg3: match$19.Arg2
              }
            };
          }
          break;
      case 8 :
          if (str_ind === end_ind || !is_int_base(Caml_string.get(str, str_ind))) {
            var match$20 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$10 = match$20.Arg0;
            var counter = counter_of_char(symb);
            if (ign_used[0] = true, ign) {
              var ignored$7 = /* constructor */{
                tag: "Ignored_scan_get_counter",
                Arg0: counter
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$7,
                  Arg1: fmt_rest$10
                }
              };
            } else {
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Scan_get_counter",
                  Arg0: counter,
                  Arg1: fmt_rest$10
                }
              };
            }
          } else {
            exit$1 = 6;
          }
          break;
      
    }
    switch (exit$1) {
      case 2 :
          plus_used = true;
          space_used = true;
          var fconv = compute_float_conv(pct_ind, str_ind, plus, space, symb);
          var match$21 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$11 = match$21.Arg0;
          if (ign_used[0] = true, ign) {
            pad_used = true;
            var ignored$8 = /* constructor */{
              tag: "Ignored_float",
              Arg0: opt_of_pad(/* "_" */95, pad),
              Arg1: get_prec_opt(/* () */0)
            };
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Ignored_param",
                Arg0: ignored$8,
                Arg1: fmt_rest$11
              }
            };
          } else {
            pad_used = true;
            var match$22 = make_padprec_fmt_ebb(pad, (prec_used[0] = true, prec), fmt_rest$11);
            fmt_result = /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Float",
                Arg0: fconv,
                Arg1: match$22.Arg0,
                Arg2: match$22.Arg1,
                Arg3: match$22.Arg2
              }
            };
          }
          break;
      case 3 :
          var match$23 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$12 = match$23.Arg0;
          fmt_result = (ign_used[0] = true, ign) ? /* constructor */({
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: "Ignored_bool",
                  Arg1: fmt_rest$12
                }
              }) : /* constructor */({
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Bool",
                  Arg0: fmt_rest$12
                }
              });
          break;
      case 4 :
          var match$24 = parse_literal(str_ind, str_ind, end_ind);
          fmt_result = /* constructor */{
            tag: "Fmt_EBB",
            Arg0: /* constructor */{
              tag: "Char_literal",
              Arg0: symb,
              Arg1: match$24.Arg0
            }
          };
          break;
      case 5 :
          fmt_result = Curry._3(failwith_message(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "invalid format ",
                      Arg1: /* constructor */{
                        tag: "Caml_string",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: ": at character number ",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ", flag ",
                              Arg1: /* constructor */{
                                tag: "Caml_char",
                                Arg0: /* constructor */{
                                  tag: "String_literal",
                                  Arg0: " is only allowed after the '",
                                  Arg1: /* constructor */{
                                    tag: "Char_literal",
                                    Arg0: /* "%" */37,
                                    Arg1: /* constructor */{
                                      tag: "String_literal",
                                      Arg0: "', before padding and precision",
                                      Arg1: "End_of_format"
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "invalid format %S: at character number %d, flag %C is only allowed after the '%%', before padding and precision"
                  }), str, pct_ind, symb);
          break;
      case 6 :
          if (symb >= 108) {
            if (symb >= 111) {
              exit = 1;
            } else {
              switch (symb - 108 | 0) {
                case 0 :
                    plus_used = true;
                    sharp_used = true;
                    space_used = true;
                    var iconv$1 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, sharp, space, Caml_string.get(str, str_ind));
                    var beg_ind$2 = str_ind + 1 | 0;
                    var match$25 = parse_literal(beg_ind$2, beg_ind$2, end_ind);
                    var fmt_rest$13 = match$25.Arg0;
                    if (ign_used[0] = true, ign) {
                      pad_used = true;
                      var ignored$9 = /* constructor */{
                        tag: "Ignored_int32",
                        Arg0: iconv$1,
                        Arg1: opt_of_pad(/* "_" */95, pad)
                      };
                      fmt_result = /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Ignored_param",
                          Arg0: ignored$9,
                          Arg1: fmt_rest$13
                        }
                      };
                    } else {
                      pad_used = true;
                      var match$26 = make_padprec_fmt_ebb(pad, (prec_used[0] = true, prec), fmt_rest$13);
                      fmt_result = /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Int32",
                          Arg0: iconv$1,
                          Arg1: match$26.Arg0,
                          Arg2: match$26.Arg1,
                          Arg3: match$26.Arg2
                        }
                      };
                    }
                    break;
                case 1 :
                    exit = 1;
                    break;
                case 2 :
                    plus_used = true;
                    sharp_used = true;
                    space_used = true;
                    var iconv$2 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, sharp, space, Caml_string.get(str, str_ind));
                    var beg_ind$3 = str_ind + 1 | 0;
                    var match$27 = parse_literal(beg_ind$3, beg_ind$3, end_ind);
                    var fmt_rest$14 = match$27.Arg0;
                    if (ign_used[0] = true, ign) {
                      pad_used = true;
                      var ignored$10 = /* constructor */{
                        tag: "Ignored_nativeint",
                        Arg0: iconv$2,
                        Arg1: opt_of_pad(/* "_" */95, pad)
                      };
                      fmt_result = /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Ignored_param",
                          Arg0: ignored$10,
                          Arg1: fmt_rest$14
                        }
                      };
                    } else {
                      pad_used = true;
                      var match$28 = make_padprec_fmt_ebb(pad, (prec_used[0] = true, prec), fmt_rest$14);
                      fmt_result = /* constructor */{
                        tag: "Fmt_EBB",
                        Arg0: /* constructor */{
                          tag: "Nativeint",
                          Arg0: iconv$2,
                          Arg1: match$28.Arg0,
                          Arg2: match$28.Arg1,
                          Arg3: match$28.Arg2
                        }
                      };
                    }
                    break;
                
              }
            }
          } else if (symb !== 76) {
            exit = 1;
          } else {
            plus_used = true;
            sharp_used = true;
            space_used = true;
            var iconv$3 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, sharp, space, Caml_string.get(str, str_ind));
            var beg_ind$4 = str_ind + 1 | 0;
            var match$29 = parse_literal(beg_ind$4, beg_ind$4, end_ind);
            var fmt_rest$15 = match$29.Arg0;
            if (ign_used[0] = true, ign) {
              pad_used = true;
              var ignored$11 = /* constructor */{
                tag: "Ignored_int64",
                Arg0: iconv$3,
                Arg1: opt_of_pad(/* "_" */95, pad)
              };
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Ignored_param",
                  Arg0: ignored$11,
                  Arg1: fmt_rest$15
                }
              };
            } else {
              pad_used = true;
              var match$30 = make_padprec_fmt_ebb(pad, (prec_used[0] = true, prec), fmt_rest$15);
              fmt_result = /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Int64",
                  Arg0: iconv$3,
                  Arg1: match$30.Arg0,
                  Arg2: match$30.Arg1,
                  Arg3: match$30.Arg2
                }
              };
            }
          }
          break;
      
    }
    if (exit === 1) {
      fmt_result = Curry._3(failwith_message(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "invalid format ",
                  Arg1: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "String_literal",
                      Arg0: ": at character number ",
                      Arg1: /* constructor */{
                        tag: "Int",
                        Arg0: "Int_d",
                        Arg1: "No_padding",
                        Arg2: "No_precision",
                        Arg3: /* constructor */{
                          tag: "String_literal",
                          Arg0: ", invalid conversion \"",
                          Arg1: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* "%" */37,
                            Arg1: /* constructor */{
                              tag: "Char",
                              Arg0: /* constructor */{
                                tag: "Char_literal",
                                Arg0: /* "\"" */34,
                                Arg1: "End_of_format"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                },
                Arg1: "invalid format %S: at character number %d, invalid conversion \"%%%c\""
              }), str, str_ind - 1 | 0, symb);
    }
    if (!legacy_behavior$1) {
      if (!plus_used && plus) {
        incompatible_flag(pct_ind, str_ind, symb, "'+'");
      }
      if (!sharp_used && sharp) {
        incompatible_flag(pct_ind, str_ind, symb, "'#'");
      }
      if (!space_used && space) {
        incompatible_flag(pct_ind, str_ind, symb, "' '");
      }
      if (!pad_used && Caml_obj.caml_notequal(/* constructor */{
              tag: "Padding_EBB",
              Arg0: pad
            }, /* constructor */{
              tag: "Padding_EBB",
              Arg0: "No_padding"
            })) {
        incompatible_flag(pct_ind, str_ind, symb, "`padding'");
      }
      if (!prec_used[0] && Caml_obj.caml_notequal(/* constructor */{
              tag: "Precision_EBB",
              Arg0: prec
            }, /* constructor */{
              tag: "Precision_EBB",
              Arg0: "No_precision"
            })) {
        incompatible_flag(pct_ind, str_ind, ign ? /* "_" */95 : symb, "`precision'");
      }
      if (ign && plus) {
        incompatible_flag(pct_ind, str_ind, /* "_" */95, "'+'");
      }
      
    }
    if (!ign_used[0] && ign) {
      var exit$4 = 0;
      if (symb >= 38) {
        if (symb !== 44) {
          if (symb !== 64 || !legacy_behavior$1) {
            exit$4 = 1;
          }
          
        } else if (!legacy_behavior$1) {
          exit$4 = 1;
        }
        
      } else if (symb !== 33) {
        if (!(symb >= 37 && legacy_behavior$1)) {
          exit$4 = 1;
        }
        
      } else if (!legacy_behavior$1) {
        exit$4 = 1;
      }
      if (exit$4 === 1) {
        incompatible_flag(pct_ind, str_ind, symb, "'_'");
      }
      
    }
    return fmt_result;
  };
  var parse_flags = function (pct_ind, str_ind, end_ind, ign) {
    var zero = /* record */[/* contents */false];
    var minus = /* record */[/* contents */false];
    var plus = /* record */[/* contents */false];
    var space = /* record */[/* contents */false];
    var sharp = /* record */[/* contents */false];
    var set_flag = function (str_ind, flag) {
      if (flag[0] && !legacy_behavior$1) {
        Curry._3(failwith_message(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "invalid format ",
                    Arg1: /* constructor */{
                      tag: "Caml_string",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ": at character number ",
                        Arg1: /* constructor */{
                          tag: "Int",
                          Arg0: "Int_d",
                          Arg1: "No_padding",
                          Arg2: "No_precision",
                          Arg3: /* constructor */{
                            tag: "String_literal",
                            Arg0: ", duplicate flag ",
                            Arg1: /* constructor */{
                              tag: "Caml_char",
                              Arg0: "End_of_format"
                            }
                          }
                        }
                      }
                    }
                  },
                  Arg1: "invalid format %S: at character number %d, duplicate flag %C"
                }), str, str_ind, Caml_string.get(str, str_ind));
      }
      flag[0] = true;
      return /* () */0;
    };
    var _str_ind = str_ind;
    while(true) {
      var str_ind$1 = _str_ind;
      if (str_ind$1 === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var match = Caml_string.get(str, str_ind$1);
      switch (match) {
        case 32 :
            set_flag(str_ind$1, space);
            _str_ind = str_ind$1 + 1 | 0;
            continue ;
        case 35 :
            set_flag(str_ind$1, sharp);
            _str_ind = str_ind$1 + 1 | 0;
            continue ;
        case 43 :
            set_flag(str_ind$1, plus);
            _str_ind = str_ind$1 + 1 | 0;
            continue ;
        case 45 :
            set_flag(str_ind$1, minus);
            _str_ind = str_ind$1 + 1 | 0;
            continue ;
        case 33 :
        case 34 :
        case 36 :
        case 37 :
        case 38 :
        case 39 :
        case 40 :
        case 41 :
        case 42 :
        case 44 :
        case 46 :
        case 47 :
            break;
        case 48 :
            set_flag(str_ind$1, zero);
            _str_ind = str_ind$1 + 1 | 0;
            continue ;
        default:
          
      }
      var pct_ind$1 = pct_ind;
      var str_ind$2 = str_ind$1;
      var end_ind$1 = end_ind;
      var zero$1 = zero[0];
      var minus$1 = minus[0];
      var plus$1 = plus[0];
      var sharp$1 = sharp[0];
      var space$1 = space[0];
      var ign$1 = ign;
      if (str_ind$2 === end_ind$1) {
        invalid_format_message(end_ind$1, "unexpected end of format");
      }
      var padty = zero$1 ? (
          minus$1 ? (
              legacy_behavior$1 ? "Left" : incompatible_flag(pct_ind$1, str_ind$2, /* "-" */45, "0")
            ) : "Zeros"
        ) : (
          minus$1 ? "Left" : "Right"
        );
      var match$1 = Caml_string.get(str, str_ind$2);
      if (match$1 >= 48) {
        if (match$1 < 58) {
          var match$2 = parse_positive(str_ind$2, end_ind$1, 0);
          return parse_after_padding(pct_ind$1, match$2[0], end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, /* constructor */{
                      tag: "Lit_padding",
                      Arg0: padty,
                      Arg1: match$2[1]
                    });
        }
        
      } else if (match$1 === 42) {
        return parse_after_padding(pct_ind$1, str_ind$2 + 1 | 0, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, /* constructor */{
                    tag: "Arg_padding",
                    Arg0: padty
                  });
      }
      switch (padty) {
        case "Left" :
            if (!legacy_behavior$1) {
              invalid_format_without(str_ind$2 - 1 | 0, /* "-" */45, "padding");
            }
            return parse_after_padding(pct_ind$1, str_ind$2, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, "No_padding");
        case "Right" :
            return parse_after_padding(pct_ind$1, str_ind$2, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, "No_padding");
        case "Zeros" :
            return parse_after_padding(pct_ind$1, str_ind$2, end_ind$1, minus$1, plus$1, sharp$1, space$1, ign$1, /* constructor */{
                        tag: "Lit_padding",
                        Arg0: "Right",
                        Arg1: 0
                      });
        
      }
    };
  };
  var is_int_base = function (symb) {
    switch (symb) {
      case 89 :
      case 90 :
      case 91 :
      case 92 :
      case 93 :
      case 94 :
      case 95 :
      case 96 :
      case 97 :
      case 98 :
      case 99 :
      case 101 :
      case 102 :
      case 103 :
      case 104 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 110 :
      case 112 :
      case 113 :
      case 114 :
      case 115 :
      case 116 :
      case 118 :
      case 119 :
          return false;
      case 88 :
      case 100 :
      case 105 :
      case 111 :
      case 117 :
      case 120 :
          return true;
      default:
        return false;
    }
  };
  var counter_of_char = function (symb) {
    if (symb >= 108) {
      if (symb < 111) {
        switch (symb - 108 | 0) {
          case 0 :
              return "Line_counter";
          case 1 :
              break;
          case 2 :
              return "Char_counter";
          
        }
      }
      
    } else if (symb === 76) {
      return "Token_counter";
    }
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "camlinternalFormat.ml",
            2686,
            34
          ]
        ];
  };
  var parse_char_set = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var char_set = Bytes.make(32, /* "\000" */0);
    var add_range = function (c, c$prime) {
      for(var i = c; i <= c$prime; ++i){
        add_in_char_set(char_set, Pervasives.char_of_int(i));
      }
      return /* () */0;
    };
    var fail_single_percent = function (str_ind) {
      return Curry._2(failwith_message(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "invalid format ",
                        Arg1: /* constructor */{
                          tag: "Caml_string",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: ": '",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* "%" */37,
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "' alone is not accepted in character sets, use ",
                                Arg1: /* constructor */{
                                  tag: "Char_literal",
                                  Arg0: /* "%" */37,
                                  Arg1: /* constructor */{
                                    tag: "Char_literal",
                                    Arg0: /* "%" */37,
                                    Arg1: /* constructor */{
                                      tag: "String_literal",
                                      Arg0: " instead at position ",
                                      Arg1: /* constructor */{
                                        tag: "Int",
                                        Arg0: "Int_d",
                                        Arg1: "No_padding",
                                        Arg2: "No_precision",
                                        Arg3: /* constructor */{
                                          tag: "Char_literal",
                                          Arg0: /* "." */46,
                                          Arg1: "End_of_format"
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      },
                      Arg1: "invalid format %S: '%%' alone is not accepted in character sets, use %%%% instead at position %d."
                    }), str, str_ind);
    };
    var parse_char_set_after_char = function (_str_ind, end_ind, _c) {
      while(true) {
        var c = _c;
        var str_ind = _str_ind;
        if (str_ind === end_ind) {
          invalid_format_message(end_ind, "unexpected end of format");
        }
        var c$prime = Caml_string.get(str, str_ind);
        var exit = 0;
        if (c$prime >= 46) {
          if (c$prime !== 64) {
            if (c$prime === 93) {
              add_in_char_set(char_set, c);
              return str_ind + 1 | 0;
            }
            
          } else {
            exit = 2;
          }
        } else if (c$prime !== 37) {
          if (c$prime >= 45) {
            var str_ind$1 = str_ind + 1 | 0;
            var end_ind$1 = end_ind;
            var c$1 = c;
            if (str_ind$1 === end_ind$1) {
              invalid_format_message(end_ind$1, "unexpected end of format");
            }
            var c$prime$1 = Caml_string.get(str, str_ind$1);
            if (c$prime$1 !== 37) {
              if (c$prime$1 !== 93) {
                add_range(c$1, c$prime$1);
                return parse_char_set_content(str_ind$1 + 1 | 0, end_ind$1);
              } else {
                add_in_char_set(char_set, c$1);
                add_in_char_set(char_set, /* "-" */45);
                return str_ind$1 + 1 | 0;
              }
            } else {
              if ((str_ind$1 + 1 | 0) === end_ind$1) {
                invalid_format_message(end_ind$1, "unexpected end of format");
              }
              var c$prime$2 = Caml_string.get(str, str_ind$1 + 1 | 0);
              if (c$prime$2 !== 37 && c$prime$2 !== 64) {
                return fail_single_percent(str_ind$1);
              }
              add_range(c$1, c$prime$2);
              return parse_char_set_content(str_ind$1 + 2 | 0, end_ind$1);
            }
          }
          
        } else {
          exit = 2;
        }
        if (exit === 2 && c === /* "%" */37) {
          add_in_char_set(char_set, c$prime);
          return parse_char_set_content(str_ind + 1 | 0, end_ind);
        }
        if (c === /* "%" */37) {
          fail_single_percent(str_ind);
        }
        add_in_char_set(char_set, c);
        _c = c$prime;
        _str_ind = str_ind + 1 | 0;
        continue ;
      };
    };
    var parse_char_set_content = function (_str_ind, end_ind) {
      while(true) {
        var str_ind = _str_ind;
        if (str_ind === end_ind) {
          invalid_format_message(end_ind, "unexpected end of format");
        }
        var c = Caml_string.get(str, str_ind);
        if (c !== 45) {
          if (c !== 93) {
            return parse_char_set_after_char(str_ind + 1 | 0, end_ind, c);
          } else {
            return str_ind + 1 | 0;
          }
        } else {
          add_in_char_set(char_set, /* "-" */45);
          _str_ind = str_ind + 1 | 0;
          continue ;
        }
      };
    };
    var parse_char_set_start = function (str_ind, end_ind) {
      if (str_ind === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var c = Caml_string.get(str, str_ind);
      return parse_char_set_after_char(str_ind + 1 | 0, end_ind, c);
    };
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var match = Caml_string.get(str, str_ind);
    var match$1 = match !== 94 ? /* tuple */[
        str_ind,
        false
      ] : /* tuple */[
        str_ind + 1 | 0,
        true
      ];
    var next_ind = parse_char_set_start(match$1[0], end_ind);
    var char_set$1 = Bytes.to_string(char_set);
    return /* tuple */[
            next_ind,
            match$1[1] ? rev_char_set(char_set$1) : char_set$1
          ];
  };
  var check_open_box = function (fmt) {
    if (typeof fmt === "string" || /* XXX */fmt.tag !== "String_literal") {
      return /* () */0;
    } else {
      var tmp = fmt.Arg1;
      if (typeof tmp === "string") {
        try {
          open_box_of_string(fmt.Arg0);
          return /* () */0;
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn[0] === Caml_builtin_exceptions.failure) {
            return /* () */0;
          } else {
            throw exn;
          }
        }
      } else {
        return /* () */0;
      }
    }
  };
  var parse_tag = function (is_open_tag, str_ind, end_ind) {
    try {
      if (str_ind === end_ind) {
        throw Caml_builtin_exceptions.not_found;
      }
      var match = Caml_string.get(str, str_ind);
      if (match !== 60) {
        throw Caml_builtin_exceptions.not_found;
      }
      var ind = $$String.index_from(str, str_ind + 1 | 0, /* ">" */62);
      if (ind >= end_ind) {
        throw Caml_builtin_exceptions.not_found;
      }
      var sub_str = $$String.sub(str, str_ind, (ind - str_ind | 0) + 1 | 0);
      var beg_ind = ind + 1 | 0;
      var match$1 = parse_literal(beg_ind, beg_ind, end_ind);
      var match$2 = parse_literal(str_ind, str_ind, ind + 1 | 0);
      var sub_fmt = match$2.Arg0;
      var sub_format = /* constructor */{
        tag: "Format",
        Arg0: sub_fmt,
        Arg1: sub_str
      };
      var formatting = is_open_tag ? /* constructor */({
            tag: "Open_tag",
            Arg0: sub_format
          }) : (check_open_box(sub_fmt), /* constructor */{
            tag: "Open_box",
            Arg0: sub_format
          });
      return /* constructor */{
              tag: "Fmt_EBB",
              Arg0: /* constructor */{
                tag: "Formatting_gen",
                Arg0: formatting,
                Arg1: match$1.Arg0
              }
            };
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var match$3 = parse_literal(str_ind, str_ind, end_ind);
        var sub_format$1 = /* constructor */{
          tag: "Format",
          Arg0: "End_of_format",
          Arg1: ""
        };
        var formatting$1 = is_open_tag ? /* constructor */({
              tag: "Open_tag",
              Arg0: sub_format$1
            }) : /* constructor */({
              tag: "Open_box",
              Arg0: sub_format$1
            });
        return /* constructor */{
                tag: "Fmt_EBB",
                Arg0: /* constructor */{
                  tag: "Formatting_gen",
                  Arg0: formatting$1,
                  Arg1: match$3.Arg0
                }
              };
      } else {
        throw exn;
      }
    }
  };
  return parse_literal(0, 0, str.length);
}

function format_of_string_fmtty(str, fmtty) {
  var match = fmt_ebb_of_string(undefined, str);
  try {
    return /* constructor */{
            tag: "Format",
            Arg0: type_format(match.Arg0, fmtty),
            Arg1: str
          };
  }
  catch (exn){
    if (exn === Type_mismatch) {
      return Curry._2(failwith_message(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "bad input: format type mismatch between ",
                        Arg1: /* constructor */{
                          tag: "Caml_string",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: " and ",
                            Arg1: /* constructor */{
                              tag: "Caml_string",
                              Arg0: "No_padding",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      },
                      Arg1: "bad input: format type mismatch between %S and %S"
                    }), str, string_of_fmtty(fmtty));
    } else {
      throw exn;
    }
  }
}

function format_of_string_format(str, param) {
  var match = fmt_ebb_of_string(undefined, str);
  try {
    return /* constructor */{
            tag: "Format",
            Arg0: type_format(match.Arg0, fmtty_of_fmt(param.Arg0)),
            Arg1: str
          };
  }
  catch (exn){
    if (exn === Type_mismatch) {
      return Curry._2(failwith_message(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "bad input: format type mismatch between ",
                        Arg1: /* constructor */{
                          tag: "Caml_string",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: " and ",
                            Arg1: /* constructor */{
                              tag: "Caml_string",
                              Arg0: "No_padding",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      },
                      Arg1: "bad input: format type mismatch between %S and %S"
                    }), str, param.Arg1);
    } else {
      throw exn;
    }
  }
}

exports.is_in_char_set = is_in_char_set;
exports.rev_char_set = rev_char_set;
exports.create_char_set = create_char_set;
exports.add_in_char_set = add_in_char_set;
exports.freeze_char_set = freeze_char_set;
exports.param_format_of_ignored_format = param_format_of_ignored_format;
exports.make_printf = make_printf;
exports.output_acc = output_acc;
exports.bufput_acc = bufput_acc;
exports.strput_acc = strput_acc;
exports.type_format = type_format;
exports.fmt_ebb_of_string = fmt_ebb_of_string;
exports.format_of_string_fmtty = format_of_string_fmtty;
exports.format_of_string_format = format_of_string_format;
exports.char_of_iconv = char_of_iconv;
exports.string_of_formatting_lit = string_of_formatting_lit;
exports.string_of_formatting_gen = string_of_formatting_gen;
exports.string_of_fmtty = string_of_fmtty;
exports.string_of_fmt = string_of_fmt;
exports.open_box_of_string = open_box_of_string;
exports.symm = symm;
exports.trans = trans;
exports.recast = recast;
/* No side effect */
