

import * as Caml from "./caml.js";
import * as Char from "./char.js";
import * as Bytes from "./bytes.js";
import * as Curry from "./curry.js";
import * as $$Buffer from "./buffer.js";
import * as $$String from "./string.js";
import * as Caml_io from "./caml_io.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_bytes from "./caml_bytes.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_format from "./caml_format.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";
import * as CamlinternalFormatBasics from "./camlinternalFormatBasics.js";

function create_char_set(param) {
  return Bytes.make(32, /* '\000' */0);
}

function add_in_char_set(char_set, c) {
  var str_ind = (c >>> 3);
  var mask = (1 << (c & 7));
  return Caml_bytes.set(char_set, str_ind, Pervasives.char_of_int(Caml_bytes.get(char_set, str_ind) | mask));
}

var freeze_char_set = Bytes.to_string;

function rev_char_set(char_set) {
  var char_set$p = Bytes.make(32, /* '\000' */0);
  for(var i = 0; i <= 31; ++i){
    Caml_bytes.set(char_set$p, i, Pervasives.char_of_int(Caml_string.get(char_set, i) ^ 255));
  }
  return Caml_bytes.bytes_to_string(char_set$p);
}

function is_in_char_set(char_set, c) {
  var str_ind = (c >>> 3);
  var mask = (1 << (c & 7));
  return (Caml_string.get(char_set, str_ind) & mask) !== 0;
}

function pad_of_pad_opt(pad_opt) {
  if (pad_opt !== undefined) {
    return {
            TAG: /* Lit_padding */0,
            _0: /* Right */1,
            _1: pad_opt
          };
  } else {
    return /* No_padding */0;
  }
}

function prec_of_prec_opt(prec_opt) {
  if (prec_opt !== undefined) {
    return /* Lit_precision */{
            _0: prec_opt
          };
  } else {
    return /* No_precision */0;
  }
}

function param_format_of_ignored_format(ign, fmt) {
  if (typeof ign === "number") {
    switch (ign) {
      case /* Ignored_char */0 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Char */0,
                    _0: fmt
                  }
                };
      case /* Ignored_caml_char */1 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Caml_char */1,
                    _0: fmt
                  }
                };
      case /* Ignored_reader */2 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Reader */19,
                    _0: fmt
                  }
                };
      case /* Ignored_scan_next_char */3 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Scan_next_char */22,
                    _0: fmt
                  }
                };
      
    }
  } else {
    switch (ign.TAG | 0) {
      case /* Ignored_string */0 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* String */2,
                    _0: pad_of_pad_opt(ign._0),
                    _1: fmt
                  }
                };
      case /* Ignored_caml_string */1 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Caml_string */3,
                    _0: pad_of_pad_opt(ign._0),
                    _1: fmt
                  }
                };
      case /* Ignored_int */2 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Int */4,
                    _0: ign._0,
                    _1: pad_of_pad_opt(ign._1),
                    _2: /* No_precision */0,
                    _3: fmt
                  }
                };
      case /* Ignored_int32 */3 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Int32 */5,
                    _0: ign._0,
                    _1: pad_of_pad_opt(ign._1),
                    _2: /* No_precision */0,
                    _3: fmt
                  }
                };
      case /* Ignored_nativeint */4 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Nativeint */6,
                    _0: ign._0,
                    _1: pad_of_pad_opt(ign._1),
                    _2: /* No_precision */0,
                    _3: fmt
                  }
                };
      case /* Ignored_int64 */5 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Int64 */7,
                    _0: ign._0,
                    _1: pad_of_pad_opt(ign._1),
                    _2: /* No_precision */0,
                    _3: fmt
                  }
                };
      case /* Ignored_float */6 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Float */8,
                    _0: /* Float_f */0,
                    _1: pad_of_pad_opt(ign._0),
                    _2: prec_of_prec_opt(ign._1),
                    _3: fmt
                  }
                };
      case /* Ignored_bool */7 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Bool */9,
                    _0: pad_of_pad_opt(ign._0),
                    _1: fmt
                  }
                };
      case /* Ignored_format_arg */8 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Format_arg */13,
                    _0: ign._0,
                    _1: ign._1,
                    _2: fmt
                  }
                };
      case /* Ignored_format_subst */9 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Format_subst */14,
                    _0: ign._0,
                    _1: ign._1,
                    _2: fmt
                  }
                };
      case /* Ignored_scan_char_set */10 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: ign._0,
                    _1: ign._1,
                    _2: fmt
                  }
                };
      case /* Ignored_scan_get_counter */11 :
          return /* Param_format_EBB */{
                  _0: {
                    TAG: /* Scan_get_counter */21,
                    _0: ign._0,
                    _1: fmt
                  }
                };
      
    }
  }
}

function buffer_check_size(buf, overhead) {
  var len = buf.bytes.length;
  var min_len = buf.ind + overhead | 0;
  if (min_len <= len) {
    return ;
  }
  var new_len = Caml.caml_int_max((len << 1), min_len);
  var new_str = Caml_bytes.caml_create_bytes(new_len);
  Bytes.blit(buf.bytes, 0, new_str, 0, len);
  buf.bytes = new_str;
  
}

function buffer_add_char(buf, c) {
  buffer_check_size(buf, 1);
  Caml_bytes.set(buf.bytes, buf.ind, c);
  buf.ind = buf.ind + 1 | 0;
  
}

function buffer_add_string(buf, s) {
  var str_len = s.length;
  buffer_check_size(buf, str_len);
  $$String.blit(s, 0, buf.bytes, buf.ind, str_len);
  buf.ind = buf.ind + str_len | 0;
  
}

function buffer_contents(buf) {
  return Bytes.sub_string(buf.bytes, 0, buf.ind);
}

function char_of_iconv(iconv) {
  switch (iconv) {
    case /* Int_d */0 :
    case /* Int_pd */1 :
    case /* Int_sd */2 :
        return /* 'd' */100;
    case /* Int_i */3 :
    case /* Int_pi */4 :
    case /* Int_si */5 :
        return /* 'i' */105;
    case /* Int_x */6 :
    case /* Int_Cx */7 :
        return /* 'x' */120;
    case /* Int_X */8 :
    case /* Int_CX */9 :
        return /* 'X' */88;
    case /* Int_o */10 :
    case /* Int_Co */11 :
        return /* 'o' */111;
    case /* Int_u */12 :
        return /* 'u' */117;
    
  }
}

function char_of_fconv(fconv) {
  switch (fconv) {
    case /* Float_f */0 :
    case /* Float_pf */1 :
    case /* Float_sf */2 :
        return /* 'f' */102;
    case /* Float_e */3 :
    case /* Float_pe */4 :
    case /* Float_se */5 :
        return /* 'e' */101;
    case /* Float_E */6 :
    case /* Float_pE */7 :
    case /* Float_sE */8 :
        return /* 'E' */69;
    case /* Float_g */9 :
    case /* Float_pg */10 :
    case /* Float_sg */11 :
        return /* 'g' */103;
    case /* Float_G */12 :
    case /* Float_pG */13 :
    case /* Float_sG */14 :
        return /* 'G' */71;
    case /* Float_F */15 :
        return /* 'F' */70;
    case /* Float_h */16 :
    case /* Float_ph */17 :
    case /* Float_sh */18 :
        return /* 'h' */104;
    case /* Float_H */19 :
    case /* Float_pH */20 :
    case /* Float_sH */21 :
        return /* 'H' */72;
    
  }
}

function char_of_counter(counter) {
  switch (counter) {
    case /* Line_counter */0 :
        return /* 'l' */108;
    case /* Char_counter */1 :
        return /* 'n' */110;
    case /* Token_counter */2 :
        return /* 'N' */78;
    
  }
}

function bprint_char_set(buf, char_set) {
  var print_char = function (buf, i) {
    var c = Pervasives.char_of_int(i);
    if (c !== 37) {
      if (c !== 64) {
        return buffer_add_char(buf, c);
      } else {
        buffer_add_char(buf, /* '%' */37);
        return buffer_add_char(buf, /* '@' */64);
      }
    } else {
      buffer_add_char(buf, /* '%' */37);
      return buffer_add_char(buf, /* '%' */37);
    }
  };
  var print_out = function (set, _i) {
    while(true) {
      var i = _i;
      if (i >= 256) {
        return ;
      }
      if (is_in_char_set(set, Pervasives.char_of_int(i))) {
        var match = Pervasives.char_of_int(i);
        if (match > 93 || match < 45) {
          if (match >= 255) {
            return print_char(buf, 255);
          } else {
            return print_second(set, i + 1 | 0);
          }
        } else if (match > 92 || match < 46) {
          return print_out(set, i + 1 | 0);
        } else {
          return print_second(set, i + 1 | 0);
        }
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  var print_second = function (set, i) {
    if (is_in_char_set(set, Pervasives.char_of_int(i))) {
      var match = Pervasives.char_of_int(i);
      if (match > 93 || match < 45) {
        if (match >= 255) {
          print_char(buf, 254);
          return print_char(buf, 255);
        }
        
      } else if ((match > 92 || match < 46) && !is_in_char_set(set, Pervasives.char_of_int(i + 1 | 0))) {
        print_char(buf, i - 1 | 0);
        return print_out(set, i + 1 | 0);
      }
      if (is_in_char_set(set, Pervasives.char_of_int(i + 1 | 0))) {
        var i$1 = i - 1 | 0;
        var _j = i + 2 | 0;
        while(true) {
          var j = _j;
          if (j === 256 || !is_in_char_set(set, Pervasives.char_of_int(j))) {
            print_char(buf, i$1);
            print_char(buf, /* '-' */45);
            print_char(buf, j - 1 | 0);
            if (j < 256) {
              return print_out(set, j + 1 | 0);
            } else {
              return ;
            }
          }
          _j = j + 1 | 0;
          continue ;
        };
      } else {
        print_char(buf, i - 1 | 0);
        print_char(buf, i);
        return print_out(set, i + 2 | 0);
      }
    }
    print_char(buf, i - 1 | 0);
    return print_out(set, i + 1 | 0);
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
    if (is_alone(/* ']' */93)) {
      buffer_add_char(buf, /* ']' */93);
    }
    print_out(set, 1);
    if (is_alone(/* '-' */45)) {
      return buffer_add_char(buf, /* '-' */45);
    }
    
  };
  buffer_add_char(buf, /* '[' */91);
  print_start(is_in_char_set(char_set, /* '\000' */0) ? (buffer_add_char(buf, /* '^' */94), rev_char_set(char_set)) : char_set);
  return buffer_add_char(buf, /* ']' */93);
}

function bprint_padty(buf, padty) {
  switch (padty) {
    case /* Left */0 :
        return buffer_add_char(buf, /* '-' */45);
    case /* Right */1 :
        return ;
    case /* Zeros */2 :
        return buffer_add_char(buf, /* '0' */48);
    
  }
}

function bprint_ignored_flag(buf, ign_flag) {
  if (ign_flag) {
    return buffer_add_char(buf, /* '_' */95);
  }
  
}

function bprint_pad_opt(buf, pad_opt) {
  if (pad_opt !== undefined) {
    return buffer_add_string(buf, String(pad_opt));
  }
  
}

function bprint_padding(buf, pad) {
  if (typeof pad === "number") {
    return ;
  }
  if (pad.TAG === /* Lit_padding */0) {
    bprint_padty(buf, pad._0);
    return buffer_add_string(buf, String(pad._1));
  }
  bprint_padty(buf, pad._0);
  return buffer_add_char(buf, /* '*' */42);
}

function bprint_precision(buf, prec) {
  if (typeof prec === "number") {
    if (prec !== 0) {
      return buffer_add_string(buf, ".*");
    } else {
      return ;
    }
  } else {
    buffer_add_char(buf, /* '.' */46);
    return buffer_add_string(buf, String(prec._0));
  }
}

function bprint_iconv_flag(buf, iconv) {
  switch (iconv) {
    case /* Int_pd */1 :
    case /* Int_pi */4 :
        return buffer_add_char(buf, /* '+' */43);
    case /* Int_sd */2 :
    case /* Int_si */5 :
        return buffer_add_char(buf, /* ' ' */32);
    case /* Int_Cx */7 :
    case /* Int_CX */9 :
    case /* Int_Co */11 :
        return buffer_add_char(buf, /* '#' */35);
    case /* Int_d */0 :
    case /* Int_i */3 :
    case /* Int_x */6 :
    case /* Int_X */8 :
    case /* Int_o */10 :
    case /* Int_u */12 :
        return ;
    
  }
}

function bprint_int_fmt(buf, ign_flag, iconv, pad, prec) {
  buffer_add_char(buf, /* '%' */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_iconv_flag(buf, iconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  return buffer_add_char(buf, char_of_iconv(iconv));
}

function bprint_altint_fmt(buf, ign_flag, iconv, pad, prec, c) {
  buffer_add_char(buf, /* '%' */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_iconv_flag(buf, iconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  buffer_add_char(buf, c);
  return buffer_add_char(buf, char_of_iconv(iconv));
}

function bprint_fconv_flag(buf, fconv) {
  switch (fconv) {
    case /* Float_f */0 :
    case /* Float_e */3 :
    case /* Float_E */6 :
    case /* Float_g */9 :
    case /* Float_G */12 :
    case /* Float_F */15 :
    case /* Float_h */16 :
    case /* Float_H */19 :
        return ;
    case /* Float_pf */1 :
    case /* Float_pe */4 :
    case /* Float_pE */7 :
    case /* Float_pg */10 :
    case /* Float_pG */13 :
    case /* Float_ph */17 :
    case /* Float_pH */20 :
        return buffer_add_char(buf, /* '+' */43);
    case /* Float_sf */2 :
    case /* Float_se */5 :
    case /* Float_sE */8 :
    case /* Float_sg */11 :
    case /* Float_sG */14 :
    case /* Float_sh */18 :
    case /* Float_sH */21 :
        return buffer_add_char(buf, /* ' ' */32);
    
  }
}

function bprint_float_fmt(buf, ign_flag, fconv, pad, prec) {
  buffer_add_char(buf, /* '%' */37);
  bprint_ignored_flag(buf, ign_flag);
  bprint_fconv_flag(buf, fconv);
  bprint_padding(buf, pad);
  bprint_precision(buf, prec);
  return buffer_add_char(buf, char_of_fconv(fconv));
}

function string_of_formatting_lit(formatting_lit) {
  if (typeof formatting_lit === "number") {
    switch (formatting_lit) {
      case /* Close_box */0 :
          return "@]";
      case /* Close_tag */1 :
          return "@}";
      case /* FFlush */2 :
          return "@?";
      case /* Force_newline */3 :
          return "@\n";
      case /* Flush_newline */4 :
          return "@.";
      case /* Escaped_at */5 :
          return "@@";
      case /* Escaped_percent */6 :
          return "@%";
      
    }
  } else {
    switch (formatting_lit.TAG | 0) {
      case /* Break */0 :
      case /* Magic_size */1 :
          return formatting_lit._0;
      case /* Scan_indic */2 :
          return "@" + Caml_string.make(1, formatting_lit._0);
      
    }
  }
}

function string_of_formatting_gen(formatting_gen) {
  return formatting_gen._0._1;
}

function bprint_char_literal(buf, chr) {
  if (chr !== 37) {
    return buffer_add_char(buf, chr);
  } else {
    return buffer_add_string(buf, "%%");
  }
}

function bprint_string_literal(buf, str) {
  for(var i = 0 ,i_finish = str.length; i < i_finish; ++i){
    bprint_char_literal(buf, Caml_string.get(str, i));
  }
  
}

function bprint_fmtty(buf, _fmtty) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "number") {
      return ;
    }
    switch (fmtty.TAG | 0) {
      case /* Char_ty */0 :
          buffer_add_string(buf, "%c");
          _fmtty = fmtty._0;
          continue ;
      case /* String_ty */1 :
          buffer_add_string(buf, "%s");
          _fmtty = fmtty._0;
          continue ;
      case /* Int_ty */2 :
          buffer_add_string(buf, "%i");
          _fmtty = fmtty._0;
          continue ;
      case /* Int32_ty */3 :
          buffer_add_string(buf, "%li");
          _fmtty = fmtty._0;
          continue ;
      case /* Nativeint_ty */4 :
          buffer_add_string(buf, "%ni");
          _fmtty = fmtty._0;
          continue ;
      case /* Int64_ty */5 :
          buffer_add_string(buf, "%Li");
          _fmtty = fmtty._0;
          continue ;
      case /* Float_ty */6 :
          buffer_add_string(buf, "%f");
          _fmtty = fmtty._0;
          continue ;
      case /* Bool_ty */7 :
          buffer_add_string(buf, "%B");
          _fmtty = fmtty._0;
          continue ;
      case /* Format_arg_ty */8 :
          buffer_add_string(buf, "%{");
          bprint_fmtty(buf, fmtty._0);
          buffer_add_string(buf, "%}");
          _fmtty = fmtty._1;
          continue ;
      case /* Format_subst_ty */9 :
          buffer_add_string(buf, "%(");
          bprint_fmtty(buf, fmtty._0);
          buffer_add_string(buf, "%)");
          _fmtty = fmtty._2;
          continue ;
      case /* Alpha_ty */10 :
          buffer_add_string(buf, "%a");
          _fmtty = fmtty._0;
          continue ;
      case /* Theta_ty */11 :
          buffer_add_string(buf, "%t");
          _fmtty = fmtty._0;
          continue ;
      case /* Any_ty */12 :
          buffer_add_string(buf, "%?");
          _fmtty = fmtty._0;
          continue ;
      case /* Reader_ty */13 :
          buffer_add_string(buf, "%r");
          _fmtty = fmtty._0;
          continue ;
      case /* Ignored_reader_ty */14 :
          buffer_add_string(buf, "%_r");
          _fmtty = fmtty._0;
          continue ;
      
    }
  };
}

function int_of_custom_arity(x) {
  if (x) {
    return 1 + int_of_custom_arity(x._0) | 0;
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
    if (typeof fmt$1 === "number") {
      return ;
    }
    switch (fmt$1.TAG | 0) {
      case /* Char */0 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, /* 'c' */99);
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* Caml_char */1 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, /* 'C' */67);
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* String */2 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_padding(buf, fmt$1._0);
          buffer_add_char(buf, /* 's' */115);
          _ign_flag = false;
          _fmt = fmt$1._1;
          continue ;
      case /* Caml_string */3 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_padding(buf, fmt$1._0);
          buffer_add_char(buf, /* 'S' */83);
          _ign_flag = false;
          _fmt = fmt$1._1;
          continue ;
      case /* Int */4 :
          bprint_int_fmt(buf, ign_flag, fmt$1._0, fmt$1._1, fmt$1._2);
          _ign_flag = false;
          _fmt = fmt$1._3;
          continue ;
      case /* Int32 */5 :
          bprint_altint_fmt(buf, ign_flag, fmt$1._0, fmt$1._1, fmt$1._2, /* 'l' */108);
          _ign_flag = false;
          _fmt = fmt$1._3;
          continue ;
      case /* Nativeint */6 :
          bprint_altint_fmt(buf, ign_flag, fmt$1._0, fmt$1._1, fmt$1._2, /* 'n' */110);
          _ign_flag = false;
          _fmt = fmt$1._3;
          continue ;
      case /* Int64 */7 :
          bprint_altint_fmt(buf, ign_flag, fmt$1._0, fmt$1._1, fmt$1._2, /* 'L' */76);
          _ign_flag = false;
          _fmt = fmt$1._3;
          continue ;
      case /* Float */8 :
          bprint_float_fmt(buf, ign_flag, fmt$1._0, fmt$1._1, fmt$1._2);
          _ign_flag = false;
          _fmt = fmt$1._3;
          continue ;
      case /* Bool */9 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_padding(buf, fmt$1._0);
          buffer_add_char(buf, /* 'B' */66);
          _ign_flag = false;
          _fmt = fmt$1._1;
          continue ;
      case /* Flush */10 :
          buffer_add_string(buf, "%!");
          _fmt = fmt$1._0;
          continue ;
      case /* String_literal */11 :
          bprint_string_literal(buf, fmt$1._0);
          _fmt = fmt$1._1;
          continue ;
      case /* Char_literal */12 :
          bprint_char_literal(buf, fmt$1._0);
          _fmt = fmt$1._1;
          continue ;
      case /* Format_arg */13 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_pad_opt(buf, fmt$1._0);
          buffer_add_char(buf, /* '{' */123);
          bprint_fmtty(buf, fmt$1._1);
          buffer_add_char(buf, /* '%' */37);
          buffer_add_char(buf, /* '}' */125);
          _ign_flag = false;
          _fmt = fmt$1._2;
          continue ;
      case /* Format_subst */14 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_pad_opt(buf, fmt$1._0);
          buffer_add_char(buf, /* '(' */40);
          bprint_fmtty(buf, fmt$1._1);
          buffer_add_char(buf, /* '%' */37);
          buffer_add_char(buf, /* ')' */41);
          _ign_flag = false;
          _fmt = fmt$1._2;
          continue ;
      case /* Alpha */15 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, /* 'a' */97);
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* Theta */16 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, /* 't' */116);
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* Formatting_lit */17 :
          bprint_string_literal(buf, string_of_formatting_lit(fmt$1._0));
          _fmt = fmt$1._1;
          continue ;
      case /* Formatting_gen */18 :
          bprint_string_literal(buf, "@{");
          bprint_string_literal(buf, string_of_formatting_gen(fmt$1._0));
          _fmt = fmt$1._1;
          continue ;
      case /* Reader */19 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, /* 'r' */114);
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* Scan_char_set */20 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_pad_opt(buf, fmt$1._0);
          bprint_char_set(buf, fmt$1._1);
          _ign_flag = false;
          _fmt = fmt$1._2;
          continue ;
      case /* Scan_get_counter */21 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          buffer_add_char(buf, char_of_counter(fmt$1._0));
          _ign_flag = false;
          _fmt = fmt$1._1;
          continue ;
      case /* Scan_next_char */22 :
          buffer_add_char(buf, /* '%' */37);
          bprint_ignored_flag(buf, ign_flag);
          bprint_string_literal(buf, "0c");
          _ign_flag = false;
          _fmt = fmt$1._0;
          continue ;
      case /* Ignored_param */23 :
          var fmt$p = param_format_of_ignored_format(fmt$1._0, fmt$1._1);
          _ign_flag = true;
          _fmt = fmt$p._0;
          continue ;
      case /* Custom */24 :
          for(var _i = 1 ,_i_finish = int_of_custom_arity(fmt$1._0); _i <= _i_finish; ++_i){
            buffer_add_char(buf, /* '%' */37);
            bprint_ignored_flag(buf, ign_flag);
            buffer_add_char(buf, /* '?' */63);
          }
          _ign_flag = false;
          _fmt = fmt$1._2;
          continue ;
      
    }
  };
}

function string_of_fmt(fmt) {
  var buf = {
    ind: 0,
    bytes: Caml_bytes.caml_create_bytes(16)
  };
  bprint_fmt(buf, fmt);
  return buffer_contents(buf);
}

function symm(rest) {
  if (typeof rest === "number") {
    return /* End_of_fmtty */0;
  }
  switch (rest.TAG | 0) {
    case /* Char_ty */0 :
        return {
                TAG: /* Char_ty */0,
                _0: symm(rest._0)
              };
    case /* String_ty */1 :
        return {
                TAG: /* String_ty */1,
                _0: symm(rest._0)
              };
    case /* Int_ty */2 :
        return {
                TAG: /* Int_ty */2,
                _0: symm(rest._0)
              };
    case /* Int32_ty */3 :
        return {
                TAG: /* Int32_ty */3,
                _0: symm(rest._0)
              };
    case /* Nativeint_ty */4 :
        return {
                TAG: /* Nativeint_ty */4,
                _0: symm(rest._0)
              };
    case /* Int64_ty */5 :
        return {
                TAG: /* Int64_ty */5,
                _0: symm(rest._0)
              };
    case /* Float_ty */6 :
        return {
                TAG: /* Float_ty */6,
                _0: symm(rest._0)
              };
    case /* Bool_ty */7 :
        return {
                TAG: /* Bool_ty */7,
                _0: symm(rest._0)
              };
    case /* Format_arg_ty */8 :
        return {
                TAG: /* Format_arg_ty */8,
                _0: rest._0,
                _1: symm(rest._1)
              };
    case /* Format_subst_ty */9 :
        return {
                TAG: /* Format_subst_ty */9,
                _0: rest._1,
                _1: rest._0,
                _2: symm(rest._2)
              };
    case /* Alpha_ty */10 :
        return {
                TAG: /* Alpha_ty */10,
                _0: symm(rest._0)
              };
    case /* Theta_ty */11 :
        return {
                TAG: /* Theta_ty */11,
                _0: symm(rest._0)
              };
    case /* Any_ty */12 :
        return {
                TAG: /* Any_ty */12,
                _0: symm(rest._0)
              };
    case /* Reader_ty */13 :
        return {
                TAG: /* Reader_ty */13,
                _0: symm(rest._0)
              };
    case /* Ignored_reader_ty */14 :
        return {
                TAG: /* Ignored_reader_ty */14,
                _0: symm(rest._0)
              };
    
  }
}

function fmtty_rel_det(rest) {
  if (typeof rest === "number") {
    return [
            (function (param) {
                return /* Refl */0;
              }),
            (function (param) {
                return /* Refl */0;
              }),
            (function (param) {
                return /* Refl */0;
              }),
            (function (param) {
                return /* Refl */0;
              })
          ];
  }
  switch (rest.TAG | 0) {
    case /* Char_ty */0 :
        var match = fmtty_rel_det(rest._0);
        var af = match[1];
        var fa = match[0];
        return [
                (function (param) {
                    Curry._1(fa, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af, /* Refl */0);
                    return /* Refl */0;
                  }),
                match[2],
                match[3]
              ];
    case /* String_ty */1 :
        var match$1 = fmtty_rel_det(rest._0);
        var af$1 = match$1[1];
        var fa$1 = match$1[0];
        return [
                (function (param) {
                    Curry._1(fa$1, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$1, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$1[2],
                match$1[3]
              ];
    case /* Int_ty */2 :
        var match$2 = fmtty_rel_det(rest._0);
        var af$2 = match$2[1];
        var fa$2 = match$2[0];
        return [
                (function (param) {
                    Curry._1(fa$2, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$2, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$2[2],
                match$2[3]
              ];
    case /* Int32_ty */3 :
        var match$3 = fmtty_rel_det(rest._0);
        var af$3 = match$3[1];
        var fa$3 = match$3[0];
        return [
                (function (param) {
                    Curry._1(fa$3, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$3, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$3[2],
                match$3[3]
              ];
    case /* Nativeint_ty */4 :
        var match$4 = fmtty_rel_det(rest._0);
        var af$4 = match$4[1];
        var fa$4 = match$4[0];
        return [
                (function (param) {
                    Curry._1(fa$4, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$4, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$4[2],
                match$4[3]
              ];
    case /* Int64_ty */5 :
        var match$5 = fmtty_rel_det(rest._0);
        var af$5 = match$5[1];
        var fa$5 = match$5[0];
        return [
                (function (param) {
                    Curry._1(fa$5, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$5, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$5[2],
                match$5[3]
              ];
    case /* Float_ty */6 :
        var match$6 = fmtty_rel_det(rest._0);
        var af$6 = match$6[1];
        var fa$6 = match$6[0];
        return [
                (function (param) {
                    Curry._1(fa$6, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$6, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$6[2],
                match$6[3]
              ];
    case /* Bool_ty */7 :
        var match$7 = fmtty_rel_det(rest._0);
        var af$7 = match$7[1];
        var fa$7 = match$7[0];
        return [
                (function (param) {
                    Curry._1(fa$7, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$7, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$7[2],
                match$7[3]
              ];
    case /* Format_arg_ty */8 :
        var match$8 = fmtty_rel_det(rest._1);
        var af$8 = match$8[1];
        var fa$8 = match$8[0];
        return [
                (function (param) {
                    Curry._1(fa$8, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$8, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$8[2],
                match$8[3]
              ];
    case /* Format_subst_ty */9 :
        var match$9 = fmtty_rel_det(rest._2);
        var de = match$9[3];
        var ed = match$9[2];
        var af$9 = match$9[1];
        var fa$9 = match$9[0];
        var ty = trans(symm(rest._0), rest._1);
        var match$10 = fmtty_rel_det(ty);
        var jd = match$10[3];
        var dj = match$10[2];
        var ga = match$10[1];
        var ag = match$10[0];
        return [
                (function (param) {
                    Curry._1(fa$9, /* Refl */0);
                    Curry._1(ag, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(ga, /* Refl */0);
                    Curry._1(af$9, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(ed, /* Refl */0);
                    Curry._1(dj, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(jd, /* Refl */0);
                    Curry._1(de, /* Refl */0);
                    return /* Refl */0;
                  })
              ];
    case /* Alpha_ty */10 :
        var match$11 = fmtty_rel_det(rest._0);
        var af$10 = match$11[1];
        var fa$10 = match$11[0];
        return [
                (function (param) {
                    Curry._1(fa$10, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$10, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$11[2],
                match$11[3]
              ];
    case /* Theta_ty */11 :
        var match$12 = fmtty_rel_det(rest._0);
        var af$11 = match$12[1];
        var fa$11 = match$12[0];
        return [
                (function (param) {
                    Curry._1(fa$11, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$11, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$12[2],
                match$12[3]
              ];
    case /* Any_ty */12 :
        var match$13 = fmtty_rel_det(rest._0);
        var af$12 = match$13[1];
        var fa$12 = match$13[0];
        return [
                (function (param) {
                    Curry._1(fa$12, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$12, /* Refl */0);
                    return /* Refl */0;
                  }),
                match$13[2],
                match$13[3]
              ];
    case /* Reader_ty */13 :
        var match$14 = fmtty_rel_det(rest._0);
        var de$1 = match$14[3];
        var ed$1 = match$14[2];
        var af$13 = match$14[1];
        var fa$13 = match$14[0];
        return [
                (function (param) {
                    Curry._1(fa$13, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$13, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(ed$1, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(de$1, /* Refl */0);
                    return /* Refl */0;
                  })
              ];
    case /* Ignored_reader_ty */14 :
        var match$15 = fmtty_rel_det(rest._0);
        var de$2 = match$15[3];
        var ed$2 = match$15[2];
        var af$14 = match$15[1];
        var fa$14 = match$15[0];
        return [
                (function (param) {
                    Curry._1(fa$14, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(af$14, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(ed$2, /* Refl */0);
                    return /* Refl */0;
                  }),
                (function (param) {
                    Curry._1(de$2, /* Refl */0);
                    return /* Refl */0;
                  })
              ];
    
  }
}

function trans(ty1, ty2) {
  var exit = 0;
  if (typeof ty1 === "number") {
    if (typeof ty2 === "number") {
      return /* End_of_fmtty */0;
    }
    switch (ty2.TAG | 0) {
      case /* Format_arg_ty */8 :
          exit = 6;
          break;
      case /* Format_subst_ty */9 :
          exit = 7;
          break;
      case /* Alpha_ty */10 :
          exit = 1;
          break;
      case /* Theta_ty */11 :
          exit = 2;
          break;
      case /* Any_ty */12 :
          exit = 3;
          break;
      case /* Reader_ty */13 :
          exit = 4;
          break;
      case /* Ignored_reader_ty */14 :
          exit = 5;
          break;
      default:
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                846,
                23
              ],
              Error: new Error()
            };
    }
  } else {
    switch (ty1.TAG | 0) {
      case /* Char_ty */0 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Char_ty */0 :
                  return {
                          TAG: /* Char_ty */0,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* String_ty */1 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* String_ty */1 :
                  return {
                          TAG: /* String_ty */1,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Int_ty */2 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Int_ty */2 :
                  return {
                          TAG: /* Int_ty */2,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Int32_ty */3 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Int32_ty */3 :
                  return {
                          TAG: /* Int32_ty */3,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Nativeint_ty */4 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Nativeint_ty */4 :
                  return {
                          TAG: /* Nativeint_ty */4,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Int64_ty */5 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Int64_ty */5 :
                  return {
                          TAG: /* Int64_ty */5,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Float_ty */6 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Float_ty */6 :
                  return {
                          TAG: /* Float_ty */6,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Bool_ty */7 :
          if (typeof ty2 === "number") {
            exit = 8;
          } else {
            switch (ty2.TAG | 0) {
              case /* Bool_ty */7 :
                  return {
                          TAG: /* Bool_ty */7,
                          _0: trans(ty1._0, ty2._0)
                        };
              case /* Format_arg_ty */8 :
                  exit = 6;
                  break;
              case /* Format_subst_ty */9 :
                  exit = 7;
                  break;
              case /* Alpha_ty */10 :
                  exit = 1;
                  break;
              case /* Theta_ty */11 :
                  exit = 2;
                  break;
              case /* Any_ty */12 :
                  exit = 3;
                  break;
              case /* Reader_ty */13 :
                  exit = 4;
                  break;
              case /* Ignored_reader_ty */14 :
                  exit = 5;
                  break;
              
            }
          }
          break;
      case /* Format_arg_ty */8 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    832,
                    26
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Format_arg_ty */8 :
                return {
                        TAG: /* Format_arg_ty */8,
                        _0: trans(ty1._0, ty2._0),
                        _1: trans(ty1._1, ty2._1)
                      };
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                exit = 2;
                break;
            case /* Any_ty */12 :
                exit = 3;
                break;
            case /* Reader_ty */13 :
                exit = 4;
                break;
            case /* Ignored_reader_ty */14 :
                exit = 5;
                break;
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      832,
                      26
                    ],
                    Error: new Error()
                  };
          }
          break;
      case /* Format_subst_ty */9 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    842,
                    28
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Format_arg_ty */8 :
                exit = 6;
                break;
            case /* Format_subst_ty */9 :
                var ty = trans(symm(ty1._1), ty2._0);
                var match = fmtty_rel_det(ty);
                Curry._1(match[1], /* Refl */0);
                Curry._1(match[3], /* Refl */0);
                return {
                        TAG: /* Format_subst_ty */9,
                        _0: ty1._0,
                        _1: ty2._1,
                        _2: trans(ty1._2, ty2._2)
                      };
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                exit = 2;
                break;
            case /* Any_ty */12 :
                exit = 3;
                break;
            case /* Reader_ty */13 :
                exit = 4;
                break;
            case /* Ignored_reader_ty */14 :
                exit = 5;
                break;
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      842,
                      28
                    ],
                    Error: new Error()
                  };
          }
          break;
      case /* Alpha_ty */10 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    810,
                    21
                  ],
                  Error: new Error()
                };
          }
          if (ty2.TAG === /* Alpha_ty */10) {
            return {
                    TAG: /* Alpha_ty */10,
                    _0: trans(ty1._0, ty2._0)
                  };
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "camlinternalFormat.ml",
                  810,
                  21
                ],
                Error: new Error()
              };
      case /* Theta_ty */11 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    814,
                    21
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                return {
                        TAG: /* Theta_ty */11,
                        _0: trans(ty1._0, ty2._0)
                      };
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      814,
                      21
                    ],
                    Error: new Error()
                  };
          }
          break;
      case /* Any_ty */12 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    818,
                    19
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                exit = 2;
                break;
            case /* Any_ty */12 :
                return {
                        TAG: /* Any_ty */12,
                        _0: trans(ty1._0, ty2._0)
                      };
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      818,
                      19
                    ],
                    Error: new Error()
                  };
          }
          break;
      case /* Reader_ty */13 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    822,
                    22
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                exit = 2;
                break;
            case /* Any_ty */12 :
                exit = 3;
                break;
            case /* Reader_ty */13 :
                return {
                        TAG: /* Reader_ty */13,
                        _0: trans(ty1._0, ty2._0)
                      };
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      822,
                      22
                    ],
                    Error: new Error()
                  };
          }
          break;
      case /* Ignored_reader_ty */14 :
          if (typeof ty2 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    827,
                    30
                  ],
                  Error: new Error()
                };
          }
          switch (ty2.TAG | 0) {
            case /* Alpha_ty */10 :
                exit = 1;
                break;
            case /* Theta_ty */11 :
                exit = 2;
                break;
            case /* Any_ty */12 :
                exit = 3;
                break;
            case /* Reader_ty */13 :
                exit = 4;
                break;
            case /* Ignored_reader_ty */14 :
                return {
                        TAG: /* Ignored_reader_ty */14,
                        _0: trans(ty1._0, ty2._0)
                      };
            default:
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "camlinternalFormat.ml",
                      827,
                      30
                    ],
                    Error: new Error()
                  };
          }
          break;
      
    }
  }
  switch (exit) {
    case 1 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                811,
                21
              ],
              Error: new Error()
            };
    case 2 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                815,
                21
              ],
              Error: new Error()
            };
    case 3 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                819,
                19
              ],
              Error: new Error()
            };
    case 4 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                823,
                22
              ],
              Error: new Error()
            };
    case 5 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                828,
                30
              ],
              Error: new Error()
            };
    case 6 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                833,
                26
              ],
              Error: new Error()
            };
    case 7 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                843,
                28
              ],
              Error: new Error()
            };
    case 8 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                847,
                23
              ],
              Error: new Error()
            };
    
  }
}

function fmtty_of_formatting_gen(formatting_gen) {
  return fmtty_of_fmt(formatting_gen._0._0);
}

function fmtty_of_fmt(_fmtty) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "number") {
      return /* End_of_fmtty */0;
    }
    switch (fmtty.TAG | 0) {
      case /* String */2 :
      case /* Caml_string */3 :
          break;
      case /* Int */4 :
          var ty_rest = fmtty_of_fmt(fmtty._3);
          var prec_ty = fmtty_of_precision_fmtty(fmtty._2, {
                TAG: /* Int_ty */2,
                _0: ty_rest
              });
          return fmtty_of_padding_fmtty(fmtty._1, prec_ty);
      case /* Int32 */5 :
          var ty_rest$1 = fmtty_of_fmt(fmtty._3);
          var prec_ty$1 = fmtty_of_precision_fmtty(fmtty._2, {
                TAG: /* Int32_ty */3,
                _0: ty_rest$1
              });
          return fmtty_of_padding_fmtty(fmtty._1, prec_ty$1);
      case /* Nativeint */6 :
          var ty_rest$2 = fmtty_of_fmt(fmtty._3);
          var prec_ty$2 = fmtty_of_precision_fmtty(fmtty._2, {
                TAG: /* Nativeint_ty */4,
                _0: ty_rest$2
              });
          return fmtty_of_padding_fmtty(fmtty._1, prec_ty$2);
      case /* Int64 */7 :
          var ty_rest$3 = fmtty_of_fmt(fmtty._3);
          var prec_ty$3 = fmtty_of_precision_fmtty(fmtty._2, {
                TAG: /* Int64_ty */5,
                _0: ty_rest$3
              });
          return fmtty_of_padding_fmtty(fmtty._1, prec_ty$3);
      case /* Float */8 :
          var ty_rest$4 = fmtty_of_fmt(fmtty._3);
          var prec_ty$4 = fmtty_of_precision_fmtty(fmtty._2, {
                TAG: /* Float_ty */6,
                _0: ty_rest$4
              });
          return fmtty_of_padding_fmtty(fmtty._1, prec_ty$4);
      case /* Bool */9 :
          return fmtty_of_padding_fmtty(fmtty._0, {
                      TAG: /* Bool_ty */7,
                      _0: fmtty_of_fmt(fmtty._1)
                    });
      case /* Flush */10 :
          _fmtty = fmtty._0;
          continue ;
      case /* Format_arg */13 :
          return {
                  TAG: /* Format_arg_ty */8,
                  _0: fmtty._1,
                  _1: fmtty_of_fmt(fmtty._2)
                };
      case /* Format_subst */14 :
          var ty = fmtty._1;
          return {
                  TAG: /* Format_subst_ty */9,
                  _0: ty,
                  _1: ty,
                  _2: fmtty_of_fmt(fmtty._2)
                };
      case /* Alpha */15 :
          return {
                  TAG: /* Alpha_ty */10,
                  _0: fmtty_of_fmt(fmtty._0)
                };
      case /* Theta */16 :
          return {
                  TAG: /* Theta_ty */11,
                  _0: fmtty_of_fmt(fmtty._0)
                };
      case /* String_literal */11 :
      case /* Char_literal */12 :
      case /* Formatting_lit */17 :
          _fmtty = fmtty._1;
          continue ;
      case /* Formatting_gen */18 :
          return CamlinternalFormatBasics.concat_fmtty(fmtty_of_formatting_gen(fmtty._0), fmtty_of_fmt(fmtty._1));
      case /* Reader */19 :
          return {
                  TAG: /* Reader_ty */13,
                  _0: fmtty_of_fmt(fmtty._0)
                };
      case /* Scan_char_set */20 :
          return {
                  TAG: /* String_ty */1,
                  _0: fmtty_of_fmt(fmtty._2)
                };
      case /* Scan_get_counter */21 :
          return {
                  TAG: /* Int_ty */2,
                  _0: fmtty_of_fmt(fmtty._1)
                };
      case /* Ignored_param */23 :
          var ign = fmtty._0;
          var fmt = fmtty._1;
          if (typeof ign === "number") {
            if (ign === /* Ignored_reader */2) {
              return {
                      TAG: /* Ignored_reader_ty */14,
                      _0: fmtty_of_fmt(fmt)
                    };
            } else {
              return fmtty_of_fmt(fmt);
            }
          } else if (ign.TAG === /* Ignored_format_subst */9) {
            return CamlinternalFormatBasics.concat_fmtty(ign._1, fmtty_of_fmt(fmt));
          } else {
            return fmtty_of_fmt(fmt);
          }
      case /* Custom */24 :
          return fmtty_of_custom(fmtty._0, fmtty_of_fmt(fmtty._2));
      default:
        return {
                TAG: /* Char_ty */0,
                _0: fmtty_of_fmt(fmtty._0)
              };
    }
    return fmtty_of_padding_fmtty(fmtty._0, {
                TAG: /* String_ty */1,
                _0: fmtty_of_fmt(fmtty._1)
              });
  };
}

function fmtty_of_custom(arity, fmtty) {
  if (arity) {
    return {
            TAG: /* Any_ty */12,
            _0: fmtty_of_custom(arity._0, fmtty)
          };
  } else {
    return fmtty;
  }
}

function fmtty_of_padding_fmtty(pad, fmtty) {
  if (typeof pad === "number" || pad.TAG === /* Lit_padding */0) {
    return fmtty;
  } else {
    return {
            TAG: /* Int_ty */2,
            _0: fmtty
          };
  }
}

function fmtty_of_precision_fmtty(prec, fmtty) {
  if (typeof prec === "number" && prec !== 0) {
    return {
            TAG: /* Int_ty */2,
            _0: fmtty
          };
  } else {
    return fmtty;
  }
}

var Type_mismatch = /* @__PURE__ */Caml_exceptions.create("CamlinternalFormat.Type_mismatch");

function type_padding(pad, fmtty) {
  if (typeof pad === "number") {
    return /* Padding_fmtty_EBB */{
            _0: /* No_padding */0,
            _1: fmtty
          };
  }
  if (pad.TAG === /* Lit_padding */0) {
    return /* Padding_fmtty_EBB */{
            _0: {
              TAG: /* Lit_padding */0,
              _0: pad._0,
              _1: pad._1
            },
            _1: fmtty
          };
  }
  if (typeof fmtty === "number") {
    throw {
          RE_EXN_ID: Type_mismatch,
          Error: new Error()
        };
  }
  if (fmtty.TAG === /* Int_ty */2) {
    return /* Padding_fmtty_EBB */{
            _0: {
              TAG: /* Arg_padding */1,
              _0: pad._0
            },
            _1: fmtty._0
          };
  }
  throw {
        RE_EXN_ID: Type_mismatch,
        Error: new Error()
      };
}

function type_padprec(pad, prec, fmtty) {
  var match = type_padding(pad, fmtty);
  if (typeof prec !== "number") {
    return /* Padprec_fmtty_EBB */{
            _0: match._0,
            _1: /* Lit_precision */{
              _0: prec._0
            },
            _2: match._1
          };
  }
  if (prec === 0) {
    return /* Padprec_fmtty_EBB */{
            _0: match._0,
            _1: /* No_precision */0,
            _2: match._1
          };
  }
  var rest = match._1;
  if (typeof rest === "number") {
    throw {
          RE_EXN_ID: Type_mismatch,
          Error: new Error()
        };
  }
  if (rest.TAG === /* Int_ty */2) {
    return /* Padprec_fmtty_EBB */{
            _0: match._0,
            _1: /* Arg_precision */1,
            _2: rest._0
          };
  }
  throw {
        RE_EXN_ID: Type_mismatch,
        Error: new Error()
      };
}

function type_ignored_format_substitution(sub_fmtty, fmt, fmtty) {
  if (typeof sub_fmtty === "number") {
    return /* Fmtty_fmt_EBB */{
            _0: /* End_of_fmtty */0,
            _1: type_format_gen(fmt, fmtty)
          };
  }
  switch (sub_fmtty.TAG | 0) {
    case /* Char_ty */0 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Char_ty */0) {
          var match = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Char_ty */0,
                    _0: match._0
                  },
                  _1: match._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* String_ty */1 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* String_ty */1) {
          var match$1 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* String_ty */1,
                    _0: match$1._0
                  },
                  _1: match$1._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int_ty */2 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Int_ty */2) {
          var match$2 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Int_ty */2,
                    _0: match$2._0
                  },
                  _1: match$2._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int32_ty */3 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Int32_ty */3) {
          var match$3 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Int32_ty */3,
                    _0: match$3._0
                  },
                  _1: match$3._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Nativeint_ty */4 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Nativeint_ty */4) {
          var match$4 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Nativeint_ty */4,
                    _0: match$4._0
                  },
                  _1: match$4._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int64_ty */5 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Int64_ty */5) {
          var match$5 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Int64_ty */5,
                    _0: match$5._0
                  },
                  _1: match$5._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Float_ty */6 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Float_ty */6) {
          var match$6 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Float_ty */6,
                    _0: match$6._0
                  },
                  _1: match$6._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Bool_ty */7 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Bool_ty */7) {
          var match$7 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Bool_ty */7,
                    _0: match$7._0
                  },
                  _1: match$7._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Format_arg_ty */8 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Format_arg_ty */8) {
          var sub2_fmtty$p = fmtty._0;
          if (Caml_obj.caml_notequal(/* Fmtty_EBB */{
                  _0: sub_fmtty._0
                }, /* Fmtty_EBB */{
                  _0: sub2_fmtty$p
                })) {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          var match$8 = type_ignored_format_substitution(sub_fmtty._1, fmt, fmtty._1);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Format_arg_ty */8,
                    _0: sub2_fmtty$p,
                    _1: match$8._0
                  },
                  _1: match$8._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Format_subst_ty */9 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Format_subst_ty */9) {
          var sub2_fmtty$p$1 = fmtty._1;
          var sub1_fmtty$p = fmtty._0;
          if (Caml_obj.caml_notequal(/* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(sub_fmtty._0)
                }, /* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(sub1_fmtty$p)
                })) {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          if (Caml_obj.caml_notequal(/* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(sub_fmtty._1)
                }, /* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(sub2_fmtty$p$1)
                })) {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          var sub_fmtty$p = trans(symm(sub1_fmtty$p), sub2_fmtty$p$1);
          var match$9 = fmtty_rel_det(sub_fmtty$p);
          Curry._1(match$9[1], /* Refl */0);
          Curry._1(match$9[3], /* Refl */0);
          var match$10 = type_ignored_format_substitution(CamlinternalFormatBasics.erase_rel(sub_fmtty._2), fmt, fmtty._2);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Format_subst_ty */9,
                    _0: sub1_fmtty$p,
                    _1: sub2_fmtty$p$1,
                    _2: symm(match$10._0)
                  },
                  _1: match$10._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Alpha_ty */10 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Alpha_ty */10) {
          var match$11 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Alpha_ty */10,
                    _0: match$11._0
                  },
                  _1: match$11._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Theta_ty */11 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Theta_ty */11) {
          var match$12 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Theta_ty */11,
                    _0: match$12._0
                  },
                  _1: match$12._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Any_ty */12 :
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Reader_ty */13 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Reader_ty */13) {
          var match$13 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Reader_ty */13,
                    _0: match$13._0
                  },
                  _1: match$13._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Ignored_reader_ty */14 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Ignored_reader_ty */14) {
          var match$14 = type_ignored_format_substitution(sub_fmtty._0, fmt, fmtty._0);
          return /* Fmtty_fmt_EBB */{
                  _0: {
                    TAG: /* Ignored_reader_ty */14,
                    _0: match$14._0
                  },
                  _1: match$14._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    
  }
}

function type_format_gen(fmt, fmtty) {
  if (typeof fmt === "number") {
    return /* Fmt_fmtty_EBB */{
            _0: /* End_of_format */0,
            _1: fmtty
          };
  }
  switch (fmt.TAG | 0) {
    case /* Char */0 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Char_ty */0) {
          var match = type_format_gen(fmt._0, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Char */0,
                    _0: match._0
                  },
                  _1: match._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Caml_char */1 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Char_ty */0) {
          var match$1 = type_format_gen(fmt._0, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Caml_char */1,
                    _0: match$1._0
                  },
                  _1: match$1._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* String */2 :
        var match$2 = type_padding(fmt._0, fmtty);
        var fmtty_rest = match$2._1;
        if (typeof fmtty_rest === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest.TAG === /* String_ty */1) {
          var match$3 = type_format_gen(fmt._1, fmtty_rest._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* String */2,
                    _0: match$2._0,
                    _1: match$3._0
                  },
                  _1: match$3._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Caml_string */3 :
        var match$4 = type_padding(fmt._0, fmtty);
        var fmtty_rest$1 = match$4._1;
        if (typeof fmtty_rest$1 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$1.TAG === /* String_ty */1) {
          var match$5 = type_format_gen(fmt._1, fmtty_rest$1._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Caml_string */3,
                    _0: match$4._0,
                    _1: match$5._0
                  },
                  _1: match$5._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int */4 :
        var match$6 = type_padprec(fmt._1, fmt._2, fmtty);
        var fmtty_rest$2 = match$6._2;
        if (typeof fmtty_rest$2 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$2.TAG === /* Int_ty */2) {
          var match$7 = type_format_gen(fmt._3, fmtty_rest$2._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Int */4,
                    _0: fmt._0,
                    _1: match$6._0,
                    _2: match$6._1,
                    _3: match$7._0
                  },
                  _1: match$7._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int32 */5 :
        var match$8 = type_padprec(fmt._1, fmt._2, fmtty);
        var fmtty_rest$3 = match$8._2;
        if (typeof fmtty_rest$3 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$3.TAG === /* Int32_ty */3) {
          var match$9 = type_format_gen(fmt._3, fmtty_rest$3._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Int32 */5,
                    _0: fmt._0,
                    _1: match$8._0,
                    _2: match$8._1,
                    _3: match$9._0
                  },
                  _1: match$9._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Nativeint */6 :
        var match$10 = type_padprec(fmt._1, fmt._2, fmtty);
        var fmtty_rest$4 = match$10._2;
        if (typeof fmtty_rest$4 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$4.TAG === /* Nativeint_ty */4) {
          var match$11 = type_format_gen(fmt._3, fmtty_rest$4._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Nativeint */6,
                    _0: fmt._0,
                    _1: match$10._0,
                    _2: match$10._1,
                    _3: match$11._0
                  },
                  _1: match$11._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Int64 */7 :
        var match$12 = type_padprec(fmt._1, fmt._2, fmtty);
        var fmtty_rest$5 = match$12._2;
        if (typeof fmtty_rest$5 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$5.TAG === /* Int64_ty */5) {
          var match$13 = type_format_gen(fmt._3, fmtty_rest$5._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Int64 */7,
                    _0: fmt._0,
                    _1: match$12._0,
                    _2: match$12._1,
                    _3: match$13._0
                  },
                  _1: match$13._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Float */8 :
        var match$14 = type_padprec(fmt._1, fmt._2, fmtty);
        var fmtty_rest$6 = match$14._2;
        if (typeof fmtty_rest$6 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$6.TAG === /* Float_ty */6) {
          var match$15 = type_format_gen(fmt._3, fmtty_rest$6._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Float */8,
                    _0: fmt._0,
                    _1: match$14._0,
                    _2: match$14._1,
                    _3: match$15._0
                  },
                  _1: match$15._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Bool */9 :
        var match$16 = type_padding(fmt._0, fmtty);
        var fmtty_rest$7 = match$16._1;
        if (typeof fmtty_rest$7 === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty_rest$7.TAG === /* Bool_ty */7) {
          var match$17 = type_format_gen(fmt._1, fmtty_rest$7._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Bool */9,
                    _0: match$16._0,
                    _1: match$17._0
                  },
                  _1: match$17._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Flush */10 :
        var match$18 = type_format_gen(fmt._0, fmtty);
        return /* Fmt_fmtty_EBB */{
                _0: {
                  TAG: /* Flush */10,
                  _0: match$18._0
                },
                _1: match$18._1
              };
    case /* String_literal */11 :
        var match$19 = type_format_gen(fmt._1, fmtty);
        return /* Fmt_fmtty_EBB */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: fmt._0,
                  _1: match$19._0
                },
                _1: match$19._1
              };
    case /* Char_literal */12 :
        var match$20 = type_format_gen(fmt._1, fmtty);
        return /* Fmt_fmtty_EBB */{
                _0: {
                  TAG: /* Char_literal */12,
                  _0: fmt._0,
                  _1: match$20._0
                },
                _1: match$20._1
              };
    case /* Format_arg */13 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Format_arg_ty */8) {
          var sub_fmtty$p = fmtty._0;
          if (Caml_obj.caml_notequal(/* Fmtty_EBB */{
                  _0: fmt._1
                }, /* Fmtty_EBB */{
                  _0: sub_fmtty$p
                })) {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          var match$21 = type_format_gen(fmt._2, fmtty._1);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Format_arg */13,
                    _0: fmt._0,
                    _1: sub_fmtty$p,
                    _2: match$21._0
                  },
                  _1: match$21._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Format_subst */14 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Format_subst_ty */9) {
          var sub_fmtty1 = fmtty._0;
          if (Caml_obj.caml_notequal(/* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(fmt._1)
                }, /* Fmtty_EBB */{
                  _0: CamlinternalFormatBasics.erase_rel(sub_fmtty1)
                })) {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          var match$22 = type_format_gen(fmt._2, CamlinternalFormatBasics.erase_rel(fmtty._2));
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Format_subst */14,
                    _0: fmt._0,
                    _1: sub_fmtty1,
                    _2: match$22._0
                  },
                  _1: match$22._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Alpha */15 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Alpha_ty */10) {
          var match$23 = type_format_gen(fmt._0, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Alpha */15,
                    _0: match$23._0
                  },
                  _1: match$23._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Theta */16 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Theta_ty */11) {
          var match$24 = type_format_gen(fmt._0, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Theta */16,
                    _0: match$24._0
                  },
                  _1: match$24._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Formatting_lit */17 :
        var match$25 = type_format_gen(fmt._1, fmtty);
        return /* Fmt_fmtty_EBB */{
                _0: {
                  TAG: /* Formatting_lit */17,
                  _0: fmt._0,
                  _1: match$25._0
                },
                _1: match$25._1
              };
    case /* Formatting_gen */18 :
        var formatting_gen = fmt._0;
        var fmt0 = fmt._1;
        if (formatting_gen.TAG === /* Open_tag */0) {
          var match$26 = formatting_gen._0;
          var match$27 = type_format_gen(match$26._0, fmtty);
          var match$28 = type_format_gen(fmt0, match$27._1);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Formatting_gen */18,
                    _0: {
                      TAG: /* Open_tag */0,
                      _0: /* Format */{
                        _0: match$27._0,
                        _1: match$26._1
                      }
                    },
                    _1: match$28._0
                  },
                  _1: match$28._1
                };
        }
        var match$29 = formatting_gen._0;
        var match$30 = type_format_gen(match$29._0, fmtty);
        var match$31 = type_format_gen(fmt0, match$30._1);
        return /* Fmt_fmtty_EBB */{
                _0: {
                  TAG: /* Formatting_gen */18,
                  _0: {
                    TAG: /* Open_box */1,
                    _0: /* Format */{
                      _0: match$30._0,
                      _1: match$29._1
                    }
                  },
                  _1: match$31._0
                },
                _1: match$31._1
              };
    case /* Reader */19 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Reader_ty */13) {
          var match$32 = type_format_gen(fmt._0, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Reader */19,
                    _0: match$32._0
                  },
                  _1: match$32._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Scan_char_set */20 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* String_ty */1) {
          var match$33 = type_format_gen(fmt._2, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Scan_char_set */20,
                    _0: fmt._0,
                    _1: fmt._1,
                    _2: match$33._0
                  },
                  _1: match$33._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Scan_get_counter */21 :
        if (typeof fmtty === "number") {
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        }
        if (fmtty.TAG === /* Int_ty */2) {
          var match$34 = type_format_gen(fmt._1, fmtty._0);
          return /* Fmt_fmtty_EBB */{
                  _0: {
                    TAG: /* Scan_get_counter */21,
                    _0: fmt._0,
                    _1: match$34._0
                  },
                  _1: match$34._1
                };
        }
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    case /* Ignored_param */23 :
        var ign = fmt._0;
        var fmt$1 = fmt._1;
        if (typeof ign === "number") {
          if (ign !== /* Ignored_reader */2) {
            return type_ignored_param_one(ign, fmt$1, fmtty);
          }
          if (typeof fmtty === "number") {
            throw {
                  RE_EXN_ID: Type_mismatch,
                  Error: new Error()
                };
          }
          if (fmtty.TAG === /* Ignored_reader_ty */14) {
            var match$35 = type_format_gen(fmt$1, fmtty._0);
            return /* Fmt_fmtty_EBB */{
                    _0: {
                      TAG: /* Ignored_param */23,
                      _0: /* Ignored_reader */2,
                      _1: match$35._0
                    },
                    _1: match$35._1
                  };
          }
          throw {
                RE_EXN_ID: Type_mismatch,
                Error: new Error()
              };
        } else {
          switch (ign.TAG | 0) {
            case /* Ignored_format_arg */8 :
                return type_ignored_param_one({
                            TAG: /* Ignored_format_arg */8,
                            _0: ign._0,
                            _1: ign._1
                          }, fmt$1, fmtty);
            case /* Ignored_format_subst */9 :
                var match$36 = type_ignored_format_substitution(ign._1, fmt$1, fmtty);
                var match$37 = match$36._1;
                return /* Fmt_fmtty_EBB */{
                        _0: {
                          TAG: /* Ignored_param */23,
                          _0: {
                            TAG: /* Ignored_format_subst */9,
                            _0: ign._0,
                            _1: match$36._0
                          },
                          _1: match$37._0
                        },
                        _1: match$37._1
                      };
            default:
              return type_ignored_param_one(ign, fmt$1, fmtty);
          }
        }
    case /* Scan_next_char */22 :
    case /* Custom */24 :
        throw {
              RE_EXN_ID: Type_mismatch,
              Error: new Error()
            };
    
  }
}

function type_ignored_param_one(ign, fmt, fmtty) {
  var match = type_format_gen(fmt, fmtty);
  return /* Fmt_fmtty_EBB */{
          _0: {
            TAG: /* Ignored_param */23,
            _0: ign,
            _1: match._0
          },
          _1: match._1
        };
}

function type_format(fmt, fmtty) {
  var match = type_format_gen(fmt, fmtty);
  if (typeof match._1 === "number") {
    return match._0;
  }
  throw {
        RE_EXN_ID: Type_mismatch,
        Error: new Error()
      };
}

function recast(fmt, fmtty) {
  return type_format(fmt, CamlinternalFormatBasics.erase_rel(symm(fmtty)));
}

function fix_padding(padty, width, str) {
  var len = str.length;
  var width$1 = Pervasives.abs(width);
  var padty$1 = width < 0 ? /* Left */0 : padty;
  if (width$1 <= len) {
    return str;
  }
  var res = Bytes.make(width$1, padty$1 === /* Zeros */2 ? /* '0' */48 : /* ' ' */32);
  switch (padty$1) {
    case /* Left */0 :
        $$String.blit(str, 0, res, 0, len);
        break;
    case /* Right */1 :
        $$String.blit(str, 0, res, width$1 - len | 0, len);
        break;
    case /* Zeros */2 :
        if (len > 0 && (Caml_string.get(str, 0) === /* '+' */43 || Caml_string.get(str, 0) === /* '-' */45 || Caml_string.get(str, 0) === /* ' ' */32)) {
          Caml_bytes.set(res, 0, Caml_string.get(str, 0));
          $$String.blit(str, 1, res, (width$1 - len | 0) + 1 | 0, len - 1 | 0);
        } else if (len > 1 && Caml_string.get(str, 0) === /* '0' */48 && (Caml_string.get(str, 1) === /* 'x' */120 || Caml_string.get(str, 1) === /* 'X' */88)) {
          Caml_bytes.set(res, 1, Caml_string.get(str, 1));
          $$String.blit(str, 2, res, (width$1 - len | 0) + 2 | 0, len - 2 | 0);
        } else {
          $$String.blit(str, 0, res, width$1 - len | 0, len);
        }
        break;
    
  }
  return Caml_bytes.bytes_to_string(res);
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
      }
      exit = 2;
    } else {
      if (c < 65) {
        return str;
      }
      exit = 2;
    }
  } else if (c !== 32) {
    if (c < 43) {
      return str;
    }
    switch (c) {
      case 43 :
      case 45 :
          exit = 1;
          break;
      case 44 :
      case 46 :
      case 47 :
          return str;
      case 48 :
          if ((prec$1 + 2 | 0) > len && len > 1 && (Caml_string.get(str, 1) === /* 'x' */120 || Caml_string.get(str, 1) === /* 'X' */88)) {
            var res = Bytes.make(prec$1 + 2 | 0, /* '0' */48);
            Caml_bytes.set(res, 1, Caml_string.get(str, 1));
            $$String.blit(str, 2, res, (prec$1 - len | 0) + 4 | 0, len - 2 | 0);
            return Caml_bytes.bytes_to_string(res);
          }
          exit = 2;
          break;
      case 49 :
      case 50 :
      case 51 :
      case 52 :
      case 53 :
      case 54 :
      case 55 :
      case 56 :
      case 57 :
          exit = 2;
          break;
      
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        if ((prec$1 + 1 | 0) <= len) {
          return str;
        }
        var res$1 = Bytes.make(prec$1 + 1 | 0, /* '0' */48);
        Caml_bytes.set(res$1, 0, c);
        $$String.blit(str, 1, res$1, (prec$1 - len | 0) + 2 | 0, len - 1 | 0);
        return Caml_bytes.bytes_to_string(res$1);
    case 2 :
        if (prec$1 <= len) {
          return str;
        }
        var res$2 = Bytes.make(prec$1, /* '0' */48);
        $$String.blit(str, 0, res$2, prec$1 - len | 0, len);
        return Caml_bytes.bytes_to_string(res$2);
    
  }
}

function string_to_caml_string(str) {
  var str$1 = $$String.escaped(str);
  var l = str$1.length;
  var res = Bytes.make(l + 2 | 0, /* '"' */34);
  Caml_bytes.caml_blit_string(str$1, 0, res, 1, l);
  return Caml_bytes.bytes_to_string(res);
}

function format_of_iconv(param) {
  switch (param) {
    case /* Int_d */0 :
        return "%d";
    case /* Int_pd */1 :
        return "%+d";
    case /* Int_sd */2 :
        return "% d";
    case /* Int_i */3 :
        return "%i";
    case /* Int_pi */4 :
        return "%+i";
    case /* Int_si */5 :
        return "% i";
    case /* Int_x */6 :
        return "%x";
    case /* Int_Cx */7 :
        return "%#x";
    case /* Int_X */8 :
        return "%X";
    case /* Int_CX */9 :
        return "%#X";
    case /* Int_o */10 :
        return "%o";
    case /* Int_Co */11 :
        return "%#o";
    case /* Int_u */12 :
        return "%u";
    
  }
}

function format_of_iconvL(param) {
  switch (param) {
    case /* Int_d */0 :
        return "%Ld";
    case /* Int_pd */1 :
        return "%+Ld";
    case /* Int_sd */2 :
        return "% Ld";
    case /* Int_i */3 :
        return "%Li";
    case /* Int_pi */4 :
        return "%+Li";
    case /* Int_si */5 :
        return "% Li";
    case /* Int_x */6 :
        return "%Lx";
    case /* Int_Cx */7 :
        return "%#Lx";
    case /* Int_X */8 :
        return "%LX";
    case /* Int_CX */9 :
        return "%#LX";
    case /* Int_o */10 :
        return "%Lo";
    case /* Int_Co */11 :
        return "%#Lo";
    case /* Int_u */12 :
        return "%Lu";
    
  }
}

function format_of_iconvl(param) {
  switch (param) {
    case /* Int_d */0 :
        return "%ld";
    case /* Int_pd */1 :
        return "%+ld";
    case /* Int_sd */2 :
        return "% ld";
    case /* Int_i */3 :
        return "%li";
    case /* Int_pi */4 :
        return "%+li";
    case /* Int_si */5 :
        return "% li";
    case /* Int_x */6 :
        return "%lx";
    case /* Int_Cx */7 :
        return "%#lx";
    case /* Int_X */8 :
        return "%lX";
    case /* Int_CX */9 :
        return "%#lX";
    case /* Int_o */10 :
        return "%lo";
    case /* Int_Co */11 :
        return "%#lo";
    case /* Int_u */12 :
        return "%lu";
    
  }
}

function format_of_iconvn(param) {
  switch (param) {
    case /* Int_d */0 :
        return "%nd";
    case /* Int_pd */1 :
        return "%+nd";
    case /* Int_sd */2 :
        return "% nd";
    case /* Int_i */3 :
        return "%ni";
    case /* Int_pi */4 :
        return "%+ni";
    case /* Int_si */5 :
        return "% ni";
    case /* Int_x */6 :
        return "%nx";
    case /* Int_Cx */7 :
        return "%#nx";
    case /* Int_X */8 :
        return "%nX";
    case /* Int_CX */9 :
        return "%#nX";
    case /* Int_o */10 :
        return "%no";
    case /* Int_Co */11 :
        return "%#no";
    case /* Int_u */12 :
        return "%nu";
    
  }
}

function format_of_fconv(fconv, prec) {
  if (fconv === /* Float_F */15) {
    return "%.12g";
  }
  var prec$1 = Pervasives.abs(prec);
  var symb = char_of_fconv(fconv);
  var buf = {
    ind: 0,
    bytes: Caml_bytes.caml_create_bytes(16)
  };
  buffer_add_char(buf, /* '%' */37);
  bprint_fconv_flag(buf, fconv);
  buffer_add_char(buf, /* '.' */46);
  buffer_add_string(buf, String(prec$1));
  buffer_add_char(buf, symb);
  return buffer_contents(buf);
}

function convert_int(iconv, n) {
  return Caml_format.caml_format_int(format_of_iconv(iconv), n);
}

function convert_int32(iconv, n) {
  return Caml_format.caml_int32_format(format_of_iconvl(iconv), n);
}

function convert_nativeint(iconv, n) {
  return Caml_format.caml_nativeint_format(format_of_iconvn(iconv), n);
}

function convert_int64(iconv, n) {
  return Caml_format.caml_int64_format(format_of_iconvL(iconv), n);
}

function convert_float(fconv, prec, x) {
  if (fconv >= 16) {
    var sign;
    if (fconv >= 17) {
      switch (fconv) {
        case /* Float_H */19 :
            sign = /* '-' */45;
            break;
        case /* Float_ph */17 :
        case /* Float_pH */20 :
            sign = /* '+' */43;
            break;
        case /* Float_sh */18 :
        case /* Float_sH */21 :
            sign = /* ' ' */32;
            break;
        
      }
    } else {
      sign = /* '-' */45;
    }
    var str = Caml_format.caml_hexstring_of_float(x, prec, sign);
    if (fconv >= 19) {
      return Caml_bytes.bytes_to_string(Bytes.uppercase_ascii(Caml_bytes.bytes_of_string(str)));
    } else {
      return str;
    }
  }
  var str$1 = Caml_format.caml_format_float(format_of_fconv(fconv, prec), x);
  if (fconv !== /* Float_F */15) {
    return str$1;
  }
  var len = str$1.length;
  var is_valid = function (_i) {
    while(true) {
      var i = _i;
      if (i === len) {
        return false;
      }
      var match = Caml_string.get(str$1, i);
      if (match > 69 || match < 46) {
        if (match === 101) {
          return true;
        }
        _i = i + 1 | 0;
        continue ;
      }
      if (match > 68 || match < 47) {
        return true;
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  var match = Pervasives.classify_float(x);
  if (match !== 3) {
    if (match >= 4) {
      return "nan";
    } else if (is_valid(0)) {
      return str$1;
    } else {
      return str$1 + ".";
    }
  } else if (x < 0.0) {
    return "neg_infinity";
  } else {
    return "infinity";
  }
}

function format_caml_char(c) {
  var str = Char.escaped(c);
  var l = str.length;
  var res = Bytes.make(l + 2 | 0, /* '\'' */39);
  Caml_bytes.caml_blit_string(str, 0, res, 1, l);
  return Caml_bytes.bytes_to_string(res);
}

function string_of_fmtty(fmtty) {
  var buf = {
    ind: 0,
    bytes: Caml_bytes.caml_create_bytes(16)
  };
  bprint_fmtty(buf, fmtty);
  return buffer_contents(buf);
}

function make_printf(_k, o, _acc, _fmt) {
  while(true) {
    var fmt = _fmt;
    var acc = _acc;
    var k = _k;
    if (typeof fmt === "number") {
      return Curry._2(k, o, acc);
    }
    switch (fmt.TAG | 0) {
      case /* Char */0 :
          var rest = fmt._0;
          return (function(k,acc,rest){
          return function (c) {
            var new_acc = {
              TAG: /* Acc_data_char */5,
              _0: acc,
              _1: c
            };
            return make_printf(k, o, new_acc, rest);
          }
          }(k,acc,rest));
      case /* Caml_char */1 :
          var rest$1 = fmt._0;
          return (function(k,acc,rest$1){
          return function (c) {
            var new_acc_1 = format_caml_char(c);
            var new_acc = {
              TAG: /* Acc_data_string */4,
              _0: acc,
              _1: new_acc_1
            };
            return make_printf(k, o, new_acc, rest$1);
          }
          }(k,acc,rest$1));
      case /* String */2 :
          return make_padding(k, o, acc, fmt._1, fmt._0, (function (str) {
                        return str;
                      }));
      case /* Caml_string */3 :
          return make_padding(k, o, acc, fmt._1, fmt._0, string_to_caml_string);
      case /* Int */4 :
          return make_int_padding_precision(k, o, acc, fmt._3, fmt._1, fmt._2, convert_int, fmt._0);
      case /* Int32 */5 :
          return make_int_padding_precision(k, o, acc, fmt._3, fmt._1, fmt._2, convert_int32, fmt._0);
      case /* Nativeint */6 :
          return make_int_padding_precision(k, o, acc, fmt._3, fmt._1, fmt._2, convert_nativeint, fmt._0);
      case /* Int64 */7 :
          return make_int_padding_precision(k, o, acc, fmt._3, fmt._1, fmt._2, convert_int64, fmt._0);
      case /* Float */8 :
          var fmt$1 = fmt._3;
          var pad = fmt._1;
          var prec = fmt._2;
          var fconv = fmt._0;
          if (typeof pad === "number") {
            if (typeof prec === "number") {
              if (prec !== 0) {
                return (function(k,acc,fmt$1,fconv){
                return function (p, x) {
                  var str = convert_float(fconv, p, x);
                  return make_printf(k, o, {
                              TAG: /* Acc_data_string */4,
                              _0: acc,
                              _1: str
                            }, fmt$1);
                }
                }(k,acc,fmt$1,fconv));
              } else {
                return (function(k,acc,fmt$1,fconv){
                return function (x) {
                  var str = convert_float(fconv, -6, x);
                  return make_printf(k, o, {
                              TAG: /* Acc_data_string */4,
                              _0: acc,
                              _1: str
                            }, fmt$1);
                }
                }(k,acc,fmt$1,fconv));
              }
            }
            var p = prec._0;
            return (function(k,acc,fmt$1,fconv,p){
            return function (x) {
              var str = convert_float(fconv, p, x);
              return make_printf(k, o, {
                          TAG: /* Acc_data_string */4,
                          _0: acc,
                          _1: str
                        }, fmt$1);
            }
            }(k,acc,fmt$1,fconv,p));
          }
          if (pad.TAG === /* Lit_padding */0) {
            var w = pad._1;
            var padty = pad._0;
            if (typeof prec === "number") {
              if (prec !== 0) {
                return (function(k,acc,fmt$1,fconv,padty,w){
                return function (p, x) {
                  var str = fix_padding(padty, w, convert_float(fconv, p, x));
                  return make_printf(k, o, {
                              TAG: /* Acc_data_string */4,
                              _0: acc,
                              _1: str
                            }, fmt$1);
                }
                }(k,acc,fmt$1,fconv,padty,w));
              } else {
                return (function(k,acc,fmt$1,fconv,padty,w){
                return function (x) {
                  var str = convert_float(fconv, -6, x);
                  var str$p = fix_padding(padty, w, str);
                  return make_printf(k, o, {
                              TAG: /* Acc_data_string */4,
                              _0: acc,
                              _1: str$p
                            }, fmt$1);
                }
                }(k,acc,fmt$1,fconv,padty,w));
              }
            }
            var p$1 = prec._0;
            return (function(k,acc,fmt$1,fconv,padty,w,p$1){
            return function (x) {
              var str = fix_padding(padty, w, convert_float(fconv, p$1, x));
              return make_printf(k, o, {
                          TAG: /* Acc_data_string */4,
                          _0: acc,
                          _1: str
                        }, fmt$1);
            }
            }(k,acc,fmt$1,fconv,padty,w,p$1));
          }
          var padty$1 = pad._0;
          if (typeof prec === "number") {
            if (prec !== 0) {
              return (function(k,acc,fmt$1,fconv,padty$1){
              return function (w, p, x) {
                var str = fix_padding(padty$1, w, convert_float(fconv, p, x));
                return make_printf(k, o, {
                            TAG: /* Acc_data_string */4,
                            _0: acc,
                            _1: str
                          }, fmt$1);
              }
              }(k,acc,fmt$1,fconv,padty$1));
            } else {
              return (function(k,acc,fmt$1,fconv,padty$1){
              return function (w, x) {
                var str = convert_float(fconv, -6, x);
                var str$p = fix_padding(padty$1, w, str);
                return make_printf(k, o, {
                            TAG: /* Acc_data_string */4,
                            _0: acc,
                            _1: str$p
                          }, fmt$1);
              }
              }(k,acc,fmt$1,fconv,padty$1));
            }
          }
          var p$2 = prec._0;
          return (function(k,acc,fmt$1,fconv,padty$1,p$2){
          return function (w, x) {
            var str = fix_padding(padty$1, w, convert_float(fconv, p$2, x));
            return make_printf(k, o, {
                        TAG: /* Acc_data_string */4,
                        _0: acc,
                        _1: str
                      }, fmt$1);
          }
          }(k,acc,fmt$1,fconv,padty$1,p$2));
      case /* Bool */9 :
          return make_padding(k, o, acc, fmt._1, fmt._0, Pervasives.string_of_bool);
      case /* Flush */10 :
          _fmt = fmt._0;
          _acc = {
            TAG: /* Acc_flush */7,
            _0: acc
          };
          continue ;
      case /* String_literal */11 :
          _fmt = fmt._1;
          _acc = {
            TAG: /* Acc_string_literal */2,
            _0: acc,
            _1: fmt._0
          };
          continue ;
      case /* Char_literal */12 :
          _fmt = fmt._1;
          _acc = {
            TAG: /* Acc_char_literal */3,
            _0: acc,
            _1: fmt._0
          };
          continue ;
      case /* Format_arg */13 :
          var rest$2 = fmt._2;
          var ty = string_of_fmtty(fmt._1);
          return (function(k,acc,rest$2,ty){
          return function (str) {
            return make_printf(k, o, {
                        TAG: /* Acc_data_string */4,
                        _0: acc,
                        _1: ty
                      }, rest$2);
          }
          }(k,acc,rest$2,ty));
      case /* Format_subst */14 :
          var rest$3 = fmt._2;
          var fmtty = fmt._1;
          return (function(k,acc,fmtty,rest$3){
          return function (param) {
            return make_printf(k, o, acc, CamlinternalFormatBasics.concat_fmt(recast(param._0, fmtty), rest$3));
          }
          }(k,acc,fmtty,rest$3));
      case /* Alpha */15 :
          var rest$4 = fmt._0;
          return (function(k,acc,rest$4){
          return function (f, x) {
            return make_printf(k, o, {
                        TAG: /* Acc_delay */6,
                        _0: acc,
                        _1: (function (o) {
                            return Curry._2(f, o, x);
                          })
                      }, rest$4);
          }
          }(k,acc,rest$4));
      case /* Theta */16 :
          var rest$5 = fmt._0;
          return (function(k,acc,rest$5){
          return function (f) {
            return make_printf(k, o, {
                        TAG: /* Acc_delay */6,
                        _0: acc,
                        _1: f
                      }, rest$5);
          }
          }(k,acc,rest$5));
      case /* Formatting_lit */17 :
          _fmt = fmt._1;
          _acc = {
            TAG: /* Acc_formatting_lit */0,
            _0: acc,
            _1: fmt._0
          };
          continue ;
      case /* Formatting_gen */18 :
          var match = fmt._0;
          if (match.TAG === /* Open_tag */0) {
            var rest$6 = fmt._1;
            var k$p = (function(k,acc,rest$6){
            return function k$p(koc, kacc) {
              return make_printf(k, koc, {
                          TAG: /* Acc_formatting_gen */1,
                          _0: acc,
                          _1: {
                            TAG: /* Acc_open_tag */0,
                            _0: kacc
                          }
                        }, rest$6);
            }
            }(k,acc,rest$6));
            _fmt = match._0._0;
            _acc = /* End_of_acc */0;
            _k = k$p;
            continue ;
          }
          var rest$7 = fmt._1;
          var k$p$1 = (function(k,acc,rest$7){
          return function k$p$1(koc, kacc) {
            return make_printf(k, koc, {
                        TAG: /* Acc_formatting_gen */1,
                        _0: acc,
                        _1: {
                          TAG: /* Acc_open_box */1,
                          _0: kacc
                        }
                      }, rest$7);
          }
          }(k,acc,rest$7));
          _fmt = match._0._0;
          _acc = /* End_of_acc */0;
          _k = k$p$1;
          continue ;
      case /* Reader */19 :
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "camlinternalFormat.ml",
                  1525,
                  4
                ],
                Error: new Error()
              };
      case /* Scan_char_set */20 :
          var rest$8 = fmt._2;
          var new_acc = {
            TAG: /* Acc_invalid_arg */8,
            _0: acc,
            _1: "Printf: bad conversion %["
          };
          return (function(k,rest$8,new_acc){
          return function (param) {
            return make_printf(k, o, new_acc, rest$8);
          }
          }(k,rest$8,new_acc));
      case /* Scan_get_counter */21 :
          var rest$9 = fmt._1;
          return (function(k,acc,rest$9){
          return function (n) {
            var new_acc_1 = Caml_format.caml_format_int("%u", n);
            var new_acc = {
              TAG: /* Acc_data_string */4,
              _0: acc,
              _1: new_acc_1
            };
            return make_printf(k, o, new_acc, rest$9);
          }
          }(k,acc,rest$9));
      case /* Scan_next_char */22 :
          var rest$10 = fmt._0;
          return (function(k,acc,rest$10){
          return function (c) {
            var new_acc = {
              TAG: /* Acc_data_char */5,
              _0: acc,
              _1: c
            };
            return make_printf(k, o, new_acc, rest$10);
          }
          }(k,acc,rest$10));
      case /* Ignored_param */23 :
          return make_ignored_param(k, o, acc, fmt._0, fmt._1);
      case /* Custom */24 :
          return make_custom(k, o, acc, fmt._2, fmt._0, Curry._1(fmt._1, undefined));
      
    }
  };
}

function make_ignored_param(k, o, acc, ign, fmt) {
  if (typeof ign !== "number") {
    if (ign.TAG === /* Ignored_format_subst */9) {
      return make_from_fmtty(k, o, acc, ign._1, fmt);
    } else {
      return make_invalid_arg(k, o, acc, fmt);
    }
  }
  if (ign !== /* Ignored_reader */2) {
    return make_invalid_arg(k, o, acc, fmt);
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalFormat.ml",
          1593,
          39
        ],
        Error: new Error()
      };
}

function make_from_fmtty(k, o, acc, fmtty, fmt) {
  if (typeof fmtty === "number") {
    return make_invalid_arg(k, o, acc, fmt);
  }
  switch (fmtty.TAG | 0) {
    case /* Char_ty */0 :
        var rest = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest, fmt);
        };
    case /* String_ty */1 :
        var rest$1 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$1, fmt);
        };
    case /* Int_ty */2 :
        var rest$2 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$2, fmt);
        };
    case /* Int32_ty */3 :
        var rest$3 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$3, fmt);
        };
    case /* Nativeint_ty */4 :
        var rest$4 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$4, fmt);
        };
    case /* Int64_ty */5 :
        var rest$5 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$5, fmt);
        };
    case /* Float_ty */6 :
        var rest$6 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$6, fmt);
        };
    case /* Bool_ty */7 :
        var rest$7 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$7, fmt);
        };
    case /* Format_arg_ty */8 :
        var rest$8 = fmtty._1;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$8, fmt);
        };
    case /* Format_subst_ty */9 :
        var rest$9 = fmtty._2;
        var ty = trans(symm(fmtty._0), fmtty._1);
        return function (param) {
          return make_from_fmtty(k, o, acc, CamlinternalFormatBasics.concat_fmtty(ty, rest$9), fmt);
        };
    case /* Alpha_ty */10 :
        var rest$10 = fmtty._0;
        return function (param, param$1) {
          return make_from_fmtty(k, o, acc, rest$10, fmt);
        };
    case /* Theta_ty */11 :
        var rest$11 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$11, fmt);
        };
    case /* Any_ty */12 :
        var rest$12 = fmtty._0;
        return function (param) {
          return make_from_fmtty(k, o, acc, rest$12, fmt);
        };
    case /* Reader_ty */13 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                1616,
                31
              ],
              Error: new Error()
            };
    case /* Ignored_reader_ty */14 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                1617,
                31
              ],
              Error: new Error()
            };
    
  }
}

function make_invalid_arg(k, o, acc, fmt) {
  return make_printf(k, o, {
              TAG: /* Acc_invalid_arg */8,
              _0: acc,
              _1: "Printf: bad conversion %_"
            }, fmt);
}

function make_padding(k, o, acc, fmt, pad, trans) {
  if (typeof pad === "number") {
    return function (x) {
      var new_acc_1 = Curry._1(trans, x);
      var new_acc = {
        TAG: /* Acc_data_string */4,
        _0: acc,
        _1: new_acc_1
      };
      return make_printf(k, o, new_acc, fmt);
    };
  }
  if (pad.TAG === /* Lit_padding */0) {
    var width = pad._1;
    var padty = pad._0;
    return function (x) {
      var new_acc_1 = fix_padding(padty, width, Curry._1(trans, x));
      var new_acc = {
        TAG: /* Acc_data_string */4,
        _0: acc,
        _1: new_acc_1
      };
      return make_printf(k, o, new_acc, fmt);
    };
  }
  var padty$1 = pad._0;
  return function (w, x) {
    var new_acc_1 = fix_padding(padty$1, w, Curry._1(trans, x));
    var new_acc = {
      TAG: /* Acc_data_string */4,
      _0: acc,
      _1: new_acc_1
    };
    return make_printf(k, o, new_acc, fmt);
  };
}

function make_int_padding_precision(k, o, acc, fmt, pad, prec, trans, iconv) {
  if (typeof pad === "number") {
    if (typeof prec === "number") {
      if (prec !== 0) {
        return function (p, x) {
          var str = fix_int_precision(p, Curry._2(trans, iconv, x));
          return make_printf(k, o, {
                      TAG: /* Acc_data_string */4,
                      _0: acc,
                      _1: str
                    }, fmt);
        };
      } else {
        return function (x) {
          var str = Curry._2(trans, iconv, x);
          return make_printf(k, o, {
                      TAG: /* Acc_data_string */4,
                      _0: acc,
                      _1: str
                    }, fmt);
        };
      }
    }
    var p = prec._0;
    return function (x) {
      var str = fix_int_precision(p, Curry._2(trans, iconv, x));
      return make_printf(k, o, {
                  TAG: /* Acc_data_string */4,
                  _0: acc,
                  _1: str
                }, fmt);
    };
  }
  if (pad.TAG === /* Lit_padding */0) {
    var w = pad._1;
    var padty = pad._0;
    if (typeof prec === "number") {
      if (prec !== 0) {
        return function (p, x) {
          var str = fix_padding(padty, w, fix_int_precision(p, Curry._2(trans, iconv, x)));
          return make_printf(k, o, {
                      TAG: /* Acc_data_string */4,
                      _0: acc,
                      _1: str
                    }, fmt);
        };
      } else {
        return function (x) {
          var str = fix_padding(padty, w, Curry._2(trans, iconv, x));
          return make_printf(k, o, {
                      TAG: /* Acc_data_string */4,
                      _0: acc,
                      _1: str
                    }, fmt);
        };
      }
    }
    var p$1 = prec._0;
    return function (x) {
      var str = fix_padding(padty, w, fix_int_precision(p$1, Curry._2(trans, iconv, x)));
      return make_printf(k, o, {
                  TAG: /* Acc_data_string */4,
                  _0: acc,
                  _1: str
                }, fmt);
    };
  }
  var padty$1 = pad._0;
  if (typeof prec === "number") {
    if (prec !== 0) {
      return function (w, p, x) {
        var str = fix_padding(padty$1, w, fix_int_precision(p, Curry._2(trans, iconv, x)));
        return make_printf(k, o, {
                    TAG: /* Acc_data_string */4,
                    _0: acc,
                    _1: str
                  }, fmt);
      };
    } else {
      return function (w, x) {
        var str = fix_padding(padty$1, w, Curry._2(trans, iconv, x));
        return make_printf(k, o, {
                    TAG: /* Acc_data_string */4,
                    _0: acc,
                    _1: str
                  }, fmt);
      };
    }
  }
  var p$2 = prec._0;
  return function (w, x) {
    var str = fix_padding(padty$1, w, fix_int_precision(p$2, Curry._2(trans, iconv, x)));
    return make_printf(k, o, {
                TAG: /* Acc_data_string */4,
                _0: acc,
                _1: str
              }, fmt);
  };
}

function make_custom(k, o, acc, rest, arity, f) {
  if (!arity) {
    return make_printf(k, o, {
                TAG: /* Acc_data_string */4,
                _0: acc,
                _1: f
              }, rest);
  }
  var arity$1 = arity._0;
  return function (x) {
    return make_custom(k, o, acc, rest, arity$1, Curry._1(f, x));
  };
}

function make_iprintf(_k, o, _fmt) {
  while(true) {
    var fmt = _fmt;
    var k = _k;
    var exit = 0;
    if (typeof fmt === "number") {
      return Curry._1(k, o);
    }
    switch (fmt.TAG | 0) {
      case /* String */2 :
          var exit$1 = 0;
          var tmp = fmt._0;
          if (typeof tmp === "number" || tmp.TAG === /* Lit_padding */0) {
            exit$1 = 4;
          } else {
            var partial_arg = make_iprintf(k, o, fmt._1);
            var partial_arg$1 = (function(partial_arg){
            return function partial_arg$1(param) {
              return partial_arg;
            }
            }(partial_arg));
            return function (param) {
              return partial_arg$1;
            };
          }
          if (exit$1 === 4) {
            var partial_arg$2 = make_iprintf(k, o, fmt._1);
            return (function(partial_arg$2){
            return function (param) {
              return partial_arg$2;
            }
            }(partial_arg$2));
          }
          break;
      case /* Caml_string */3 :
          var exit$2 = 0;
          var tmp$1 = fmt._0;
          if (typeof tmp$1 === "number" || tmp$1.TAG === /* Lit_padding */0) {
            exit$2 = 4;
          } else {
            var partial_arg$3 = make_iprintf(k, o, fmt._1);
            var partial_arg$4 = (function(partial_arg$3){
            return function partial_arg$4(param) {
              return partial_arg$3;
            }
            }(partial_arg$3));
            return function (param) {
              return partial_arg$4;
            };
          }
          if (exit$2 === 4) {
            var partial_arg$5 = make_iprintf(k, o, fmt._1);
            return (function(partial_arg$5){
            return function (param) {
              return partial_arg$5;
            }
            }(partial_arg$5));
          }
          break;
      case /* Bool */9 :
          var exit$3 = 0;
          var tmp$2 = fmt._0;
          if (typeof tmp$2 === "number" || tmp$2.TAG === /* Lit_padding */0) {
            exit$3 = 4;
          } else {
            var partial_arg$6 = make_iprintf(k, o, fmt._1);
            var partial_arg$7 = (function(partial_arg$6){
            return function partial_arg$7(param) {
              return partial_arg$6;
            }
            }(partial_arg$6));
            return function (param) {
              return partial_arg$7;
            };
          }
          if (exit$3 === 4) {
            var partial_arg$8 = make_iprintf(k, o, fmt._1);
            return (function(partial_arg$8){
            return function (param) {
              return partial_arg$8;
            }
            }(partial_arg$8));
          }
          break;
      case /* Flush */10 :
          _fmt = fmt._0;
          continue ;
      case /* Format_subst */14 :
          var rest = fmt._2;
          var fmtty = fmt._1;
          return (function(k,fmtty,rest){
          return function (param) {
            return make_iprintf(k, o, CamlinternalFormatBasics.concat_fmt(recast(param._0, fmtty), rest));
          }
          }(k,fmtty,rest));
      case /* Alpha */15 :
          var partial_arg$9 = make_iprintf(k, o, fmt._0);
          var partial_arg$10 = (function(partial_arg$9){
          return function partial_arg$10(param) {
            return partial_arg$9;
          }
          }(partial_arg$9));
          return function (param) {
            return partial_arg$10;
          };
      case /* String_literal */11 :
      case /* Char_literal */12 :
      case /* Formatting_lit */17 :
          exit = 2;
          break;
      case /* Formatting_gen */18 :
          var match = fmt._0;
          if (match.TAG === /* Open_tag */0) {
            var rest$1 = fmt._1;
            _fmt = match._0._0;
            _k = (function(k,rest$1){
            return function (koc) {
              return make_iprintf(k, koc, rest$1);
            }
            }(k,rest$1));
            continue ;
          }
          var rest$2 = fmt._1;
          _fmt = match._0._0;
          _k = (function(k,rest$2){
          return function (koc) {
            return make_iprintf(k, koc, rest$2);
          }
          }(k,rest$2));
          continue ;
      case /* Reader */19 :
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "camlinternalFormat.ml",
                  1797,
                  8
                ],
                Error: new Error()
              };
      case /* Format_arg */13 :
      case /* Scan_char_set */20 :
          exit = 3;
          break;
      case /* Scan_get_counter */21 :
          var partial_arg$11 = make_iprintf(k, o, fmt._1);
          return (function(partial_arg$11){
          return function (param) {
            return partial_arg$11;
          }
          }(partial_arg$11));
      case /* Char */0 :
      case /* Caml_char */1 :
      case /* Theta */16 :
      case /* Scan_next_char */22 :
          exit = 1;
          break;
      case /* Ignored_param */23 :
          return make_ignored_param((function(k){
                    return function (x, param) {
                      return Curry._1(k, x);
                    }
                    }(k)), o, /* End_of_acc */0, fmt._0, fmt._1);
      case /* Custom */24 :
          return fn_of_custom_arity(k, o, fmt._2, fmt._0);
      default:
        var fmt$1 = fmt._3;
        var pad = fmt._1;
        var prec = fmt._2;
        if (typeof pad === "number") {
          if (typeof prec === "number") {
            if (prec !== 0) {
              var partial_arg$12 = make_iprintf(k, o, fmt$1);
              var partial_arg$13 = (function(partial_arg$12){
              return function partial_arg$13(param) {
                return partial_arg$12;
              }
              }(partial_arg$12));
              return function (param) {
                return partial_arg$13;
              };
            }
            var partial_arg$14 = make_iprintf(k, o, fmt$1);
            return (function(partial_arg$14){
            return function (param) {
              return partial_arg$14;
            }
            }(partial_arg$14));
          }
          var partial_arg$15 = make_iprintf(k, o, fmt$1);
          return (function(partial_arg$15){
          return function (param) {
            return partial_arg$15;
          }
          }(partial_arg$15));
        }
        if (pad.TAG === /* Lit_padding */0) {
          if (typeof prec === "number") {
            if (prec !== 0) {
              var partial_arg$16 = make_iprintf(k, o, fmt$1);
              var partial_arg$17 = (function(partial_arg$16){
              return function partial_arg$17(param) {
                return partial_arg$16;
              }
              }(partial_arg$16));
              return function (param) {
                return partial_arg$17;
              };
            }
            var partial_arg$18 = make_iprintf(k, o, fmt$1);
            return (function(partial_arg$18){
            return function (param) {
              return partial_arg$18;
            }
            }(partial_arg$18));
          }
          var partial_arg$19 = make_iprintf(k, o, fmt$1);
          return (function(partial_arg$19){
          return function (param) {
            return partial_arg$19;
          }
          }(partial_arg$19));
        }
        if (typeof prec === "number") {
          if (prec !== 0) {
            var partial_arg$20 = make_iprintf(k, o, fmt$1);
            var partial_arg$21 = (function(partial_arg$20){
            return function partial_arg$21(param) {
              return partial_arg$20;
            }
            }(partial_arg$20));
            var partial_arg$22 = function (param) {
              return partial_arg$21;
            };
            return function (param) {
              return partial_arg$22;
            };
          }
          var partial_arg$23 = make_iprintf(k, o, fmt$1);
          var partial_arg$24 = (function(partial_arg$23){
          return function partial_arg$24(param) {
            return partial_arg$23;
          }
          }(partial_arg$23));
          return function (param) {
            return partial_arg$24;
          };
        }
        var partial_arg$25 = make_iprintf(k, o, fmt$1);
        var partial_arg$26 = (function(partial_arg$25){
        return function partial_arg$26(param) {
          return partial_arg$25;
        }
        }(partial_arg$25));
        return function (param) {
          return partial_arg$26;
        };
    }
    switch (exit) {
      case 1 :
          var partial_arg$27 = make_iprintf(k, o, fmt._0);
          return (function(partial_arg$27){
          return function (param) {
            return partial_arg$27;
          }
          }(partial_arg$27));
      case 2 :
          _fmt = fmt._1;
          continue ;
      case 3 :
          var partial_arg$28 = make_iprintf(k, o, fmt._2);
          return (function(partial_arg$28){
          return function (param) {
            return partial_arg$28;
          }
          }(partial_arg$28));
      
    }
  };
}

function fn_of_custom_arity(k, o, fmt, arity) {
  if (!arity) {
    return make_iprintf(k, o, fmt);
  }
  var partial_arg = fn_of_custom_arity(k, o, fmt, arity._0);
  return function (param) {
    return partial_arg;
  };
}

function output_acc(o, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "number") {
      return ;
    }
    switch (acc.TAG | 0) {
      case /* Acc_formatting_lit */0 :
          var s = string_of_formatting_lit(acc._1);
          output_acc(o, acc._0);
          return Pervasives.output_string(o, s);
      case /* Acc_formatting_gen */1 :
          var acc$p = acc._1;
          var p = acc._0;
          if (acc$p.TAG === /* Acc_open_tag */0) {
            output_acc(o, p);
            Pervasives.output_string(o, "@{");
            _acc = acc$p._0;
            continue ;
          }
          output_acc(o, p);
          Pervasives.output_string(o, "@[");
          _acc = acc$p._0;
          continue ;
      case /* Acc_string_literal */2 :
      case /* Acc_data_string */4 :
          exit = 1;
          break;
      case /* Acc_char_literal */3 :
      case /* Acc_data_char */5 :
          exit = 2;
          break;
      case /* Acc_delay */6 :
          output_acc(o, acc._0);
          return Curry._1(acc._1, o);
      case /* Acc_flush */7 :
          output_acc(o, acc._0);
          return Caml_io.caml_ml_flush(o);
      case /* Acc_invalid_arg */8 :
          output_acc(o, acc._0);
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: acc._1,
                Error: new Error()
              };
      
    }
    switch (exit) {
      case 1 :
          output_acc(o, acc._0);
          return Pervasives.output_string(o, acc._1);
      case 2 :
          output_acc(o, acc._0);
          return Caml_io.caml_ml_output_char(o, acc._1);
      
    }
  };
}

function bufput_acc(b, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "number") {
      return ;
    }
    switch (acc.TAG | 0) {
      case /* Acc_formatting_lit */0 :
          var s = string_of_formatting_lit(acc._1);
          bufput_acc(b, acc._0);
          return $$Buffer.add_string(b, s);
      case /* Acc_formatting_gen */1 :
          var acc$p = acc._1;
          var p = acc._0;
          if (acc$p.TAG === /* Acc_open_tag */0) {
            bufput_acc(b, p);
            $$Buffer.add_string(b, "@{");
            _acc = acc$p._0;
            continue ;
          }
          bufput_acc(b, p);
          $$Buffer.add_string(b, "@[");
          _acc = acc$p._0;
          continue ;
      case /* Acc_string_literal */2 :
      case /* Acc_data_string */4 :
          exit = 1;
          break;
      case /* Acc_char_literal */3 :
      case /* Acc_data_char */5 :
          exit = 2;
          break;
      case /* Acc_delay */6 :
          bufput_acc(b, acc._0);
          return Curry._1(acc._1, b);
      case /* Acc_flush */7 :
          _acc = acc._0;
          continue ;
      case /* Acc_invalid_arg */8 :
          bufput_acc(b, acc._0);
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: acc._1,
                Error: new Error()
              };
      
    }
    switch (exit) {
      case 1 :
          bufput_acc(b, acc._0);
          return $$Buffer.add_string(b, acc._1);
      case 2 :
          bufput_acc(b, acc._0);
          return $$Buffer.add_char(b, acc._1);
      
    }
  };
}

function strput_acc(b, _acc) {
  while(true) {
    var acc = _acc;
    var exit = 0;
    if (typeof acc === "number") {
      return ;
    }
    switch (acc.TAG | 0) {
      case /* Acc_formatting_lit */0 :
          var s = string_of_formatting_lit(acc._1);
          strput_acc(b, acc._0);
          return $$Buffer.add_string(b, s);
      case /* Acc_formatting_gen */1 :
          var acc$p = acc._1;
          var p = acc._0;
          if (acc$p.TAG === /* Acc_open_tag */0) {
            strput_acc(b, p);
            $$Buffer.add_string(b, "@{");
            _acc = acc$p._0;
            continue ;
          }
          strput_acc(b, p);
          $$Buffer.add_string(b, "@[");
          _acc = acc$p._0;
          continue ;
      case /* Acc_string_literal */2 :
      case /* Acc_data_string */4 :
          exit = 1;
          break;
      case /* Acc_char_literal */3 :
      case /* Acc_data_char */5 :
          exit = 2;
          break;
      case /* Acc_delay */6 :
          strput_acc(b, acc._0);
          return $$Buffer.add_string(b, Curry._1(acc._1, undefined));
      case /* Acc_flush */7 :
          _acc = acc._0;
          continue ;
      case /* Acc_invalid_arg */8 :
          strput_acc(b, acc._0);
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: acc._1,
                Error: new Error()
              };
      
    }
    switch (exit) {
      case 1 :
          strput_acc(b, acc._0);
          return $$Buffer.add_string(b, acc._1);
      case 2 :
          strput_acc(b, acc._0);
          return $$Buffer.add_char(b, acc._1);
      
    }
  };
}

function failwith_message(param) {
  var buf = $$Buffer.create(256);
  var k = function (param, acc) {
    strput_acc(buf, acc);
    var s = $$Buffer.contents(buf);
    throw {
          RE_EXN_ID: "Failure",
          _1: s,
          Error: new Error()
        };
  };
  return make_printf(k, undefined, /* End_of_acc */0, param._0);
}

function open_box_of_string(str) {
  if (str === "") {
    return [
            0,
            /* Pp_box */4
          ];
  }
  var len = str.length;
  var invalid_box = function (param) {
    return Curry._1(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid box description ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "invalid box description %S"
                  }), str);
  };
  var parse_spaces = function (_i) {
    while(true) {
      var i = _i;
      if (i === len) {
        return i;
      }
      var match = Caml_string.get(str, i);
      if (match !== 9) {
        if (match !== 32) {
          return i;
        }
        _i = i + 1 | 0;
        continue ;
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  var parse_lword = function (i, _j) {
    while(true) {
      var j = _j;
      if (j === len) {
        return j;
      }
      var match = Caml_string.get(str, j);
      if (match > 122 || match < 97) {
        return j;
      }
      _j = j + 1 | 0;
      continue ;
    };
  };
  var parse_int = function (i, _j) {
    while(true) {
      var j = _j;
      if (j === len) {
        return j;
      }
      var match = Caml_string.get(str, j);
      if (match >= 48) {
        if (match >= 58) {
          return j;
        }
        _j = j + 1 | 0;
        continue ;
      }
      if (match !== 45) {
        return j;
      }
      _j = j + 1 | 0;
      continue ;
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
      if (exn.RE_EXN_ID === "Failure") {
        indent = invalid_box(undefined);
      } else {
        throw exn;
      }
    }
  }
  var exp_end = parse_spaces(nend);
  if (exp_end !== len) {
    invalid_box(undefined);
  }
  var box_type;
  switch (box_name) {
    case "" :
    case "b" :
        box_type = /* Pp_box */4;
        break;
    case "h" :
        box_type = /* Pp_hbox */0;
        break;
    case "hov" :
        box_type = /* Pp_hovbox */3;
        break;
    case "hv" :
        box_type = /* Pp_hvbox */2;
        break;
    case "v" :
        box_type = /* Pp_vbox */1;
        break;
    default:
      box_type = invalid_box(undefined);
  }
  return [
          indent,
          box_type
        ];
}

function make_padding_fmt_ebb(pad, fmt) {
  if (typeof pad === "number") {
    return /* Padding_fmt_EBB */{
            _0: /* No_padding */0,
            _1: fmt
          };
  } else if (pad.TAG === /* Lit_padding */0) {
    return /* Padding_fmt_EBB */{
            _0: {
              TAG: /* Lit_padding */0,
              _0: pad._0,
              _1: pad._1
            },
            _1: fmt
          };
  } else {
    return /* Padding_fmt_EBB */{
            _0: {
              TAG: /* Arg_padding */1,
              _0: pad._0
            },
            _1: fmt
          };
  }
}

function make_precision_fmt_ebb(prec, fmt) {
  if (typeof prec === "number") {
    if (prec !== 0) {
      return /* Precision_fmt_EBB */{
              _0: /* Arg_precision */1,
              _1: fmt
            };
    } else {
      return /* Precision_fmt_EBB */{
              _0: /* No_precision */0,
              _1: fmt
            };
    }
  } else {
    return /* Precision_fmt_EBB */{
            _0: /* Lit_precision */{
              _0: prec._0
            },
            _1: fmt
          };
  }
}

function make_padprec_fmt_ebb(pad, prec, fmt) {
  var match = make_precision_fmt_ebb(prec, fmt);
  var fmt$p = match._1;
  var prec$1 = match._0;
  if (typeof pad === "number") {
    return /* Padprec_fmt_EBB */{
            _0: /* No_padding */0,
            _1: prec$1,
            _2: fmt$p
          };
  } else if (pad.TAG === /* Lit_padding */0) {
    return /* Padprec_fmt_EBB */{
            _0: {
              TAG: /* Lit_padding */0,
              _0: pad._0,
              _1: pad._1
            },
            _1: prec$1,
            _2: fmt$p
          };
  } else {
    return /* Padprec_fmt_EBB */{
            _0: {
              TAG: /* Arg_padding */1,
              _0: pad._0
            },
            _1: prec$1,
            _2: fmt$p
          };
  }
}

function fmt_ebb_of_string(legacy_behavior, str) {
  var legacy_behavior$1 = legacy_behavior !== undefined ? legacy_behavior : true;
  var invalid_format_message = function (str_ind, msg) {
    return Curry._3(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid format ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ": at character number ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ", ",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "invalid format %S: at character number %d, %s"
                  }), str, str_ind, msg);
  };
  var invalid_format_without = function (str_ind, c, s) {
    return Curry._4(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid format ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ": at character number ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ", '",
                              _1: {
                                TAG: /* Char */0,
                                _0: {
                                  TAG: /* String_literal */11,
                                  _0: "' without ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "invalid format %S: at character number %d, '%c' without %s"
                  }), str, str_ind, c, s);
  };
  var expected_character = function (str_ind, expected, read) {
    return Curry._4(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid format ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ": at character number ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ", ",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: " expected, read ",
                                  _1: {
                                    TAG: /* Caml_char */1,
                                    _0: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "invalid format %S: at character number %d, %s expected, read %C"
                  }), str, str_ind, expected, read);
  };
  var incompatible_flag = function (pct_ind, str_ind, symb, option) {
    var subfmt = $$String.sub(str, pct_ind, str_ind - pct_ind | 0);
    return Curry._5(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid format ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ": at character number ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ", ",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: " is incompatible with '",
                                  _1: {
                                    TAG: /* Char */0,
                                    _0: {
                                      TAG: /* String_literal */11,
                                      _0: "' in sub-format ",
                                      _1: {
                                        TAG: /* Caml_string */3,
                                        _0: /* No_padding */0,
                                        _1: /* End_of_format */0
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
                    _1: "invalid format %S: at character number %d, %s is incompatible with '%c' in sub-format %S"
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
        return [
                str_ind,
                acc
              ];
      }
      var new_acc = Math.imul(acc, 10) + (c - /* '0' */48 | 0) | 0;
      _acc = new_acc;
      _str_ind = str_ind + 1 | 0;
      continue ;
    };
  };
  var parse_after_padding = function (pct_ind, str_ind, end_ind, minus, plus, hash, space, ign, pad) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var symb = Caml_string.get(str, str_ind);
    if (symb !== 46) {
      return parse_conversion(pct_ind, str_ind + 1 | 0, end_ind, plus, hash, space, ign, pad, /* No_precision */0, pad, symb);
    } else {
      var str_ind$1 = str_ind + 1 | 0;
      if (str_ind$1 === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var parse_literal = function (minus, str_ind) {
        var match = parse_positive(str_ind, end_ind, 0);
        return parse_after_precision(pct_ind, match[0], end_ind, minus, plus, hash, space, ign, pad, /* Lit_precision */{
                    _0: match[1]
                  });
      };
      var symb$1 = Caml_string.get(str, str_ind$1);
      var exit = 0;
      if (symb$1 >= 48) {
        if (symb$1 < 58) {
          return parse_literal(minus, str_ind$1);
        }
        
      } else if (symb$1 >= 42) {
        switch (symb$1) {
          case 42 :
              return parse_after_precision(pct_ind, str_ind$1 + 1 | 0, end_ind, minus, plus, hash, space, ign, pad, /* Arg_precision */1);
          case 43 :
          case 45 :
              exit = 2;
              break;
          case 44 :
          case 46 :
          case 47 :
              break;
          
        }
      }
      if (exit === 2 && legacy_behavior$1) {
        return parse_literal(minus || symb$1 === /* '-' */45, str_ind$1 + 1 | 0);
      }
      if (legacy_behavior$1) {
        return parse_after_precision(pct_ind, str_ind$1, end_ind, minus, plus, hash, space, ign, pad, /* Lit_precision */{
                    _0: 0
                  });
      } else {
        return invalid_format_without(str_ind$1 - 1 | 0, /* '.' */46, "precision");
      }
    }
  };
  var parse_spaces = function (_str_ind, end_ind) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      if (Caml_string.get(str, str_ind) !== /* ' ' */32) {
        return str_ind;
      }
      _str_ind = str_ind + 1 | 0;
      continue ;
    };
  };
  var search_subformat_end = function (_str_ind, end_ind, c) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        Curry._3(failwith_message(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "invalid format ",
                    _1: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: ": unclosed sub-format, expected \"",
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* '%' */37,
                          _1: {
                            TAG: /* Char */0,
                            _0: {
                              TAG: /* String_literal */11,
                              _0: "\" at character number ",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_d */0,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "invalid format %S: unclosed sub-format, expected \"%%%c\" at character number %d"
                }), str, c, end_ind);
      }
      var match = Caml_string.get(str, str_ind);
      if (match !== 37) {
        _str_ind = str_ind + 1 | 0;
        continue ;
      }
      if ((str_ind + 1 | 0) === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      if (Caml_string.get(str, str_ind + 1 | 0) === c) {
        return str_ind;
      }
      var match$1 = Caml_string.get(str, str_ind + 1 | 0);
      if (match$1 >= 95) {
        if (match$1 >= 123) {
          if (match$1 < 126) {
            switch (match$1) {
              case 123 :
                  var sub_end = search_subformat_end(str_ind + 2 | 0, end_ind, /* '}' */125);
                  _str_ind = sub_end + 2 | 0;
                  continue ;
              case 124 :
                  break;
              case 125 :
                  return expected_character(str_ind + 1 | 0, "character ')'", /* '}' */125);
              
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
            }
            var sub_end$1 = search_subformat_end(str_ind + 3 | 0, end_ind, /* '}' */125);
            _str_ind = sub_end$1 + 2 | 0;
            continue ;
          }
          var sub_end$2 = search_subformat_end(str_ind + 3 | 0, end_ind, /* ')' */41);
          _str_ind = sub_end$2 + 2 | 0;
          continue ;
        }
        
      } else if (match$1 !== 40) {
        if (match$1 === 41) {
          return expected_character(str_ind + 1 | 0, "character '}'", /* ')' */41);
        }
        
      } else {
        var sub_end$3 = search_subformat_end(str_ind + 2 | 0, end_ind, /* ')' */41);
        _str_ind = sub_end$3 + 2 | 0;
        continue ;
      }
      _str_ind = str_ind + 2 | 0;
      continue ;
    };
  };
  var compute_int_conv = function (pct_ind, str_ind, _plus, _hash, _space, symb) {
    while(true) {
      var space = _space;
      var hash = _hash;
      var plus = _plus;
      var exit = 0;
      if (plus) {
        if (hash) {
          exit = 2;
        } else if (!space) {
          if (symb === 100) {
            return /* Int_pd */1;
          }
          if (symb === 105) {
            return /* Int_pi */4;
          }
          
        }
        
      } else if (hash) {
        if (space) {
          exit = 2;
        } else {
          if (symb === 88) {
            return /* Int_CX */9;
          }
          if (symb === 111) {
            return /* Int_Co */11;
          }
          if (symb === 120) {
            return /* Int_Cx */7;
          }
          exit = 2;
        }
      } else if (space) {
        if (symb === 100) {
          return /* Int_sd */2;
        }
        if (symb === 105) {
          return /* Int_si */5;
        }
        
      } else {
        switch (symb) {
          case 88 :
              return /* Int_X */8;
          case 100 :
              return /* Int_d */0;
          case 105 :
              return /* Int_i */3;
          case 111 :
              return /* Int_o */10;
          case 117 :
              return /* Int_u */12;
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
              return /* Int_x */6;
          default:
            
        }
      }
      if (exit === 2) {
        var exit$1 = 0;
        switch (symb) {
          case 88 :
              if (legacy_behavior$1) {
                return /* Int_CX */9;
              }
              break;
          case 111 :
              if (legacy_behavior$1) {
                return /* Int_Co */11;
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
                return /* Int_Cx */7;
              }
              break;
          default:
            
        }
        if (exit$1 === 3) {
          if (!legacy_behavior$1) {
            return incompatible_flag(pct_ind, str_ind, symb, "'#'");
          }
          _hash = false;
          continue ;
        }
        
      }
      if (plus) {
        if (space) {
          if (!legacy_behavior$1) {
            return incompatible_flag(pct_ind, str_ind, /* ' ' */32, "'+'");
          }
          _space = false;
          continue ;
        }
        if (!legacy_behavior$1) {
          return incompatible_flag(pct_ind, str_ind, symb, "'+'");
        }
        _plus = false;
        continue ;
      }
      if (space) {
        if (!legacy_behavior$1) {
          return incompatible_flag(pct_ind, str_ind, symb, "' '");
        }
        _space = false;
        continue ;
      }
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "camlinternalFormat.ml",
              2909,
              28
            ],
            Error: new Error()
          };
    };
  };
  var parse_literal = function (lit_start, _str_ind, end_ind) {
    while(true) {
      var str_ind = _str_ind;
      if (str_ind === end_ind) {
        return add_literal(lit_start, str_ind, /* End_of_format */0);
      }
      var match = Caml_string.get(str, str_ind);
      if (match !== 37) {
        if (match !== 64) {
          _str_ind = str_ind + 1 | 0;
          continue ;
        }
        var fmt_rest = parse_after_at(str_ind + 1 | 0, end_ind);
        return add_literal(lit_start, str_ind, fmt_rest._0);
      }
      var fmt_rest$1 = parse_format(str_ind, end_ind);
      return add_literal(lit_start, str_ind, fmt_rest$1._0);
    };
  };
  var parse_integer = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var match = Caml_string.get(str, str_ind);
    if (match >= 48) {
      if (match >= 58) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                2814,
                11
              ],
              Error: new Error()
            };
      }
      return parse_positive(str_ind, end_ind, 0);
    }
    if (match !== 45) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "camlinternalFormat.ml",
              2814,
              11
            ],
            Error: new Error()
          };
    }
    if ((str_ind + 1 | 0) === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var c = Caml_string.get(str, str_ind + 1 | 0);
    if (c > 57 || c < 48) {
      return expected_character(str_ind + 1 | 0, "digit", c);
    }
    var match$1 = parse_positive(str_ind + 1 | 0, end_ind, 0);
    return [
            match$1[0],
            -match$1[1] | 0
          ];
  };
  var parse_after_precision = function (pct_ind, str_ind, end_ind, minus, plus, hash, space, ign, pad, prec) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var parse_conv = function (padprec) {
      return parse_conversion(pct_ind, str_ind + 1 | 0, end_ind, plus, hash, space, ign, pad, prec, padprec, Caml_string.get(str, str_ind));
    };
    if (typeof pad === "number") {
      if (prec === 0) {
        return parse_conv(/* No_padding */0);
      } else if (minus) {
        if (typeof prec === "number") {
          return parse_conv({
                      TAG: /* Arg_padding */1,
                      _0: /* Left */0
                    });
        } else {
          return parse_conv({
                      TAG: /* Lit_padding */0,
                      _0: /* Left */0,
                      _1: prec._0
                    });
        }
      } else if (typeof prec === "number") {
        return parse_conv({
                    TAG: /* Arg_padding */1,
                    _0: /* Right */1
                  });
      } else {
        return parse_conv({
                    TAG: /* Lit_padding */0,
                    _0: /* Right */1,
                    _1: prec._0
                  });
      }
    } else {
      return parse_conv(pad);
    }
  };
  var parse_flags = function (pct_ind, str_ind, end_ind, ign) {
    var zero = {
      contents: false
    };
    var minus = {
      contents: false
    };
    var plus = {
      contents: false
    };
    var space = {
      contents: false
    };
    var hash = {
      contents: false
    };
    var set_flag = function (str_ind, flag) {
      if (flag.contents && !legacy_behavior$1) {
        Curry._3(failwith_message(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "invalid format ",
                    _1: {
                      TAG: /* Caml_string */3,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: ": at character number ",
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: ", duplicate flag ",
                            _1: {
                              TAG: /* Caml_char */1,
                              _0: /* End_of_format */0
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "invalid format %S: at character number %d, duplicate flag %C"
                }), str, str_ind, Caml_string.get(str, str_ind));
      }
      flag.contents = true;
      
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
            set_flag(str_ind$1, hash);
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
      var zero$1 = zero.contents;
      var minus$1 = minus.contents;
      var plus$1 = plus.contents;
      var hash$1 = hash.contents;
      var space$1 = space.contents;
      if (str_ind$1 === end_ind) {
        invalid_format_message(end_ind, "unexpected end of format");
      }
      var padty = zero$1 ? (
          minus$1 ? (
              legacy_behavior$1 ? /* Left */0 : incompatible_flag(pct_ind, str_ind$1, /* '-' */45, "0")
            ) : /* Zeros */2
        ) : (
          minus$1 ? /* Left */0 : /* Right */1
        );
      var match$1 = Caml_string.get(str, str_ind$1);
      if (match$1 >= 48) {
        if (match$1 < 58) {
          var match$2 = parse_positive(str_ind$1, end_ind, 0);
          return parse_after_padding(pct_ind, match$2[0], end_ind, minus$1, plus$1, hash$1, space$1, ign, {
                      TAG: /* Lit_padding */0,
                      _0: padty,
                      _1: match$2[1]
                    });
        }
        
      } else if (match$1 === 42) {
        return parse_after_padding(pct_ind, str_ind$1 + 1 | 0, end_ind, minus$1, plus$1, hash$1, space$1, ign, {
                    TAG: /* Arg_padding */1,
                    _0: padty
                  });
      }
      switch (padty) {
        case /* Left */0 :
            if (!legacy_behavior$1) {
              invalid_format_without(str_ind$1 - 1 | 0, /* '-' */45, "padding");
            }
            return parse_after_padding(pct_ind, str_ind$1, end_ind, minus$1, plus$1, hash$1, space$1, ign, /* No_padding */0);
        case /* Right */1 :
            return parse_after_padding(pct_ind, str_ind$1, end_ind, minus$1, plus$1, hash$1, space$1, ign, /* No_padding */0);
        case /* Zeros */2 :
            return parse_after_padding(pct_ind, str_ind$1, end_ind, minus$1, plus$1, hash$1, space$1, ign, {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 0
                      });
        
      }
    };
  };
  var parse_conversion = function (pct_ind, str_ind, end_ind, plus, hash, space, ign, pad, prec, padprec, symb) {
    var plus_used = false;
    var hash_used = false;
    var space_used = false;
    var ign_used = {
      contents: false
    };
    var pad_used = {
      contents: false
    };
    var prec_used = {
      contents: false
    };
    var get_int_pad = function (param) {
      pad_used.contents = true;
      prec_used.contents = true;
      if (prec === 0) {
        return pad;
      } else if (typeof pad === "number") {
        return /* No_padding */0;
      } else if (pad.TAG === /* Lit_padding */0) {
        if (pad._0 >= 2) {
          if (legacy_behavior$1) {
            return {
                    TAG: /* Lit_padding */0,
                    _0: /* Right */1,
                    _1: pad._1
                  };
          } else {
            return incompatible_flag(pct_ind, str_ind, /* '0' */48, "precision");
          }
        } else {
          return pad;
        }
      } else if (pad._0 >= 2) {
        if (legacy_behavior$1) {
          return {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                };
        } else {
          return incompatible_flag(pct_ind, str_ind, /* '0' */48, "precision");
        }
      } else {
        return pad;
      }
    };
    var check_no_0 = function (symb, pad) {
      if (typeof pad === "number") {
        return pad;
      } else if (pad.TAG === /* Lit_padding */0) {
        if (pad._0 >= 2) {
          if (legacy_behavior$1) {
            return {
                    TAG: /* Lit_padding */0,
                    _0: /* Right */1,
                    _1: pad._1
                  };
          } else {
            return incompatible_flag(pct_ind, str_ind, symb, "0");
          }
        } else {
          return pad;
        }
      } else if (pad._0 >= 2) {
        if (legacy_behavior$1) {
          return {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                };
        } else {
          return incompatible_flag(pct_ind, str_ind, symb, "0");
        }
      } else {
        return pad;
      }
    };
    var opt_of_pad = function (c, pad) {
      if (typeof pad === "number") {
        return ;
      }
      if (pad.TAG !== /* Lit_padding */0) {
        return incompatible_flag(pct_ind, str_ind, c, "'*'");
      }
      switch (pad._0) {
        case /* Left */0 :
            if (legacy_behavior$1) {
              return pad._1;
            } else {
              return incompatible_flag(pct_ind, str_ind, c, "'-'");
            }
        case /* Right */1 :
            return pad._1;
        case /* Zeros */2 :
            if (legacy_behavior$1) {
              return pad._1;
            } else {
              return incompatible_flag(pct_ind, str_ind, c, "'0'");
            }
        
      }
    };
    var get_prec_opt = function (param) {
      prec_used.contents = true;
      if (typeof prec === "number") {
        if (prec !== 0) {
          return incompatible_flag(pct_ind, str_ind, /* '_' */95, "'*'");
        } else {
          return ;
        }
      } else {
        return prec._0;
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
            var fmt_rest = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Flush */10,
                _0: fmt_rest._0
              }
            };
            break;
        case 40 :
            var sub_end = search_subformat_end(str_ind, end_ind, /* ')' */41);
            var beg_ind = sub_end + 2 | 0;
            var fmt_rest$1 = parse_literal(beg_ind, beg_ind, end_ind);
            var fmt_rest$2 = fmt_rest$1._0;
            var sub_fmt = parse_literal(str_ind, str_ind, sub_end);
            var sub_fmtty = fmtty_of_fmt(sub_fmt._0);
            if (ign_used.contents = true, ign) {
              var ignored_0 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
              var ignored = {
                TAG: /* Ignored_format_subst */9,
                _0: ignored_0,
                _1: sub_fmtty
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored,
                  _1: fmt_rest$2
                }
              };
            } else {
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Format_subst */14,
                  _0: opt_of_pad(/* '(' */40, (pad_used.contents = true, pad)),
                  _1: sub_fmtty,
                  _2: fmt_rest$2
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
            var fmt_rest$3 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$4 = fmt_rest$3._0;
            fmt_result = (ign_used.contents = true, ign) ? /* Fmt_EBB */({
                  _0: {
                    TAG: /* Ignored_param */23,
                    _0: /* Ignored_caml_char */1,
                    _1: fmt_rest$4
                  }
                }) : /* Fmt_EBB */({
                  _0: {
                    TAG: /* Caml_char */1,
                    _0: fmt_rest$4
                  }
                });
            break;
        case 78 :
            var fmt_rest$5 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$6 = fmt_rest$5._0;
            if (ign_used.contents = true, ign) {
              var ignored$1 = {
                TAG: /* Ignored_scan_get_counter */11,
                _0: /* Token_counter */2
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$1,
                  _1: fmt_rest$6
                }
              };
            } else {
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Scan_get_counter */21,
                  _0: /* Token_counter */2,
                  _1: fmt_rest$6
                }
              };
            }
            break;
        case 83 :
            var pad$1 = check_no_0(symb, (pad_used.contents = true, padprec));
            var fmt_rest$7 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$8 = fmt_rest$7._0;
            if (ign_used.contents = true, ign) {
              var ignored$2 = {
                TAG: /* Ignored_caml_string */1,
                _0: opt_of_pad(/* '_' */95, (pad_used.contents = true, padprec))
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$2,
                  _1: fmt_rest$8
                }
              };
            } else {
              var match = make_padding_fmt_ebb(pad$1, fmt_rest$8);
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Caml_string */3,
                  _0: match._0,
                  _1: match._1
                }
              };
            }
            break;
        case 91 :
            var match$1 = parse_char_set(str_ind, end_ind);
            var char_set = match$1[1];
            var next_ind = match$1[0];
            var fmt_rest$9 = parse_literal(next_ind, next_ind, end_ind);
            var fmt_rest$10 = fmt_rest$9._0;
            if (ign_used.contents = true, ign) {
              var ignored_0$1 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
              var ignored$3 = {
                TAG: /* Ignored_scan_char_set */10,
                _0: ignored_0$1,
                _1: char_set
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$3,
                  _1: fmt_rest$10
                }
              };
            } else {
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Scan_char_set */20,
                  _0: opt_of_pad(/* '[' */91, (pad_used.contents = true, pad)),
                  _1: char_set,
                  _2: fmt_rest$10
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
            var fmt_rest$11 = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Alpha */15,
                _0: fmt_rest$11._0
              }
            };
            break;
        case 66 :
        case 98 :
            exit$1 = 3;
            break;
        case 99 :
            var char_format = function (fmt_rest) {
              if (ign_used.contents = true, ign) {
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Ignored_param */23,
                          _0: /* Ignored_char */0,
                          _1: fmt_rest
                        }
                      };
              } else {
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Char */0,
                          _0: fmt_rest
                        }
                      };
              }
            };
            var scan_format = function (fmt_rest) {
              if (ign_used.contents = true, ign) {
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Ignored_param */23,
                          _0: /* Ignored_scan_next_char */3,
                          _1: fmt_rest
                        }
                      };
              } else {
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Scan_next_char */22,
                          _0: fmt_rest
                        }
                      };
              }
            };
            var fmt_rest$12 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$13 = fmt_rest$12._0;
            var _n = opt_of_pad(/* 'c' */99, (pad_used.contents = true, pad));
            fmt_result = _n !== undefined ? (
                _n !== 0 ? (
                    legacy_behavior$1 ? char_format(fmt_rest$13) : invalid_format_message(str_ind, "non-zero widths are unsupported for %c conversions")
                  ) : scan_format(fmt_rest$13)
              ) : char_format(fmt_rest$13);
            break;
        case 69 :
        case 70 :
        case 71 :
        case 72 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
            exit$1 = 2;
            break;
        case 76 :
        case 108 :
        case 110 :
            exit$2 = 8;
            break;
        case 114 :
            var fmt_rest$14 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$15 = fmt_rest$14._0;
            fmt_result = (ign_used.contents = true, ign) ? /* Fmt_EBB */({
                  _0: {
                    TAG: /* Ignored_param */23,
                    _0: /* Ignored_reader */2,
                    _1: fmt_rest$15
                  }
                }) : /* Fmt_EBB */({
                  _0: {
                    TAG: /* Reader */19,
                    _0: fmt_rest$15
                  }
                });
            break;
        case 115 :
            var pad$2 = check_no_0(symb, (pad_used.contents = true, padprec));
            var fmt_rest$16 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$17 = fmt_rest$16._0;
            if (ign_used.contents = true, ign) {
              var ignored$4 = {
                TAG: /* Ignored_string */0,
                _0: opt_of_pad(/* '_' */95, (pad_used.contents = true, padprec))
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$4,
                  _1: fmt_rest$17
                }
              };
            } else {
              var match$2 = make_padding_fmt_ebb(pad$2, fmt_rest$17);
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* String */2,
                  _0: match$2._0,
                  _1: match$2._1
                }
              };
            }
            break;
        case 116 :
            var fmt_rest$18 = parse_literal(str_ind, str_ind, end_ind);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Theta */16,
                _0: fmt_rest$18._0
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
            var sub_end$1 = search_subformat_end(str_ind, end_ind, /* '}' */125);
            var sub_fmt$1 = parse_literal(str_ind, str_ind, sub_end$1);
            var beg_ind$1 = sub_end$1 + 2 | 0;
            var fmt_rest$19 = parse_literal(beg_ind$1, beg_ind$1, end_ind);
            var fmt_rest$20 = fmt_rest$19._0;
            var sub_fmtty$1 = fmtty_of_fmt(sub_fmt$1._0);
            if (ign_used.contents = true, ign) {
              var ignored_0$2 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
              var ignored$5 = {
                TAG: /* Ignored_format_arg */8,
                _0: ignored_0$2,
                _1: sub_fmtty$1
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$5,
                  _1: fmt_rest$20
                }
              };
            } else {
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Format_arg */13,
                  _0: opt_of_pad(/* '{' */123, (pad_used.contents = true, pad)),
                  _1: sub_fmtty$1,
                  _2: fmt_rest$20
                }
              };
            }
            break;
        
      }
    }
    switch (exit$2) {
      case 7 :
          plus_used = true;
          hash_used = true;
          space_used = true;
          var iconv = compute_int_conv(pct_ind, str_ind, plus, hash, space, symb);
          var fmt_rest$21 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$22 = fmt_rest$21._0;
          if (ign_used.contents = true, ign) {
            var ignored_1 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
            var ignored$6 = {
              TAG: /* Ignored_int */2,
              _0: iconv,
              _1: ignored_1
            };
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Ignored_param */23,
                _0: ignored$6,
                _1: fmt_rest$22
              }
            };
          } else {
            var match$3 = make_padprec_fmt_ebb(get_int_pad(undefined), (prec_used.contents = true, prec), fmt_rest$22);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Int */4,
                _0: iconv,
                _1: match$3._0,
                _2: match$3._1,
                _3: match$3._2
              }
            };
          }
          break;
      case 8 :
          if (str_ind === end_ind || !is_int_base(Caml_string.get(str, str_ind))) {
            var fmt_rest$23 = parse_literal(str_ind, str_ind, end_ind);
            var fmt_rest$24 = fmt_rest$23._0;
            var counter = counter_of_char(symb);
            if (ign_used.contents = true, ign) {
              var ignored$7 = {
                TAG: /* Ignored_scan_get_counter */11,
                _0: counter
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$7,
                  _1: fmt_rest$24
                }
              };
            } else {
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Scan_get_counter */21,
                  _0: counter,
                  _1: fmt_rest$24
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
          var fmt_rest$25 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$26 = fmt_rest$25._0;
          if (ign_used.contents = true, ign) {
            var ignored_0$3 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
            var ignored_1$1 = get_prec_opt(undefined);
            var ignored$8 = {
              TAG: /* Ignored_float */6,
              _0: ignored_0$3,
              _1: ignored_1$1
            };
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Ignored_param */23,
                _0: ignored$8,
                _1: fmt_rest$26
              }
            };
          } else {
            var match$4 = make_padprec_fmt_ebb((pad_used.contents = true, pad), (prec_used.contents = true, prec), fmt_rest$26);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Float */8,
                _0: fconv,
                _1: match$4._0,
                _2: match$4._1,
                _3: match$4._2
              }
            };
          }
          break;
      case 3 :
          var pad$3 = check_no_0(symb, (pad_used.contents = true, padprec));
          var fmt_rest$27 = parse_literal(str_ind, str_ind, end_ind);
          var fmt_rest$28 = fmt_rest$27._0;
          if (ign_used.contents = true, ign) {
            var ignored$9 = {
              TAG: /* Ignored_bool */7,
              _0: opt_of_pad(/* '_' */95, (pad_used.contents = true, padprec))
            };
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Ignored_param */23,
                _0: ignored$9,
                _1: fmt_rest$28
              }
            };
          } else {
            var match$5 = make_padding_fmt_ebb(pad$3, fmt_rest$28);
            fmt_result = /* Fmt_EBB */{
              _0: {
                TAG: /* Bool */9,
                _0: match$5._0,
                _1: match$5._1
              }
            };
          }
          break;
      case 4 :
          var fmt_rest$29 = parse_literal(str_ind, str_ind, end_ind);
          fmt_result = /* Fmt_EBB */{
            _0: {
              TAG: /* Char_literal */12,
              _0: symb,
              _1: fmt_rest$29._0
            }
          };
          break;
      case 5 :
          fmt_result = Curry._3(failwith_message(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "invalid format ",
                      _1: {
                        TAG: /* Caml_string */3,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: ": at character number ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ", flag ",
                              _1: {
                                TAG: /* Caml_char */1,
                                _0: {
                                  TAG: /* String_literal */11,
                                  _0: " is only allowed after the '",
                                  _1: {
                                    TAG: /* Char_literal */12,
                                    _0: /* '%' */37,
                                    _1: {
                                      TAG: /* String_literal */11,
                                      _0: "', before padding and precision",
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    _1: "invalid format %S: at character number %d, flag %C is only allowed after the '%%', before padding and precision"
                  }), str, pct_ind, symb);
          break;
      case 6 :
          if (symb >= 108) {
            if (symb >= 111) {
              exit = 1;
            } else {
              switch (symb) {
                case 108 :
                    plus_used = true;
                    hash_used = true;
                    space_used = true;
                    var iconv$1 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, hash, space, Caml_string.get(str, str_ind));
                    var beg_ind$2 = str_ind + 1 | 0;
                    var fmt_rest$30 = parse_literal(beg_ind$2, beg_ind$2, end_ind);
                    var fmt_rest$31 = fmt_rest$30._0;
                    if (ign_used.contents = true, ign) {
                      var ignored_1$2 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
                      var ignored$10 = {
                        TAG: /* Ignored_int32 */3,
                        _0: iconv$1,
                        _1: ignored_1$2
                      };
                      fmt_result = /* Fmt_EBB */{
                        _0: {
                          TAG: /* Ignored_param */23,
                          _0: ignored$10,
                          _1: fmt_rest$31
                        }
                      };
                    } else {
                      var match$6 = make_padprec_fmt_ebb(get_int_pad(undefined), (prec_used.contents = true, prec), fmt_rest$31);
                      fmt_result = /* Fmt_EBB */{
                        _0: {
                          TAG: /* Int32 */5,
                          _0: iconv$1,
                          _1: match$6._0,
                          _2: match$6._1,
                          _3: match$6._2
                        }
                      };
                    }
                    break;
                case 109 :
                    exit = 1;
                    break;
                case 110 :
                    plus_used = true;
                    hash_used = true;
                    space_used = true;
                    var iconv$2 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, hash, space, Caml_string.get(str, str_ind));
                    var beg_ind$3 = str_ind + 1 | 0;
                    var fmt_rest$32 = parse_literal(beg_ind$3, beg_ind$3, end_ind);
                    var fmt_rest$33 = fmt_rest$32._0;
                    if (ign_used.contents = true, ign) {
                      var ignored_1$3 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
                      var ignored$11 = {
                        TAG: /* Ignored_nativeint */4,
                        _0: iconv$2,
                        _1: ignored_1$3
                      };
                      fmt_result = /* Fmt_EBB */{
                        _0: {
                          TAG: /* Ignored_param */23,
                          _0: ignored$11,
                          _1: fmt_rest$33
                        }
                      };
                    } else {
                      var match$7 = make_padprec_fmt_ebb(get_int_pad(undefined), (prec_used.contents = true, prec), fmt_rest$33);
                      fmt_result = /* Fmt_EBB */{
                        _0: {
                          TAG: /* Nativeint */6,
                          _0: iconv$2,
                          _1: match$7._0,
                          _2: match$7._1,
                          _3: match$7._2
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
            hash_used = true;
            space_used = true;
            var iconv$3 = compute_int_conv(pct_ind, str_ind + 1 | 0, plus, hash, space, Caml_string.get(str, str_ind));
            var beg_ind$4 = str_ind + 1 | 0;
            var fmt_rest$34 = parse_literal(beg_ind$4, beg_ind$4, end_ind);
            var fmt_rest$35 = fmt_rest$34._0;
            if (ign_used.contents = true, ign) {
              var ignored_1$4 = opt_of_pad(/* '_' */95, (pad_used.contents = true, pad));
              var ignored$12 = {
                TAG: /* Ignored_int64 */5,
                _0: iconv$3,
                _1: ignored_1$4
              };
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Ignored_param */23,
                  _0: ignored$12,
                  _1: fmt_rest$35
                }
              };
            } else {
              var match$8 = make_padprec_fmt_ebb(get_int_pad(undefined), (prec_used.contents = true, prec), fmt_rest$35);
              fmt_result = /* Fmt_EBB */{
                _0: {
                  TAG: /* Int64 */7,
                  _0: iconv$3,
                  _1: match$8._0,
                  _2: match$8._1,
                  _3: match$8._2
                }
              };
            }
          }
          break;
      
    }
    if (exit === 1) {
      fmt_result = Curry._3(failwith_message(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "invalid format ",
                  _1: {
                    TAG: /* Caml_string */3,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: ": at character number ",
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_d */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* String_literal */11,
                          _0: ", invalid conversion \"",
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* '%' */37,
                            _1: {
                              TAG: /* Char */0,
                              _0: {
                                TAG: /* Char_literal */12,
                                _0: /* '"' */34,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                },
                _1: "invalid format %S: at character number %d, invalid conversion \"%%%c\""
              }), str, str_ind - 1 | 0, symb);
    }
    if (!legacy_behavior$1) {
      if (!plus_used && plus) {
        incompatible_flag(pct_ind, str_ind, symb, "'+'");
      }
      if (!hash_used && hash) {
        incompatible_flag(pct_ind, str_ind, symb, "'#'");
      }
      if (!space_used && space) {
        incompatible_flag(pct_ind, str_ind, symb, "' '");
      }
      if (!pad_used.contents && Caml_obj.caml_notequal(/* Padding_EBB */{
              _0: pad
            }, /* Padding_EBB */{
              _0: /* No_padding */0
            })) {
        incompatible_flag(pct_ind, str_ind, symb, "`padding'");
      }
      if (!prec_used.contents && Caml_obj.caml_notequal(/* Precision_EBB */{
              _0: prec
            }, /* Precision_EBB */{
              _0: /* No_precision */0
            })) {
        incompatible_flag(pct_ind, str_ind, ign ? /* '_' */95 : symb, "`precision'");
      }
      if (ign && plus) {
        incompatible_flag(pct_ind, str_ind, /* '_' */95, "'+'");
      }
      
    }
    if (!ign_used.contents && ign) {
      var exit$3 = 0;
      if (symb >= 38) {
        if (symb !== 44) {
          if (symb !== 64 || !legacy_behavior$1) {
            exit$3 = 1;
          }
          
        } else if (!legacy_behavior$1) {
          exit$3 = 1;
        }
        
      } else if (symb !== 33) {
        if (!(symb >= 37 && legacy_behavior$1)) {
          exit$3 = 1;
        }
        
      } else if (!legacy_behavior$1) {
        exit$3 = 1;
      }
      if (exit$3 === 1) {
        incompatible_flag(pct_ind, str_ind, symb, "'_'");
      }
      
    }
    return fmt_result;
  };
  var parse_tag = function (is_open_tag, str_ind, end_ind) {
    try {
      if (str_ind === end_ind) {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var match = Caml_string.get(str, str_ind);
      if (match !== 60) {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var ind = $$String.index_from(str, str_ind + 1 | 0, /* '>' */62);
      if (ind >= end_ind) {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var sub_str = $$String.sub(str, str_ind, (ind - str_ind | 0) + 1 | 0);
      var beg_ind = ind + 1 | 0;
      var fmt_rest = parse_literal(beg_ind, beg_ind, end_ind);
      var sub_fmt = parse_literal(str_ind, str_ind, ind + 1 | 0);
      var sub_fmt$1 = sub_fmt._0;
      var sub_format = /* Format */{
        _0: sub_fmt$1,
        _1: sub_str
      };
      var formatting = is_open_tag ? ({
            TAG: /* Open_tag */0,
            _0: sub_format
          }) : (check_open_box(sub_fmt$1), {
            TAG: /* Open_box */1,
            _0: sub_format
          });
      return /* Fmt_EBB */{
              _0: {
                TAG: /* Formatting_gen */18,
                _0: formatting,
                _1: fmt_rest._0
              }
            };
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var fmt_rest$1 = parse_literal(str_ind, str_ind, end_ind);
        var sub_format$1 = /* Format */{
          _0: /* End_of_format */0,
          _1: ""
        };
        var formatting$1 = is_open_tag ? ({
              TAG: /* Open_tag */0,
              _0: sub_format$1
            }) : ({
              TAG: /* Open_box */1,
              _0: sub_format$1
            });
        return /* Fmt_EBB */{
                _0: {
                  TAG: /* Formatting_gen */18,
                  _0: formatting$1,
                  _1: fmt_rest$1._0
                }
              };
      }
      throw exn;
    }
  };
  var compute_float_conv = function (pct_ind, str_ind, _plus, _space, symb) {
    while(true) {
      var space = _space;
      var plus = _plus;
      if (plus) {
        if (space) {
          if (!legacy_behavior$1) {
            return incompatible_flag(pct_ind, str_ind, /* ' ' */32, "'+'");
          }
          _space = false;
          continue ;
        }
        if (symb >= 73) {
          switch (symb) {
            case 101 :
                return /* Float_pe */4;
            case 102 :
                return /* Float_pf */1;
            case 103 :
                return /* Float_pg */10;
            case 104 :
                return /* Float_ph */17;
            default:
              
          }
        } else if (symb >= 69) {
          switch (symb) {
            case 69 :
                return /* Float_pE */7;
            case 70 :
                break;
            case 71 :
                return /* Float_pG */13;
            case 72 :
                return /* Float_pH */20;
            
          }
        }
        if (!legacy_behavior$1) {
          return incompatible_flag(pct_ind, str_ind, symb, "'+'");
        }
        _plus = false;
        continue ;
      }
      if (space) {
        if (symb >= 73) {
          switch (symb) {
            case 101 :
                return /* Float_se */5;
            case 102 :
                return /* Float_sf */2;
            case 103 :
                return /* Float_sg */11;
            case 104 :
                return /* Float_sh */18;
            default:
              
          }
        } else if (symb >= 69) {
          switch (symb) {
            case 69 :
                return /* Float_sE */8;
            case 70 :
                break;
            case 71 :
                return /* Float_sG */14;
            case 72 :
                return /* Float_sH */21;
            
          }
        }
        if (!legacy_behavior$1) {
          return incompatible_flag(pct_ind, str_ind, symb, "' '");
        }
        _space = false;
        continue ;
      }
      if (symb >= 73) {
        switch (symb) {
          case 101 :
              return /* Float_e */3;
          case 102 :
              return /* Float_f */0;
          case 103 :
              return /* Float_g */9;
          case 104 :
              return /* Float_h */16;
          default:
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "camlinternalFormat.ml",
                    2943,
                    25
                  ],
                  Error: new Error()
                };
        }
      } else if (symb >= 69) {
        switch (symb) {
          case 69 :
              return /* Float_E */6;
          case 70 :
              return /* Float_F */15;
          case 71 :
              return /* Float_G */12;
          case 72 :
              return /* Float_H */19;
          
        }
      } else {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "camlinternalFormat.ml",
                2943,
                25
              ],
              Error: new Error()
            };
      }
    };
  };
  var parse_format = function (pct_ind, end_ind) {
    var str_ind = pct_ind + 1 | 0;
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var match = Caml_string.get(str, str_ind);
    if (match !== 95) {
      return parse_flags(pct_ind, str_ind, end_ind, false);
    } else {
      return parse_flags(pct_ind, str_ind + 1 | 0, end_ind, true);
    }
  };
  var parse_after_at = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      return /* Fmt_EBB */{
              _0: {
                TAG: /* Char_literal */12,
                _0: /* '@' */64,
                _1: /* End_of_format */0
              }
            };
    }
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
              var fmt_rest = parse_literal(beg_ind, beg_ind, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Close_tag */1,
                        _1: fmt_rest._0
                      }
                    };
          default:
            
        }
      } else if (c >= 91) {
        switch (c) {
          case 91 :
              return parse_tag(false, str_ind + 1 | 0, end_ind);
          case 92 :
              break;
          case 93 :
              var beg_ind$1 = str_ind + 1 | 0;
              var fmt_rest$1 = parse_literal(beg_ind$1, beg_ind$1, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Close_box */0,
                        _1: fmt_rest$1._0
                      }
                    };
          
        }
      }
      
    } else if (c !== 10) {
      if (c >= 32) {
        switch (c) {
          case 32 :
              var beg_ind$2 = str_ind + 1 | 0;
              var fmt_rest$2 = parse_literal(beg_ind$2, beg_ind$2, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Break */0,
                          _0: "@ ",
                          _1: 1,
                          _2: 0
                        },
                        _1: fmt_rest$2._0
                      }
                    };
          case 37 :
              if ((str_ind + 1 | 0) < end_ind && Caml_string.get(str, str_ind + 1 | 0) === /* '%' */37) {
                var beg_ind$3 = str_ind + 2 | 0;
                var fmt_rest$3 = parse_literal(beg_ind$3, beg_ind$3, end_ind);
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Formatting_lit */17,
                          _0: /* Escaped_percent */6,
                          _1: fmt_rest$3._0
                        }
                      };
              }
              var fmt_rest$4 = parse_literal(str_ind, str_ind, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Char_literal */12,
                        _0: /* '@' */64,
                        _1: fmt_rest$4._0
                      }
                    };
          case 44 :
              var beg_ind$4 = str_ind + 1 | 0;
              var fmt_rest$5 = parse_literal(beg_ind$4, beg_ind$4, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Break */0,
                          _0: "@,",
                          _1: 0,
                          _2: 0
                        },
                        _1: fmt_rest$5._0
                      }
                    };
          case 46 :
              var beg_ind$5 = str_ind + 1 | 0;
              var fmt_rest$6 = parse_literal(beg_ind$5, beg_ind$5, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Flush_newline */4,
                        _1: fmt_rest$6._0
                      }
                    };
          case 59 :
              var str_ind$1 = str_ind + 1 | 0;
              var match;
              try {
                if (str_ind$1 === end_ind || Caml_string.get(str, str_ind$1) !== /* '<' */60) {
                  throw {
                        RE_EXN_ID: "Not_found",
                        Error: new Error()
                      };
                }
                var str_ind_1 = parse_spaces(str_ind$1 + 1 | 0, end_ind);
                var match$1 = Caml_string.get(str, str_ind_1);
                var exit = 0;
                if (match$1 >= 48) {
                  if (match$1 >= 58) {
                    throw {
                          RE_EXN_ID: "Not_found",
                          Error: new Error()
                        };
                  }
                  exit = 1;
                } else {
                  if (match$1 !== 45) {
                    throw {
                          RE_EXN_ID: "Not_found",
                          Error: new Error()
                        };
                  }
                  exit = 1;
                }
                if (exit === 1) {
                  var match$2 = parse_integer(str_ind_1, end_ind);
                  var width = match$2[1];
                  var str_ind_3 = parse_spaces(match$2[0], end_ind);
                  var match$3 = Caml_string.get(str, str_ind_3);
                  if (match$3 > 57 || match$3 < 45) {
                    if (match$3 !== 62) {
                      throw {
                            RE_EXN_ID: "Not_found",
                            Error: new Error()
                          };
                    }
                    var s = $$String.sub(str, str_ind$1 - 2 | 0, (str_ind_3 - str_ind$1 | 0) + 3 | 0);
                    match = [
                      str_ind_3 + 1 | 0,
                      {
                        TAG: /* Break */0,
                        _0: s,
                        _1: width,
                        _2: 0
                      }
                    ];
                  } else {
                    if (match$3 === 47 || match$3 === 46) {
                      throw {
                            RE_EXN_ID: "Not_found",
                            Error: new Error()
                          };
                    }
                    var match$4 = parse_integer(str_ind_3, end_ind);
                    var str_ind_5 = parse_spaces(match$4[0], end_ind);
                    if (Caml_string.get(str, str_ind_5) !== /* '>' */62) {
                      throw {
                            RE_EXN_ID: "Not_found",
                            Error: new Error()
                          };
                    }
                    var s$1 = $$String.sub(str, str_ind$1 - 2 | 0, (str_ind_5 - str_ind$1 | 0) + 3 | 0);
                    match = [
                      str_ind_5 + 1 | 0,
                      {
                        TAG: /* Break */0,
                        _0: s$1,
                        _1: width,
                        _2: match$4[1]
                      }
                    ];
                  }
                }
                
              }
              catch (raw_exn){
                var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                if (exn.RE_EXN_ID === "Not_found" || exn.RE_EXN_ID === "Failure") {
                  match = [
                    str_ind$1,
                    {
                      TAG: /* Break */0,
                      _0: "@;",
                      _1: 1,
                      _2: 0
                    }
                  ];
                } else {
                  throw exn;
                }
              }
              var next_ind = match[0];
              var fmt_rest$7 = parse_literal(next_ind, next_ind, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: match[1],
                        _1: fmt_rest$7._0
                      }
                    };
          case 60 :
              var str_ind$2 = str_ind + 1 | 0;
              var match$5;
              try {
                var str_ind_1$1 = parse_spaces(str_ind$2, end_ind);
                var match$6 = Caml_string.get(str, str_ind_1$1);
                var exit$1 = 0;
                if (match$6 >= 48) {
                  if (match$6 >= 58) {
                    match$5 = undefined;
                  } else {
                    exit$1 = 1;
                  }
                } else if (match$6 !== 45) {
                  match$5 = undefined;
                } else {
                  exit$1 = 1;
                }
                if (exit$1 === 1) {
                  var match$7 = parse_integer(str_ind_1$1, end_ind);
                  var str_ind_3$1 = parse_spaces(match$7[0], end_ind);
                  if (Caml_string.get(str, str_ind_3$1) !== /* '>' */62) {
                    throw {
                          RE_EXN_ID: "Not_found",
                          Error: new Error()
                        };
                  }
                  var s$2 = $$String.sub(str, str_ind$2 - 2 | 0, (str_ind_3$1 - str_ind$2 | 0) + 3 | 0);
                  match$5 = [
                    str_ind_3$1 + 1 | 0,
                    {
                      TAG: /* Magic_size */1,
                      _0: s$2,
                      _1: match$7[1]
                    }
                  ];
                }
                
              }
              catch (raw_exn$1){
                var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                if (exn$1.RE_EXN_ID === "Not_found" || exn$1.RE_EXN_ID === "Failure") {
                  match$5 = undefined;
                } else {
                  throw exn$1;
                }
              }
              if (match$5 !== undefined) {
                var next_ind$1 = match$5[0];
                var fmt_rest$8 = parse_literal(next_ind$1, next_ind$1, end_ind);
                return /* Fmt_EBB */{
                        _0: {
                          TAG: /* Formatting_lit */17,
                          _0: match$5[1],
                          _1: fmt_rest$8._0
                        }
                      };
              }
              var fmt_rest$9 = parse_literal(str_ind$2, str_ind$2, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: {
                          TAG: /* Scan_indic */2,
                          _0: /* '<' */60
                        },
                        _1: fmt_rest$9._0
                      }
                    };
          case 33 :
          case 34 :
          case 35 :
          case 36 :
          case 38 :
          case 39 :
          case 40 :
          case 41 :
          case 42 :
          case 43 :
          case 45 :
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
          case 61 :
          case 62 :
              break;
          case 63 :
              var beg_ind$6 = str_ind + 1 | 0;
              var fmt_rest$10 = parse_literal(beg_ind$6, beg_ind$6, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* FFlush */2,
                        _1: fmt_rest$10._0
                      }
                    };
          case 64 :
              var beg_ind$7 = str_ind + 1 | 0;
              var fmt_rest$11 = parse_literal(beg_ind$7, beg_ind$7, end_ind);
              return /* Fmt_EBB */{
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Escaped_at */5,
                        _1: fmt_rest$11._0
                      }
                    };
          
        }
      }
      
    } else {
      var beg_ind$8 = str_ind + 1 | 0;
      var fmt_rest$12 = parse_literal(beg_ind$8, beg_ind$8, end_ind);
      return /* Fmt_EBB */{
              _0: {
                TAG: /* Formatting_lit */17,
                _0: /* Force_newline */3,
                _1: fmt_rest$12._0
              }
            };
    }
    var beg_ind$9 = str_ind + 1 | 0;
    var fmt_rest$13 = parse_literal(beg_ind$9, beg_ind$9, end_ind);
    return /* Fmt_EBB */{
            _0: {
              TAG: /* Formatting_lit */17,
              _0: {
                TAG: /* Scan_indic */2,
                _0: c
              },
              _1: fmt_rest$13._0
            }
          };
  };
  var add_literal = function (lit_start, str_ind, fmt) {
    var size = str_ind - lit_start | 0;
    if (size !== 0) {
      if (size !== 1) {
        return /* Fmt_EBB */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: $$String.sub(str, lit_start, size),
                  _1: fmt
                }
              };
      } else {
        return /* Fmt_EBB */{
                _0: {
                  TAG: /* Char_literal */12,
                  _0: Caml_string.get(str, lit_start),
                  _1: fmt
                }
              };
      }
    } else {
      return /* Fmt_EBB */{
              _0: fmt
            };
    }
  };
  var check_open_box = function (fmt) {
    if (typeof fmt === "number") {
      return ;
    }
    if (fmt.TAG !== /* String_literal */11) {
      return ;
    }
    if (typeof fmt._1 !== "number") {
      return ;
    }
    try {
      open_box_of_string(fmt._0);
      return ;
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Failure") {
        return ;
      }
      throw exn;
    }
  };
  var parse_char_set = function (str_ind, end_ind) {
    if (str_ind === end_ind) {
      invalid_format_message(end_ind, "unexpected end of format");
    }
    var char_set = Bytes.make(32, /* '\000' */0);
    var add_range = function (c, c$p) {
      for(var i = c; i <= c$p; ++i){
        add_in_char_set(char_set, Pervasives.char_of_int(i));
      }
      
    };
    var fail_single_percent = function (str_ind) {
      return Curry._2(failwith_message(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "invalid format ",
                        _1: {
                          TAG: /* Caml_string */3,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ": '",
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* '%' */37,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: "' alone is not accepted in character sets, use ",
                                _1: {
                                  TAG: /* Char_literal */12,
                                  _0: /* '%' */37,
                                  _1: {
                                    TAG: /* Char_literal */12,
                                    _0: /* '%' */37,
                                    _1: {
                                      TAG: /* String_literal */11,
                                      _0: " instead at position ",
                                      _1: {
                                        TAG: /* Int */4,
                                        _0: /* Int_d */0,
                                        _1: /* No_padding */0,
                                        _2: /* No_precision */0,
                                        _3: {
                                          TAG: /* Char_literal */12,
                                          _0: /* '.' */46,
                                          _1: /* End_of_format */0
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
                      _1: "invalid format %S: '%%' alone is not accepted in character sets, use %%%% instead at position %d."
                    }), str, str_ind);
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
        }
        add_in_char_set(char_set, /* '-' */45);
        _str_ind = str_ind + 1 | 0;
        continue ;
      };
    };
    var parse_char_set_after_char = function (_str_ind, end_ind, _c) {
      while(true) {
        var c = _c;
        var str_ind = _str_ind;
        if (str_ind === end_ind) {
          invalid_format_message(end_ind, "unexpected end of format");
        }
        var c$p = Caml_string.get(str, str_ind);
        var exit = 0;
        if (c$p >= 46) {
          if (c$p !== 64) {
            if (c$p === 93) {
              add_in_char_set(char_set, c);
              return str_ind + 1 | 0;
            }
            
          } else {
            exit = 2;
          }
        } else if (c$p !== 37) {
          if (c$p >= 45) {
            var str_ind$1 = str_ind + 1 | 0;
            if (str_ind$1 === end_ind) {
              invalid_format_message(end_ind, "unexpected end of format");
            }
            var c$p$1 = Caml_string.get(str, str_ind$1);
            if (c$p$1 !== 37) {
              if (c$p$1 !== 93) {
                add_range(c, c$p$1);
                return parse_char_set_content(str_ind$1 + 1 | 0, end_ind);
              } else {
                add_in_char_set(char_set, c);
                add_in_char_set(char_set, /* '-' */45);
                return str_ind$1 + 1 | 0;
              }
            }
            if ((str_ind$1 + 1 | 0) === end_ind) {
              invalid_format_message(end_ind, "unexpected end of format");
            }
            var c$p$2 = Caml_string.get(str, str_ind$1 + 1 | 0);
            if (c$p$2 !== 37 && c$p$2 !== 64) {
              return fail_single_percent(str_ind$1);
            }
            add_range(c, c$p$2);
            return parse_char_set_content(str_ind$1 + 2 | 0, end_ind);
          }
          
        } else {
          exit = 2;
        }
        if (exit === 2 && c === /* '%' */37) {
          add_in_char_set(char_set, c$p);
          return parse_char_set_content(str_ind + 1 | 0, end_ind);
        }
        if (c === /* '%' */37) {
          fail_single_percent(str_ind);
        }
        add_in_char_set(char_set, c);
        _c = c$p;
        _str_ind = str_ind + 1 | 0;
        continue ;
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
    var match$1 = match !== 94 ? [
        str_ind,
        false
      ] : [
        str_ind + 1 | 0,
        true
      ];
    var next_ind = parse_char_set_start(match$1[0], end_ind);
    var char_set$1 = Bytes.to_string(char_set);
    return [
            next_ind,
            match$1[1] ? rev_char_set(char_set$1) : char_set$1
          ];
  };
  var counter_of_char = function (symb) {
    if (symb >= 108) {
      if (symb < 111) {
        switch (symb) {
          case 108 :
              return /* Line_counter */0;
          case 109 :
              break;
          case 110 :
              return /* Char_counter */1;
          
        }
      }
      
    } else if (symb === 76) {
      return /* Token_counter */2;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "camlinternalFormat.ml",
            2876,
            34
          ],
          Error: new Error()
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
  return parse_literal(0, 0, str.length);
}

function format_of_string_fmtty(str, fmtty) {
  var fmt = fmt_ebb_of_string(undefined, str);
  try {
    return /* Format */{
            _0: type_format(fmt._0, fmtty),
            _1: str
          };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Type_mismatch) {
      return Curry._2(failwith_message(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "bad input: format type mismatch between ",
                        _1: {
                          TAG: /* Caml_string */3,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: " and ",
                            _1: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      },
                      _1: "bad input: format type mismatch between %S and %S"
                    }), str, string_of_fmtty(fmtty));
    }
    throw exn;
  }
}

function format_of_string_format(str, param) {
  var fmt = fmt_ebb_of_string(undefined, str);
  try {
    return /* Format */{
            _0: type_format(fmt._0, fmtty_of_fmt(param._0)),
            _1: str
          };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Type_mismatch) {
      return Curry._2(failwith_message(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "bad input: format type mismatch between ",
                        _1: {
                          TAG: /* Caml_string */3,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: " and ",
                            _1: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      },
                      _1: "bad input: format type mismatch between %S and %S"
                    }), str, param._1);
    }
    throw exn;
  }
}

export {
  is_in_char_set ,
  rev_char_set ,
  create_char_set ,
  add_in_char_set ,
  freeze_char_set ,
  param_format_of_ignored_format ,
  make_printf ,
  make_iprintf ,
  output_acc ,
  bufput_acc ,
  strput_acc ,
  type_format ,
  fmt_ebb_of_string ,
  format_of_string_fmtty ,
  format_of_string_format ,
  char_of_iconv ,
  string_of_formatting_lit ,
  string_of_formatting_gen ,
  string_of_fmtty ,
  string_of_fmt ,
  open_box_of_string ,
  symm ,
  trans ,
  recast ,
  
}
/* No side effect */
