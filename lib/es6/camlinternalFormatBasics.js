

import * as Block from "./block.js";

function erase_rel(rest) {
  if (typeof rest === "number") {
    return /* End_of_fmtty */0;
  }
  switch (rest.tag | 0) {
    case /* Char_ty */0 :
        return /* Char_ty */Block.__(0, [erase_rel(rest[0])]);
    case /* String_ty */1 :
        return /* String_ty */Block.__(1, [erase_rel(rest[0])]);
    case /* Int_ty */2 :
        return /* Int_ty */Block.__(2, [erase_rel(rest[0])]);
    case /* Int32_ty */3 :
        return /* Int32_ty */Block.__(3, [erase_rel(rest[0])]);
    case /* Nativeint_ty */4 :
        return /* Nativeint_ty */Block.__(4, [erase_rel(rest[0])]);
    case /* Int64_ty */5 :
        return /* Int64_ty */Block.__(5, [erase_rel(rest[0])]);
    case /* Float_ty */6 :
        return /* Float_ty */Block.__(6, [erase_rel(rest[0])]);
    case /* Bool_ty */7 :
        return /* Bool_ty */Block.__(7, [erase_rel(rest[0])]);
    case /* Format_arg_ty */8 :
        return /* Format_arg_ty */Block.__(8, [
                  rest[0],
                  erase_rel(rest[1])
                ]);
    case /* Format_subst_ty */9 :
        var ty1 = rest[0];
        return /* Format_subst_ty */Block.__(9, [
                  ty1,
                  ty1,
                  erase_rel(rest[2])
                ]);
    case /* Alpha_ty */10 :
        return /* Alpha_ty */Block.__(10, [erase_rel(rest[0])]);
    case /* Theta_ty */11 :
        return /* Theta_ty */Block.__(11, [erase_rel(rest[0])]);
    case /* Any_ty */12 :
        return /* Any_ty */Block.__(12, [erase_rel(rest[0])]);
    case /* Reader_ty */13 :
        return /* Reader_ty */Block.__(13, [erase_rel(rest[0])]);
    case /* Ignored_reader_ty */14 :
        return /* Ignored_reader_ty */Block.__(14, [erase_rel(rest[0])]);
    
  }
}

function concat_fmtty(fmtty1, fmtty2) {
  if (typeof fmtty1 === "number") {
    return fmtty2;
  }
  switch (fmtty1.tag | 0) {
    case /* Char_ty */0 :
        return /* Char_ty */Block.__(0, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* String_ty */1 :
        return /* String_ty */Block.__(1, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Int_ty */2 :
        return /* Int_ty */Block.__(2, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Int32_ty */3 :
        return /* Int32_ty */Block.__(3, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Nativeint_ty */4 :
        return /* Nativeint_ty */Block.__(4, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Int64_ty */5 :
        return /* Int64_ty */Block.__(5, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Float_ty */6 :
        return /* Float_ty */Block.__(6, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Bool_ty */7 :
        return /* Bool_ty */Block.__(7, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Format_arg_ty */8 :
        return /* Format_arg_ty */Block.__(8, [
                  fmtty1[0],
                  concat_fmtty(fmtty1[1], fmtty2)
                ]);
    case /* Format_subst_ty */9 :
        return /* Format_subst_ty */Block.__(9, [
                  fmtty1[0],
                  fmtty1[1],
                  concat_fmtty(fmtty1[2], fmtty2)
                ]);
    case /* Alpha_ty */10 :
        return /* Alpha_ty */Block.__(10, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Theta_ty */11 :
        return /* Theta_ty */Block.__(11, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Any_ty */12 :
        return /* Any_ty */Block.__(12, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Reader_ty */13 :
        return /* Reader_ty */Block.__(13, [concat_fmtty(fmtty1[0], fmtty2)]);
    case /* Ignored_reader_ty */14 :
        return /* Ignored_reader_ty */Block.__(14, [concat_fmtty(fmtty1[0], fmtty2)]);
    
  }
}

function concat_fmt(fmt1, fmt2) {
  if (typeof fmt1 === "number") {
    return fmt2;
  }
  switch (fmt1.tag | 0) {
    case /* Char */0 :
        return /* Char */Block.__(0, [concat_fmt(fmt1[0], fmt2)]);
    case /* Caml_char */1 :
        return /* Caml_char */Block.__(1, [concat_fmt(fmt1[0], fmt2)]);
    case /* String */2 :
        return /* String */Block.__(2, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Caml_string */3 :
        return /* Caml_string */Block.__(3, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Int */4 :
        return /* Int */Block.__(4, [
                  fmt1[0],
                  fmt1[1],
                  fmt1[2],
                  concat_fmt(fmt1[3], fmt2)
                ]);
    case /* Int32 */5 :
        return /* Int32 */Block.__(5, [
                  fmt1[0],
                  fmt1[1],
                  fmt1[2],
                  concat_fmt(fmt1[3], fmt2)
                ]);
    case /* Nativeint */6 :
        return /* Nativeint */Block.__(6, [
                  fmt1[0],
                  fmt1[1],
                  fmt1[2],
                  concat_fmt(fmt1[3], fmt2)
                ]);
    case /* Int64 */7 :
        return /* Int64 */Block.__(7, [
                  fmt1[0],
                  fmt1[1],
                  fmt1[2],
                  concat_fmt(fmt1[3], fmt2)
                ]);
    case /* Float */8 :
        return /* Float */Block.__(8, [
                  fmt1[0],
                  fmt1[1],
                  fmt1[2],
                  concat_fmt(fmt1[3], fmt2)
                ]);
    case /* Bool */9 :
        return /* Bool */Block.__(9, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Flush */10 :
        return /* Flush */Block.__(10, [concat_fmt(fmt1[0], fmt2)]);
    case /* String_literal */11 :
        return /* String_literal */Block.__(11, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Char_literal */12 :
        return /* Char_literal */Block.__(12, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Format_arg */13 :
        return /* Format_arg */Block.__(13, [
                  fmt1[0],
                  fmt1[1],
                  concat_fmt(fmt1[2], fmt2)
                ]);
    case /* Format_subst */14 :
        return /* Format_subst */Block.__(14, [
                  fmt1[0],
                  fmt1[1],
                  concat_fmt(fmt1[2], fmt2)
                ]);
    case /* Alpha */15 :
        return /* Alpha */Block.__(15, [concat_fmt(fmt1[0], fmt2)]);
    case /* Theta */16 :
        return /* Theta */Block.__(16, [concat_fmt(fmt1[0], fmt2)]);
    case /* Formatting_lit */17 :
        return /* Formatting_lit */Block.__(17, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Formatting_gen */18 :
        return /* Formatting_gen */Block.__(18, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Reader */19 :
        return /* Reader */Block.__(19, [concat_fmt(fmt1[0], fmt2)]);
    case /* Scan_char_set */20 :
        return /* Scan_char_set */Block.__(20, [
                  fmt1[0],
                  fmt1[1],
                  concat_fmt(fmt1[2], fmt2)
                ]);
    case /* Scan_get_counter */21 :
        return /* Scan_get_counter */Block.__(21, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Scan_next_char */22 :
        return /* Scan_next_char */Block.__(22, [concat_fmt(fmt1[0], fmt2)]);
    case /* Ignored_param */23 :
        return /* Ignored_param */Block.__(23, [
                  fmt1[0],
                  concat_fmt(fmt1[1], fmt2)
                ]);
    case /* Custom */24 :
        return /* Custom */Block.__(24, [
                  fmt1[0],
                  fmt1[1],
                  concat_fmt(fmt1[2], fmt2)
                ]);
    
  }
}

export {
  concat_fmtty ,
  erase_rel ,
  concat_fmt ,
  
}
/* No side effect */
