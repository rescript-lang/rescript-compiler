


function erase_rel(rest) {
  if (typeof rest === "number") {
    return /* End_of_fmtty */0;
  }
  switch (rest.TAG | 0) {
    case /* Char_ty */0 :
        return {
                TAG: /* Char_ty */0,
                _0: erase_rel(rest._0)
              };
    case /* String_ty */1 :
        return {
                TAG: /* String_ty */1,
                _0: erase_rel(rest._0)
              };
    case /* Int_ty */2 :
        return {
                TAG: /* Int_ty */2,
                _0: erase_rel(rest._0)
              };
    case /* Int32_ty */3 :
        return {
                TAG: /* Int32_ty */3,
                _0: erase_rel(rest._0)
              };
    case /* Nativeint_ty */4 :
        return {
                TAG: /* Nativeint_ty */4,
                _0: erase_rel(rest._0)
              };
    case /* Int64_ty */5 :
        return {
                TAG: /* Int64_ty */5,
                _0: erase_rel(rest._0)
              };
    case /* Float_ty */6 :
        return {
                TAG: /* Float_ty */6,
                _0: erase_rel(rest._0)
              };
    case /* Bool_ty */7 :
        return {
                TAG: /* Bool_ty */7,
                _0: erase_rel(rest._0)
              };
    case /* Format_arg_ty */8 :
        return {
                TAG: /* Format_arg_ty */8,
                _0: rest._0,
                _1: erase_rel(rest._1)
              };
    case /* Format_subst_ty */9 :
        var ty1 = rest._0;
        return {
                TAG: /* Format_subst_ty */9,
                _0: ty1,
                _1: ty1,
                _2: erase_rel(rest._2)
              };
    case /* Alpha_ty */10 :
        return {
                TAG: /* Alpha_ty */10,
                _0: erase_rel(rest._0)
              };
    case /* Theta_ty */11 :
        return {
                TAG: /* Theta_ty */11,
                _0: erase_rel(rest._0)
              };
    case /* Any_ty */12 :
        return {
                TAG: /* Any_ty */12,
                _0: erase_rel(rest._0)
              };
    case /* Reader_ty */13 :
        return {
                TAG: /* Reader_ty */13,
                _0: erase_rel(rest._0)
              };
    case /* Ignored_reader_ty */14 :
        return {
                TAG: /* Ignored_reader_ty */14,
                _0: erase_rel(rest._0)
              };
    
  }
}

function concat_fmtty(fmtty1, fmtty2) {
  if (typeof fmtty1 === "number") {
    return fmtty2;
  }
  switch (fmtty1.TAG | 0) {
    case /* Char_ty */0 :
        return {
                TAG: /* Char_ty */0,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* String_ty */1 :
        return {
                TAG: /* String_ty */1,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int_ty */2 :
        return {
                TAG: /* Int_ty */2,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int32_ty */3 :
        return {
                TAG: /* Int32_ty */3,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Nativeint_ty */4 :
        return {
                TAG: /* Nativeint_ty */4,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Int64_ty */5 :
        return {
                TAG: /* Int64_ty */5,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Float_ty */6 :
        return {
                TAG: /* Float_ty */6,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Bool_ty */7 :
        return {
                TAG: /* Bool_ty */7,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Format_arg_ty */8 :
        return {
                TAG: /* Format_arg_ty */8,
                _0: fmtty1._0,
                _1: concat_fmtty(fmtty1._1, fmtty2)
              };
    case /* Format_subst_ty */9 :
        return {
                TAG: /* Format_subst_ty */9,
                _0: fmtty1._0,
                _1: fmtty1._1,
                _2: concat_fmtty(fmtty1._2, fmtty2)
              };
    case /* Alpha_ty */10 :
        return {
                TAG: /* Alpha_ty */10,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Theta_ty */11 :
        return {
                TAG: /* Theta_ty */11,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Any_ty */12 :
        return {
                TAG: /* Any_ty */12,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Reader_ty */13 :
        return {
                TAG: /* Reader_ty */13,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    case /* Ignored_reader_ty */14 :
        return {
                TAG: /* Ignored_reader_ty */14,
                _0: concat_fmtty(fmtty1._0, fmtty2)
              };
    
  }
}

function concat_fmt(fmt1, fmt2) {
  if (typeof fmt1 === "number") {
    return fmt2;
  }
  switch (fmt1.TAG | 0) {
    case /* Char */0 :
        return {
                TAG: /* Char */0,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Caml_char */1 :
        return {
                TAG: /* Caml_char */1,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* String */2 :
        return {
                TAG: /* String */2,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Caml_string */3 :
        return {
                TAG: /* Caml_string */3,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Int */4 :
        return {
                TAG: /* Int */4,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Int32 */5 :
        return {
                TAG: /* Int32 */5,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Nativeint */6 :
        return {
                TAG: /* Nativeint */6,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Int64 */7 :
        return {
                TAG: /* Int64 */7,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Float */8 :
        return {
                TAG: /* Float */8,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: fmt1._2,
                _3: concat_fmt(fmt1._3, fmt2)
              };
    case /* Bool */9 :
        return {
                TAG: /* Bool */9,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Flush */10 :
        return {
                TAG: /* Flush */10,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* String_literal */11 :
        return {
                TAG: /* String_literal */11,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Char_literal */12 :
        return {
                TAG: /* Char_literal */12,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Format_arg */13 :
        return {
                TAG: /* Format_arg */13,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Format_subst */14 :
        return {
                TAG: /* Format_subst */14,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Alpha */15 :
        return {
                TAG: /* Alpha */15,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Theta */16 :
        return {
                TAG: /* Theta */16,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Formatting_lit */17 :
        return {
                TAG: /* Formatting_lit */17,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Formatting_gen */18 :
        return {
                TAG: /* Formatting_gen */18,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Reader */19 :
        return {
                TAG: /* Reader */19,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Scan_char_set */20 :
        return {
                TAG: /* Scan_char_set */20,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    case /* Scan_get_counter */21 :
        return {
                TAG: /* Scan_get_counter */21,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Scan_next_char */22 :
        return {
                TAG: /* Scan_next_char */22,
                _0: concat_fmt(fmt1._0, fmt2)
              };
    case /* Ignored_param */23 :
        return {
                TAG: /* Ignored_param */23,
                _0: fmt1._0,
                _1: concat_fmt(fmt1._1, fmt2)
              };
    case /* Custom */24 :
        return {
                TAG: /* Custom */24,
                _0: fmt1._0,
                _1: fmt1._1,
                _2: concat_fmt(fmt1._2, fmt2)
              };
    
  }
}

export {
  concat_fmtty ,
  erase_rel ,
  concat_fmt ,
  
}
/* No side effect */
