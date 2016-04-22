// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function erase_rel(param) {
      if (typeof param === "number") {
        return /* End_of_fmtty */0;
      }
      else {
        switch (param.tag | 0) {
          case 0 : 
              return /* Char_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 0
                    };
          case 1 : 
              return /* String_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 1
                    };
          case 2 : 
              return /* Int_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 2
                    };
          case 3 : 
              return /* Int32_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 3
                    };
          case 4 : 
              return /* Nativeint_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 4
                    };
          case 5 : 
              return /* Int64_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 5
                    };
          case 6 : 
              return /* Float_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 6
                    };
          case 7 : 
              return /* Bool_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 7
                    };
          case 8 : 
              return /* Format_arg_ty */{
                      0: param[0],
                      1: erase_rel(param[1]),
                      length: 2,
                      tag: 8
                    };
          case 9 : 
              var ty1 = param[0];
              return /* Format_subst_ty */{
                      0: ty1,
                      1: ty1,
                      2: erase_rel(param[2]),
                      length: 3,
                      tag: 9
                    };
          case 10 : 
              return /* Alpha_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 10
                    };
          case 11 : 
              return /* Theta_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 11
                    };
          case 12 : 
              return /* Any_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 12
                    };
          case 13 : 
              return /* Reader_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 13
                    };
          case 14 : 
              return /* Ignored_reader_ty */{
                      0: erase_rel(param[0]),
                      length: 1,
                      tag: 14
                    };
          
        }
      }
    }
    
    function concat_fmtty(fmtty1, fmtty2) {
      if (typeof fmtty1 === "number") {
        return fmtty2;
      }
      else {
        switch (fmtty1.tag | 0) {
          case 0 : 
              return /* Char_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 0
                    };
          case 1 : 
              return /* String_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 1
                    };
          case 2 : 
              return /* Int_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 2
                    };
          case 3 : 
              return /* Int32_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 3
                    };
          case 4 : 
              return /* Nativeint_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 4
                    };
          case 5 : 
              return /* Int64_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 5
                    };
          case 6 : 
              return /* Float_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 6
                    };
          case 7 : 
              return /* Bool_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 7
                    };
          case 8 : 
              return /* Format_arg_ty */{
                      0: fmtty1[0],
                      1: concat_fmtty(fmtty1[1], fmtty2),
                      length: 2,
                      tag: 8
                    };
          case 9 : 
              return /* Format_subst_ty */{
                      0: fmtty1[0],
                      1: fmtty1[1],
                      2: concat_fmtty(fmtty1[2], fmtty2),
                      length: 3,
                      tag: 9
                    };
          case 10 : 
              return /* Alpha_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 10
                    };
          case 11 : 
              return /* Theta_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 11
                    };
          case 12 : 
              return /* Any_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 12
                    };
          case 13 : 
              return /* Reader_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 13
                    };
          case 14 : 
              return /* Ignored_reader_ty */{
                      0: concat_fmtty(fmtty1[0], fmtty2),
                      length: 1,
                      tag: 14
                    };
          
        }
      }
    }
    
    function concat_fmt(fmt1, fmt2) {
      if (typeof fmt1 === "number") {
        return fmt2;
      }
      else {
        switch (fmt1.tag | 0) {
          case 0 : 
              return /* Char */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 0
                    };
          case 1 : 
              return /* Caml_char */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 1
                    };
          case 2 : 
              return /* String */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 2
                    };
          case 3 : 
              return /* Caml_string */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 3
                    };
          case 4 : 
              return /* Int */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: fmt1[2],
                      3: concat_fmt(fmt1[3], fmt2),
                      length: 4,
                      tag: 4
                    };
          case 5 : 
              return /* Int32 */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: fmt1[2],
                      3: concat_fmt(fmt1[3], fmt2),
                      length: 4,
                      tag: 5
                    };
          case 6 : 
              return /* Nativeint */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: fmt1[2],
                      3: concat_fmt(fmt1[3], fmt2),
                      length: 4,
                      tag: 6
                    };
          case 7 : 
              return /* Int64 */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: fmt1[2],
                      3: concat_fmt(fmt1[3], fmt2),
                      length: 4,
                      tag: 7
                    };
          case 8 : 
              return /* Float */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: fmt1[2],
                      3: concat_fmt(fmt1[3], fmt2),
                      length: 4,
                      tag: 8
                    };
          case 9 : 
              return /* Bool */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 9
                    };
          case 10 : 
              return /* Flush */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 10
                    };
          case 11 : 
              return /* String_literal */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 11
                    };
          case 12 : 
              return /* Char_literal */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 12
                    };
          case 13 : 
              return /* Format_arg */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: concat_fmt(fmt1[2], fmt2),
                      length: 3,
                      tag: 13
                    };
          case 14 : 
              return /* Format_subst */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: concat_fmt(fmt1[2], fmt2),
                      length: 3,
                      tag: 14
                    };
          case 15 : 
              return /* Alpha */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 15
                    };
          case 16 : 
              return /* Theta */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 16
                    };
          case 17 : 
              return /* Formatting_lit */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 17
                    };
          case 18 : 
              return /* Formatting_gen */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 18
                    };
          case 19 : 
              return /* Reader */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 19
                    };
          case 20 : 
              return /* Scan_char_set */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: concat_fmt(fmt1[2], fmt2),
                      length: 3,
                      tag: 20
                    };
          case 21 : 
              return /* Scan_get_counter */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 21
                    };
          case 22 : 
              return /* Scan_next_char */{
                      0: concat_fmt(fmt1[0], fmt2),
                      length: 1,
                      tag: 22
                    };
          case 23 : 
              return /* Ignored_param */{
                      0: fmt1[0],
                      1: concat_fmt(fmt1[1], fmt2),
                      length: 2,
                      tag: 23
                    };
          case 24 : 
              return /* Custom */{
                      0: fmt1[0],
                      1: fmt1[1],
                      2: concat_fmt(fmt1[2], fmt2),
                      length: 3,
                      tag: 24
                    };
          
        }
      }
    }
    
    exports.concat_fmtty = concat_fmtty;
    exports.erase_rel    = erase_rel;
    exports.concat_fmt   = concat_fmt;
    
  })
/* No side effect */
