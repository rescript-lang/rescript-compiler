'use strict';


function erase_rel(param) {
  if (typeof param === "string") {
    return "End_of_fmtty";
  } else {
    switch (/* XXX */param.tag) {
      case "Char_ty" :
          return /* constructor */{
                  tag: "Char_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "String_ty" :
          return /* constructor */{
                  tag: "String_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Int_ty" :
          return /* constructor */{
                  tag: "Int_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Int32_ty" :
          return /* constructor */{
                  tag: "Int32_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Nativeint_ty" :
          return /* constructor */{
                  tag: "Nativeint_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Int64_ty" :
          return /* constructor */{
                  tag: "Int64_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Float_ty" :
          return /* constructor */{
                  tag: "Float_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Bool_ty" :
          return /* constructor */{
                  tag: "Bool_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Format_arg_ty" :
          return /* constructor */{
                  tag: "Format_arg_ty",
                  Arg0: param.Arg0,
                  Arg1: erase_rel(param.Arg1)
                };
      case "Format_subst_ty" :
          var ty1 = param.Arg0;
          return /* constructor */{
                  tag: "Format_subst_ty",
                  Arg0: ty1,
                  Arg1: ty1,
                  Arg2: erase_rel(param.Arg2)
                };
      case "Alpha_ty" :
          return /* constructor */{
                  tag: "Alpha_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Theta_ty" :
          return /* constructor */{
                  tag: "Theta_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Any_ty" :
          return /* constructor */{
                  tag: "Any_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Reader_ty" :
          return /* constructor */{
                  tag: "Reader_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      case "Ignored_reader_ty" :
          return /* constructor */{
                  tag: "Ignored_reader_ty",
                  Arg0: erase_rel(param.Arg0)
                };
      
    }
  }
}

function concat_fmtty(fmtty1, fmtty2) {
  if (typeof fmtty1 === "string") {
    return fmtty2;
  } else {
    switch (/* XXX */fmtty1.tag) {
      case "Char_ty" :
          return /* constructor */{
                  tag: "Char_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "String_ty" :
          return /* constructor */{
                  tag: "String_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Int_ty" :
          return /* constructor */{
                  tag: "Int_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Int32_ty" :
          return /* constructor */{
                  tag: "Int32_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Nativeint_ty" :
          return /* constructor */{
                  tag: "Nativeint_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Int64_ty" :
          return /* constructor */{
                  tag: "Int64_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Float_ty" :
          return /* constructor */{
                  tag: "Float_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Bool_ty" :
          return /* constructor */{
                  tag: "Bool_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Format_arg_ty" :
          return /* constructor */{
                  tag: "Format_arg_ty",
                  Arg0: fmtty1.Arg0,
                  Arg1: concat_fmtty(fmtty1.Arg1, fmtty2)
                };
      case "Format_subst_ty" :
          return /* constructor */{
                  tag: "Format_subst_ty",
                  Arg0: fmtty1.Arg0,
                  Arg1: fmtty1.Arg1,
                  Arg2: concat_fmtty(fmtty1.Arg2, fmtty2)
                };
      case "Alpha_ty" :
          return /* constructor */{
                  tag: "Alpha_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Theta_ty" :
          return /* constructor */{
                  tag: "Theta_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Any_ty" :
          return /* constructor */{
                  tag: "Any_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Reader_ty" :
          return /* constructor */{
                  tag: "Reader_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      case "Ignored_reader_ty" :
          return /* constructor */{
                  tag: "Ignored_reader_ty",
                  Arg0: concat_fmtty(fmtty1.Arg0, fmtty2)
                };
      
    }
  }
}

function concat_fmt(fmt1, fmt2) {
  if (typeof fmt1 === "string") {
    return fmt2;
  } else {
    switch (/* XXX */fmt1.tag) {
      case "Char" :
          return /* constructor */{
                  tag: "Char",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Caml_char" :
          return /* constructor */{
                  tag: "Caml_char",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "String" :
          return /* constructor */{
                  tag: "String",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Caml_string" :
          return /* constructor */{
                  tag: "Caml_string",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Int" :
          return /* constructor */{
                  tag: "Int",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: fmt1.Arg2,
                  Arg3: concat_fmt(fmt1.Arg3, fmt2)
                };
      case "Int32" :
          return /* constructor */{
                  tag: "Int32",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: fmt1.Arg2,
                  Arg3: concat_fmt(fmt1.Arg3, fmt2)
                };
      case "Nativeint" :
          return /* constructor */{
                  tag: "Nativeint",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: fmt1.Arg2,
                  Arg3: concat_fmt(fmt1.Arg3, fmt2)
                };
      case "Int64" :
          return /* constructor */{
                  tag: "Int64",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: fmt1.Arg2,
                  Arg3: concat_fmt(fmt1.Arg3, fmt2)
                };
      case "Float" :
          return /* constructor */{
                  tag: "Float",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: fmt1.Arg2,
                  Arg3: concat_fmt(fmt1.Arg3, fmt2)
                };
      case "Bool" :
          return /* constructor */{
                  tag: "Bool",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Flush" :
          return /* constructor */{
                  tag: "Flush",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "String_literal" :
          return /* constructor */{
                  tag: "String_literal",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Char_literal" :
          return /* constructor */{
                  tag: "Char_literal",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Format_arg" :
          return /* constructor */{
                  tag: "Format_arg",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: concat_fmt(fmt1.Arg2, fmt2)
                };
      case "Format_subst" :
          return /* constructor */{
                  tag: "Format_subst",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: concat_fmt(fmt1.Arg2, fmt2)
                };
      case "Alpha" :
          return /* constructor */{
                  tag: "Alpha",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Theta" :
          return /* constructor */{
                  tag: "Theta",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Formatting_lit" :
          return /* constructor */{
                  tag: "Formatting_lit",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Formatting_gen" :
          return /* constructor */{
                  tag: "Formatting_gen",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Reader" :
          return /* constructor */{
                  tag: "Reader",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Scan_char_set" :
          return /* constructor */{
                  tag: "Scan_char_set",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: concat_fmt(fmt1.Arg2, fmt2)
                };
      case "Scan_get_counter" :
          return /* constructor */{
                  tag: "Scan_get_counter",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Scan_next_char" :
          return /* constructor */{
                  tag: "Scan_next_char",
                  Arg0: concat_fmt(fmt1.Arg0, fmt2)
                };
      case "Ignored_param" :
          return /* constructor */{
                  tag: "Ignored_param",
                  Arg0: fmt1.Arg0,
                  Arg1: concat_fmt(fmt1.Arg1, fmt2)
                };
      case "Custom" :
          return /* constructor */{
                  tag: "Custom",
                  Arg0: fmt1.Arg0,
                  Arg1: fmt1.Arg1,
                  Arg2: concat_fmt(fmt1.Arg2, fmt2)
                };
      
    }
  }
}

exports.concat_fmtty = concat_fmtty;
exports.erase_rel = erase_rel;
exports.concat_fmt = concat_fmt;
/* No side effect */
