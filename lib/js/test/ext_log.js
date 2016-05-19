// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.2 , PLEASE EDIT WITH CARE
'use strict';

var Pervasives       = require("../pervasives");
var Block            = require("../block");
var Curry            = require("../curry");
var Lam_current_unit = require("./lam_current_unit");
var Format           = require("../format");

function err(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* " " */32,
                              /* End_of_format */0
                            ])
                        ]),
                      "%s "
                    ], Pervasives.$caret$caret(f, /* Format */[
                          /* Formatting_lit */Block.__(17, [
                              /* Flush_newline */4,
                              /* End_of_format */0
                            ]),
                          "@."
                        ]))), str);
}

function ierr(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* Char_literal */Block.__(12, [
                                /* " " */32,
                                /* End_of_format */0
                              ])
                          ]),
                        "%s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* Char_literal */Block.__(12, [
                                /* " " */32,
                                /* End_of_format */0
                              ])
                          ]),
                        "%s "
                      ], f)), str);
  }
}

function warn(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String_literal */Block.__(11, [
                          "WARN: ",
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* " " */32,
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "WARN: %s "
                    ], Pervasives.$caret$caret(f, /* Format */[
                          /* Formatting_lit */Block.__(17, [
                              /* Flush_newline */4,
                              /* End_of_format */0
                            ]),
                          "@."
                        ]))), str);
}

function iwarn(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "WARN: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "WARN: %s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "WARN: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "WARN: %s "
                      ], f)), str);
  }
}

function dwarn(str, f) {
  if (Lam_current_unit.is_same_file(/* () */0)) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "WARN: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "WARN: %s "
                      ], Pervasives.$caret$caret(f, /* Format */[
                            /* Formatting_lit */Block.__(17, [
                                /* Flush_newline */4,
                                /* End_of_format */0
                              ]),
                            "@."
                          ]))), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "WARN: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "WARN: %s "
                      ], Pervasives.$caret$caret(f, /* Format */[
                            /* Formatting_lit */Block.__(17, [
                                /* Flush_newline */4,
                                /* End_of_format */0
                              ]),
                            "@."
                          ]))), str);
  }
}

function info(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String_literal */Block.__(11, [
                          "INFO: ",
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* " " */32,
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "INFO: %s "
                    ], f)), str);
}

function iinfo(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "INFO: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "INFO: %s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */Block.__(11, [
                            "INFO: ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "INFO: %s "
                      ], f)), str);
  }
}

exports.err   = err;
exports.ierr  = ierr;
exports.warn  = warn;
exports.iwarn = iwarn;
exports.dwarn = dwarn;
exports.info  = info;
exports.iinfo = iinfo;
/* Lam_current_unit Not a pure module */
