'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Pervasives = require("../../lib/js/pervasives.js");

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
  } else {
    return Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* " " */32,
                              /* End_of_format */0
                            ])
                        ]),
                      "%s "
                    ], f))(str);
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
  } else {
    return Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
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
                    ], f))(str);
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

exports.err = err;
exports.ierr = ierr;
exports.warn = warn;
exports.iwarn = iwarn;
exports.info = info;
exports.iinfo = iinfo;
/* Format Not a pure module */
