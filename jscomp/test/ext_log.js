// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives       = require("../stdlib/pervasives");
var Curry            = require("../runtime/curry");
var Lam_current_unit = require("./lam_current_unit");
var Format           = require("../stdlib/format");

function err(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String */{
                        0: /* No_padding */0,
                        1: /* Char_literal */{
                          0: /* " " */32,
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 12
                        },
                        length: 2,
                        tag: 2
                      },
                      "%s "
                    ], Pervasives.$caret$caret(f, /* Format */[
                          /* Formatting_lit */{
                            0: /* Flush_newline */4,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 17
                          },
                          "@."
                        ]))), str);
}

function ierr(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        "%s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        "%s "
                      ], f)), str);
  }
}

function warn(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String_literal */{
                        0: "WARN: ",
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 11
                      },
                      "WARN: %s "
                    ], Pervasives.$caret$caret(f, /* Format */[
                          /* Formatting_lit */{
                            0: /* Flush_newline */4,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 17
                          },
                          "@."
                        ]))), str);
}

function iwarn(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "WARN: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        "WARN: %s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "WARN: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        "WARN: %s "
                      ], f)), str);
  }
}

function dwarn(str, f) {
  if (Lam_current_unit.is_same_file(/* () */0)) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "WARN: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        "WARN: %s "
                      ], Pervasives.$caret$caret(f, /* Format */[
                            /* Formatting_lit */{
                              0: /* Flush_newline */4,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 17
                            },
                            "@."
                          ]))), str);
  }
  else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "WARN: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        "WARN: %s "
                      ], Pervasives.$caret$caret(f, /* Format */[
                            /* Formatting_lit */{
                              0: /* Flush_newline */4,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 17
                            },
                            "@."
                          ]))), str);
  }
}

function info(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                      /* String_literal */{
                        0: "INFO: ",
                        1: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        length: 2,
                        tag: 11
                      },
                      "INFO: %s "
                    ], f)), str);
}

function iinfo(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "INFO: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
                        "INFO: %s "
                      ], f)), str);
  }
  else {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */[
                        /* String_literal */{
                          0: "INFO: ",
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 12
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 11
                        },
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
