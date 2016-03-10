// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives       = require("../stdlib/pervasives");
var Lam_current_unit = require("./lam_current_unit");
var Caml_curry       = require("../runtime/caml_curry");
var Format           = require("../stdlib/format");

function err(str, f) {
  return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      0: /* String */{
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
                      1: "%s ",
                      length: 2,
                      tag: 0
                    }, Pervasives.$caret$caret(f, /* Format */{
                          0: /* Formatting_lit */{
                            0: /* Flush_newline */4,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 17
                          },
                          1: "@.",
                          length: 2,
                          tag: 0
                        }))), str);
}

function ierr(b, str, f) {
  if (b) {
    return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String */{
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
                        1: "%s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
  }
  else {
    return Caml_curry.app1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String */{
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
                        1: "%s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
  }
}

function warn(str, f) {
  return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      0: /* String_literal */{
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
                      1: "WARN: %s ",
                      length: 2,
                      tag: 0
                    }, Pervasives.$caret$caret(f, /* Format */{
                          0: /* Formatting_lit */{
                            0: /* Flush_newline */4,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 17
                          },
                          1: "@.",
                          length: 2,
                          tag: 0
                        }))), str);
}

function iwarn(b, str, f) {
  if (b) {
    return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "WARN: %s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
  }
  else {
    return Caml_curry.app1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "WARN: %s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
  }
}

function dwarn(str, f) {
  if (Lam_current_unit.is_same_file(/* () */0)) {
    return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "WARN: %s ",
                        length: 2,
                        tag: 0
                      }, Pervasives.$caret$caret(f, /* Format */{
                            0: /* Formatting_lit */{
                              0: /* Flush_newline */4,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 17
                            },
                            1: "@.",
                            length: 2,
                            tag: 0
                          }))), str);
  }
  else {
    return Caml_curry.app1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "WARN: %s ",
                        length: 2,
                        tag: 0
                      }, Pervasives.$caret$caret(f, /* Format */{
                            0: /* Formatting_lit */{
                              0: /* Flush_newline */4,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 17
                            },
                            1: "@.",
                            length: 2,
                            tag: 0
                          }))), str);
  }
}

function info(str, f) {
  return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      0: /* String_literal */{
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
                      1: "INFO: %s ",
                      length: 2,
                      tag: 0
                    }, f)), str);
}

function iinfo(b, str, f) {
  if (b) {
    return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "INFO: %s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
  }
  else {
    return Caml_curry.app1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        0: /* String_literal */{
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
                        1: "INFO: %s ",
                        length: 2,
                        tag: 0
                      }, f)), str);
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
