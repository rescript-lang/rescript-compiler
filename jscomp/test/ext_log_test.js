'use strict';

var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Pervasives = require("../../lib/js/pervasives.js");

function err(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* " " */32,
                          Arg1: "End_of_format"
                        }
                      },
                      Arg1: "%s "
                    }, Pervasives.$caret$caret(f, /* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: "Flush_newline",
                            Arg1: "End_of_format"
                          },
                          Arg1: "@."
                        }))), str);
}

function ierr(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* " " */32,
                            Arg1: "End_of_format"
                          }
                        },
                        Arg1: "%s "
                      }, f)), str);
  } else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* " " */32,
                            Arg1: "End_of_format"
                          }
                        },
                        Arg1: "%s "
                      }, f)), str);
  }
}

function warn(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "WARN: ",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* " " */32,
                            Arg1: "End_of_format"
                          }
                        }
                      },
                      Arg1: "WARN: %s "
                    }, Pervasives.$caret$caret(f, /* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: "Flush_newline",
                            Arg1: "End_of_format"
                          },
                          Arg1: "@."
                        }))), str);
}

function iwarn(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "WARN: ",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* " " */32,
                              Arg1: "End_of_format"
                            }
                          }
                        },
                        Arg1: "WARN: %s "
                      }, f)), str);
  } else {
    return Curry._1(Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "WARN: ",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* " " */32,
                              Arg1: "End_of_format"
                            }
                          }
                        },
                        Arg1: "WARN: %s "
                      }, f)), str);
  }
}

function info(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: "INFO: ",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "Char_literal",
                            Arg0: /* " " */32,
                            Arg1: "End_of_format"
                          }
                        }
                      },
                      Arg1: "INFO: %s "
                    }, f)), str);
}

function iinfo(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "INFO: ",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* " " */32,
                              Arg1: "End_of_format"
                            }
                          }
                        },
                        Arg1: "INFO: %s "
                      }, f)), str);
  } else {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "INFO: ",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* " " */32,
                              Arg1: "End_of_format"
                            }
                          }
                        },
                        Arg1: "INFO: %s "
                      }, f)), str);
  }
}

exports.err = err;
exports.ierr = ierr;
exports.warn = warn;
exports.iwarn = iwarn;
exports.info = info;
exports.iinfo = iinfo;
/* Format Not a pure module */
