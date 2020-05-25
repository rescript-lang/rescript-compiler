'use strict';

var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Pervasives = require("../../lib/js/pervasives.js");

function err(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: /* End_of_format */0
                        }
                      },
                      _1: "%s "
                    }, Pervasives.$caret$caret(f, /* Format */{
                          _0: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Flush_newline */4,
                            _1: /* End_of_format */0
                          },
                          _1: "@."
                        }))), str);
}

function ierr(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "%s "
                      }, f)), str);
  } else {
    return Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: /* End_of_format */0
                        }
                      },
                      _1: "%s "
                    }, f))(str);
  }
}

function warn(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "WARN: ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "WARN: %s "
                    }, Pervasives.$caret$caret(f, /* Format */{
                          _0: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Flush_newline */4,
                            _1: /* End_of_format */0
                          },
                          _1: "@."
                        }))), str);
}

function iwarn(b, str, f) {
  if (b) {
    return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "WARN: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* " " */32,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "WARN: %s "
                      }, f)), str);
  } else {
    return Format.ifprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "WARN: ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "WARN: %s "
                    }, f))(str);
  }
}

function info(str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "INFO: ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "INFO: %s "
                    }, f)), str);
}

function iinfo(b, str, f) {
  return Curry._1(Format.fprintf(Format.err_formatter, Pervasives.$caret$caret(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "INFO: ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "INFO: %s "
                    }, f)), str);
}

exports.err = err;
exports.ierr = ierr;
exports.warn = warn;
exports.iwarn = iwarn;
exports.info = info;
exports.iinfo = iinfo;
/* Format Not a pure module */
