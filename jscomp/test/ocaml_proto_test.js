'use strict';

var Mt = require("./mt.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Lexing = require("../../lib/js/lexing.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Parsing = require("../../lib/js/parsing.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Filename = require("../../lib/js/filename.js");
var Printexc = require("../../lib/js/printexc.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function field(optionsOpt, label, number, type_, name) {
  var options = optionsOpt !== undefined ? optionsOpt : /* [] */0;
  return {
          field_name: name,
          field_number: number,
          field_label: label,
          field_type: type_,
          field_options: options
        };
}

function map(map_optionsOpt, number, key_type, value_type, name) {
  var map_options = map_optionsOpt !== undefined ? map_optionsOpt : /* [] */0;
  return {
          map_name: name,
          map_number: number,
          map_key_type: key_type,
          map_value_type: value_type,
          map_options: map_options
        };
}

function oneof_field(optionsOpt, number, type_, name) {
  var options = optionsOpt !== undefined ? optionsOpt : /* [] */0;
  return {
          field_name: name,
          field_number: number,
          field_label: "Oneof",
          field_type: type_,
          field_options: options
        };
}

var message_counter = {
  contents: 0
};

function extension_range_range(from, to_) {
  var to_$1 = typeof to_ === "string" ? /* To_max */0 : /* To_number */({
        _0: to_.VAL
      });
  return {
          TAG: /* Extension_range */1,
          _0: from,
          _1: to_$1
        };
}

function message(content, message_name) {
  message_counter.contents = message_counter.contents + 1 | 0;
  return {
          id: message_counter.contents,
          message_name: message_name,
          message_body: content
        };
}

function $$import($$public, file_name) {
  return {
          file_name: file_name,
          public: $$public !== undefined
        };
}

function extend(extend_name, extend_body) {
  message_counter.contents = message_counter.contents + 1 | 0;
  return {
          id: message_counter.contents,
          extend_name: extend_name,
          extend_body: extend_body
        };
}

function proto(syntax, file_option, $$package, $$import, message, $$enum, proto$1, extend, param) {
  var proto$2 = proto$1 !== undefined ? proto$1 : ({
        syntax: syntax,
        imports: /* [] */0,
        file_options: /* [] */0,
        package: undefined,
        messages: /* [] */0,
        enums: /* [] */0,
        extends: /* [] */0
      });
  var proto$3 = syntax !== undefined ? ({
        syntax: syntax,
        imports: proto$2.imports,
        file_options: proto$2.file_options,
        package: proto$2.package,
        messages: proto$2.messages,
        enums: proto$2.enums,
        extends: proto$2.extends
      }) : proto$2;
  var proto$4 = $$package !== undefined ? ({
        syntax: proto$3.syntax,
        imports: proto$3.imports,
        file_options: proto$3.file_options,
        package: $$package,
        messages: proto$3.messages,
        enums: proto$3.enums,
        extends: proto$3.extends
      }) : proto$3;
  var proto$5 = message !== undefined ? ({
        syntax: proto$4.syntax,
        imports: proto$4.imports,
        file_options: proto$4.file_options,
        package: proto$4.package,
        messages: {
          hd: message,
          tl: proto$2.messages
        },
        enums: proto$4.enums,
        extends: proto$4.extends
      }) : proto$4;
  var proto$6 = $$enum !== undefined ? ({
        syntax: proto$5.syntax,
        imports: proto$5.imports,
        file_options: proto$5.file_options,
        package: proto$5.package,
        messages: proto$5.messages,
        enums: {
          hd: $$enum,
          tl: proto$2.enums
        },
        extends: proto$5.extends
      }) : proto$5;
  var proto$7 = $$import !== undefined ? ({
        syntax: proto$6.syntax,
        imports: {
          hd: $$import,
          tl: proto$2.imports
        },
        file_options: proto$6.file_options,
        package: proto$6.package,
        messages: proto$6.messages,
        enums: proto$6.enums,
        extends: proto$6.extends
      }) : proto$6;
  var proto$8 = file_option !== undefined ? ({
        syntax: proto$7.syntax,
        imports: proto$7.imports,
        file_options: {
          hd: file_option,
          tl: proto$2.file_options
        },
        package: proto$7.package,
        messages: proto$7.messages,
        enums: proto$7.enums,
        extends: proto$7.extends
      }) : proto$7;
  if (extend !== undefined) {
    return {
            syntax: proto$8.syntax,
            imports: proto$8.imports,
            file_options: proto$8.file_options,
            package: proto$8.package,
            messages: proto$8.messages,
            enums: proto$8.enums,
            extends: {
              hd: extend,
              tl: proto$2.extends
            }
          };
  } else {
    return proto$8;
  }
}

function file_option(file_options, name) {
  var x;
  try {
    x = List.assoc(name, file_options);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return ;
    }
    throw exn;
  }
  return x;
}

function rev_split_by_char(c, s) {
  var loop = function (i, l) {
    try {
      var i$prime = $$String.index_from(s, i, c);
      var s$prime = $$String.sub(s, i, i$prime - i | 0);
      return loop(i$prime + 1 | 0, s$prime === "" ? l : ({
                      hd: s$prime,
                      tl: l
                    }));
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return {
                hd: $$String.sub(s, i, s.length - i | 0),
                tl: l
              };
      }
      throw exn;
    }
  };
  return loop(0, /* [] */0);
}

function pop_last(param) {
  if (param) {
    var tl = param.tl;
    if (tl) {
      return {
              hd: param.hd,
              tl: pop_last(tl)
            };
    } else {
      return /* [] */0;
    }
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Invalid argument [] for pop_last",
        Error: new Error()
      };
}

function apply_until(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var x = Curry._1(f, param.hd);
    if (x !== undefined) {
      return x;
    }
    _param = param.tl;
    continue ;
  };
}

function string_of_string_list(l) {
  return Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* '[' */91,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* ']' */93,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "[%s]"
                }), $$String.concat(",", l));
}

function string_fold_lefti(f, e0, s) {
  var len = s.length;
  var _acc = e0;
  var _i = 0;
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i === len) {
      return acc;
    }
    _i = i + 1 | 0;
    _acc = Curry._3(f, acc, i, s.charCodeAt(i));
    continue ;
  };
}

function option_default(x, y) {
  if (y !== undefined) {
    return Caml_option.valFromOption(y);
  } else {
    return x;
  }
}

function from_lexbuf(lexbuf) {
  var x = lexbuf.lex_curr_p.pos_fname;
  var file_name = x === "" ? undefined : x;
  var line = lexbuf.lex_curr_p.pos_lnum;
  return {
          file_name: file_name,
          line: line
        };
}

function file_name(param) {
  return param.file_name;
}

function line(param) {
  return param.line;
}

function to_string(param) {
  return Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "File ",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: ", line ",
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_i */3,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: ":\n",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "File %s, line %i:\n"
                }), option_default("", param.file_name), param.line);
}

function string_of_programmatic_error(e) {
  var tmp;
  switch (e) {
    case /* Invalid_string_split */0 :
        tmp = "string split error";
        break;
    case /* Unexpected_field_type */1 :
        tmp = "unexpected field type";
        break;
    case /* No_type_found_for_id */2 :
        tmp = "no type was found for type id";
        break;
    case /* One_of_should_be_inlined_in_message */3 :
        tmp = "one of variant encoding must be inlined in message";
        break;
    
  }
  return "Programatic_error" + tmp;
}

var Compilation_error = /* @__PURE__ */Caml_exceptions.create("Ocaml_proto_test.Exception.Compilation_error");

function prepare_error(e) {
  if (typeof e === "number") {
    return Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "Syntax error",
                  _1: /* End_of_format */0
                },
                _1: "Syntax error"
              });
  }
  switch (e.TAG | 0) {
    case /* Unresolved_type */0 :
        var match = e._0;
        return Curry._3(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "unresolved type for field name : ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " (type:",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: ", in message: ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* Char_literal */12,
                                      _0: /* ')' */41,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "unresolved type for field name : %s (type:%s, in message: %s)"
                      }), match.field_name, match.type_, match.message_name);
    case /* Duplicated_field_number */1 :
        var match$1 = e._0;
        return Curry._3(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "duplicated field number for field name: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " (previous field name:",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: ", message: ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* Char_literal */12,
                                      _0: /* ')' */41,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "duplicated field number for field name: %s (previous field name:%s, message: %s)"
                      }), match$1.field_name, match$1.previous_field_name, match$1.message_name);
    case /* Invalid_default_value */2 :
        var match$2 = e._0;
        return Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "invalid default value for field name:",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " (info: ",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* Char_literal */12,
                                  _0: /* ')' */41,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "invalid default value for field name:%s (info: %s)"
                      }), option_default("", match$2.field_name), match$2.info);
    case /* Unsupported_field_type */3 :
        var match$3 = e._0;
        return Curry._3(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "unsupported field type for field name:",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " with type:",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: " in bakend: ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "unsupported field type for field name:%s with type:%s in bakend: %s"
                      }), option_default("", match$3.field_name), match$3.field_type, match$3.backend_name);
    case /* Programatic_error */4 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "programmatic error: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "programmatic error: %s"
                      }), string_of_programmatic_error(e._0));
    case /* Invalid_import_qualifier */5 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Invalid import qualified, only 'public' supported",
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "%sInvalid import qualified, only 'public' supported"
                      }), to_string(e._0));
    case /* Invalid_file_name */6 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "Invalid file name: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: ", format must <name>.proto",
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "Invalid file name: %s, format must <name>.proto"
                      }), e._0);
    case /* Import_file_not_found */7 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "File: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: ", could not be found.",
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "File: %s, could not be found."
                      }), e._0);
    case /* Invalid_packed_option */8 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "Invalid packed option for field: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "Invalid packed option for field: %s"
                      }), e._0);
    case /* Missing_semicolon_for_enum_value */9 :
        return Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Missing semicolon for enum value: ",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%sMissing semicolon for enum value: %s"
                      }), to_string(e._1), e._0);
    case /* Invalid_enum_specification */10 :
        return Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Missing enum specification (<identifier> = <id>;) for enum value: ",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%sMissing enum specification (<identifier> = <id>;) for enum value: %s"
                      }), to_string(e._1), e._0);
    case /* Invalid_mutable_option */11 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "Invalid mutable option for field ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "Invalid mutable option for field %s"
                      }), option_default("", e._0));
    case /* Missing_one_of_name */12 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Missing oneof name",
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "%sMissing oneof name"
                      }), to_string(e._0));
    case /* Invalid_field_label */13 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Invalid field label. [required|repeated|optional] expected",
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "%sInvalid field label. [required|repeated|optional] expected"
                      }), to_string(e._0));
    case /* Missing_field_label */14 :
        return Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: "Missing field label. [required|repeated|optional] expected",
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "%sMissing field label. [required|repeated|optional] expected"
                      }), to_string(e._0));
    case /* Parsing_error */15 :
        return Curry._3(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "File ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: ", line ",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_i */3,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* String_literal */11,
                                  _0: ":\n",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "File %s, line %i:\n%s"
                      }), e._0, e._1, e._2);
    
  }
}

function add_loc(loc, exn) {
  if (exn.RE_EXN_ID === Compilation_error) {
    var tmp = exn._1;
    if (typeof tmp !== "number") {
      switch (tmp.TAG | 0) {
        case /* Invalid_import_qualifier */5 :
        case /* Missing_semicolon_for_enum_value */9 :
        case /* Invalid_enum_specification */10 :
        case /* Missing_one_of_name */12 :
        case /* Invalid_field_label */13 :
        case /* Missing_field_label */14 :
            return exn;
        default:
          
      }
    }
    
  }
  var file_name$1 = option_default("", file_name(loc));
  var line$1 = line(loc);
  var detail = Printexc.to_string(exn);
  return {
          RE_EXN_ID: Compilation_error,
          _1: {
            TAG: /* Parsing_error */15,
            _0: file_name$1,
            _1: line$1,
            _2: detail
          }
        };
}

Printexc.register_printer(function (exn) {
      if (exn.RE_EXN_ID === Compilation_error) {
        return prepare_error(exn._1);
      }
      
    });

function invalid_default_value(field_name, info, param) {
  throw {
        RE_EXN_ID: Compilation_error,
        _1: {
          TAG: /* Invalid_default_value */2,
          _0: {
            field_name: field_name,
            info: info
          }
        },
        Error: new Error()
      };
}

function unsupported_field_type(field_name, field_type, backend_name, param) {
  throw {
        RE_EXN_ID: Compilation_error,
        _1: {
          TAG: /* Unsupported_field_type */3,
          _0: {
            field_name: field_name,
            field_type: field_type,
            backend_name: backend_name
          }
        },
        Error: new Error()
      };
}

function invalid_enum_specification(enum_name, loc) {
  throw {
        RE_EXN_ID: Compilation_error,
        _1: {
          TAG: /* Invalid_enum_specification */10,
          _0: enum_name,
          _1: loc
        },
        Error: new Error()
      };
}

var yytransl_const = [
  257,
  258,
  259,
  261,
  262,
  263,
  265,
  266,
  267,
  268,
  269,
  270,
  271,
  272,
  273,
  274,
  275,
  276,
  277,
  278,
  279,
  280,
  281,
  282,
  283,
  0,
  0
];

var yytransl_block = [
  260,
  264,
  284,
  285,
  286,
  287,
  0
];

var yyact = [
  (function (param) {
      throw {
            RE_EXN_ID: "Failure",
            _1: "parser",
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(_1, undefined, undefined, undefined, undefined, undefined, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, _1, undefined, undefined, undefined, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, _1, undefined, undefined, undefined, undefined, undefined, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, _1, undefined, undefined, undefined, undefined, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, _1, undefined, undefined, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, undefined, _1, undefined, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, undefined, undefined, undefined, _1, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, _1, undefined, undefined, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, _1, undefined, undefined, undefined, undefined, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, _1, undefined, undefined, undefined, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, _1, undefined, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, undefined, _1, _2, undefined, undefined);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return proto(undefined, undefined, undefined, undefined, undefined, undefined, _2, _1, undefined);
    }),
  (function (__caml_parser_env) {
      var _3 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return _3;
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 2);
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return $$import(undefined, _2);
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 3);
      var _3 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return $$import(Caml_option.some(undefined), _3);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 3);
      Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Invalid_import_qualifier */5,
              _0: _1
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return _2[1];
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return message(_4, _2[1]);
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 0);
      return message(/* [] */0, _2[1]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _2
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_field */0,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_map_field */1,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_oneof_field */2,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_sub */3,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_enum */4,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Message_extension */5,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Compilation_error,
            _1: /* Syntax_error */0,
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return extend(_2[1], _4);
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 0);
      return extend(_2[1], /* [] */0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _2
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return _2;
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      return {
              TAG: /* Extension_single_number */0,
              _0: Parsing.peek_val(__caml_parser_env, 0)
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return extension_range_range(_1, {
                  NAME: "Number",
                  VAL: _3
                });
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      return extension_range_range(_1, "Max");
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 4);
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return {
              oneof_name: _2[1],
              oneof_fields: _4
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 3);
      Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Missing_one_of_name */12,
              _0: _1
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _2
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 5);
      var _2 = Parsing.peek_val(__caml_parser_env, 4);
      var _4 = Parsing.peek_val(__caml_parser_env, 2);
      var _5 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return oneof_field(_5, _4, _1[1], _2);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 4);
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return oneof_field(undefined, _4, _1[1], _2);
    }),
  (function (__caml_parser_env) {
      var _3 = Parsing.peek_val(__caml_parser_env, 7);
      var _5 = Parsing.peek_val(__caml_parser_env, 5);
      var _7 = Parsing.peek_val(__caml_parser_env, 3);
      var _9 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return map(undefined, _9, _3[1], _5[1], _7);
    }),
  (function (__caml_parser_env) {
      var _3 = Parsing.peek_val(__caml_parser_env, 8);
      var _5 = Parsing.peek_val(__caml_parser_env, 6);
      var _7 = Parsing.peek_val(__caml_parser_env, 4);
      var _9 = Parsing.peek_val(__caml_parser_env, 2);
      var _10 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return map(_10, _9, _3[1], _5[1], _7);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 6);
      var _2 = Parsing.peek_val(__caml_parser_env, 5);
      var _3 = Parsing.peek_val(__caml_parser_env, 4);
      var _5 = Parsing.peek_val(__caml_parser_env, 2);
      var _6 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return field(_6, _1, _5, _2[1], _3);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 5);
      var _2 = Parsing.peek_val(__caml_parser_env, 4);
      var _3 = Parsing.peek_val(__caml_parser_env, 3);
      var _5 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return field(undefined, _1, _5, _2[1], _3);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 5);
      Parsing.peek_val(__caml_parser_env, 4);
      Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Missing_field_label */14,
              _0: _1[0]
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 4);
      Parsing.peek_val(__caml_parser_env, 3);
      Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Missing_field_label */14,
              _0: _1[0]
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0)[1];
    }),
  (function (__caml_parser_env) {
      return "required";
    }),
  (function (__caml_parser_env) {
      return "optional";
    }),
  (function (__caml_parser_env) {
      return "repeated";
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 0);
      return "oneof";
    }),
  (function (__caml_parser_env) {
      return "enum";
    }),
  (function (__caml_parser_env) {
      return "package";
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 0);
      return "import";
    }),
  (function (__caml_parser_env) {
      return "public";
    }),
  (function (__caml_parser_env) {
      return "option";
    }),
  (function (__caml_parser_env) {
      return "extensions";
    }),
  (function (__caml_parser_env) {
      return "extend";
    }),
  (function (__caml_parser_env) {
      return "syntax";
    }),
  (function (__caml_parser_env) {
      return "message";
    }),
  (function (__caml_parser_env) {
      return "to";
    }),
  (function (__caml_parser_env) {
      return "max";
    }),
  (function (__caml_parser_env) {
      return "map";
    }),
  (function (__caml_parser_env) {
      return "Required";
    }),
  (function (__caml_parser_env) {
      return "Repeated";
    }),
  (function (__caml_parser_env) {
      return "Optional";
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Invalid_field_label */13,
              _0: _1[0]
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return [
              _1[1],
              _3
            ];
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _5 = Parsing.peek_val(__caml_parser_env, 0);
      return [
              _2[1],
              _5
            ];
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0)[1];
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1)[1];
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return _1 + _2[1];
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return [
              _2,
              _4
            ];
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Constant_int */2,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Constant_float */3,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      var litteral = _1[1];
      switch (litteral) {
        case "false" :
            return {
                    TAG: /* Constant_bool */1,
                    _0: false
                  };
        case "true" :
            return {
                    TAG: /* Constant_bool */1,
                    _0: true
                  };
        default:
          return {
                  TAG: /* Constant_litteral */4,
                  _0: litteral
                };
      }
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Constant_string */0,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      var enum_valuesOpt = _4;
      var enum_name = _2[1];
      var enum_values = enum_valuesOpt !== undefined ? enum_valuesOpt : /* [] */0;
      message_counter.contents = message_counter.contents + 1 | 0;
      return {
              enum_id: message_counter.contents,
              enum_name: enum_name,
              enum_values: enum_values
            };
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _2
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 3);
      var _3 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return {
              enum_value_name: _1[1],
              enum_value_int: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 0);
      var enum_value = _1[1];
      var loc = _1[0];
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Missing_semicolon_for_enum_value */9,
              _0: enum_value,
              _1: loc
            },
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 3);
      Parsing.peek_val(__caml_parser_env, 1);
      return invalid_enum_specification(_1[1], _1[0]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      return invalid_enum_specification(_1[1], _1[0]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      return invalid_enum_specification(_1[1], _1[0]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return invalid_enum_specification(_1[1], _1[0]);
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 1);
      
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      Parsing.peek_val(__caml_parser_env, 1);
      
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    })
];

var yytables = {
  actions: yyact,
  transl_const: yytransl_const,
  transl_block: yytransl_block,
  lhs: "\xff\xff\x01\0\x02\0\x03\0\x04\0\x05\0\x06\0\b\0\t\0\n\0\x0b\0\f\0\x07\0\x18\0\x18\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x19\0\x13\0\x13\0\x13\0\x1b\0\x12\0\x12\0\x1d\0\x1d\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x17\0\x17\0!\0!\0\x16\0\x15\0\x15\0\"\0\"\0\"\0\x11\0\x11\0#\0#\0$\0$\0 \0 \0\x0e\0\x0e\0\x0e\0\x0e\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0&\0&\0&\0&\0\r\0\r\0'\0'\0(\0(\0*\0*\0+\0+\0\x14\0)\0)\0)\0)\0\x10\0,\0,\0\x0f\0\x0f\0\x0f\0\x0f\0\x0f\0\x0f\0\x1c\0\x1c\0\x1e\0\x1e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  len: "\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x04\0\x03\0\x04\0\x04\0\x03\0\x05\0\x04\0\x01\0\x02\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x05\0\x04\0\x01\0\x02\0\x03\0\x01\0\x03\0\x01\0\x03\0\x03\0\x05\0\x04\0\0\0\x02\0\x06\0\x05\0\n\0\x0b\0\x07\0\x06\0\x06\0\x05\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x03\0\x02\0\x01\0\x03\0\x03\0\x05\0\x01\0\x03\0\x01\0\x02\0\x05\0\x01\0\x01\0\x01\0\x01\0\x05\0\0\0\x02\0\x04\0\x03\0\x04\0\x02\0\x02\0\x01\0\x01\0\x02\0\x01\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0",
  defred: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0r\0\0\0R\0T\0S\0\0\0s\0\0\0\0\0\0\0t\0\0\0\0\0u\0\0\0\0\0v\0\0\0\0\0w\0\0\0\0\0\0\0\0\0\0\0\0\0x\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0e\0\0\0y\0\0\0z\0\0\0\0\0{\0\0\0\0\0\0\0|\0\0\0}\0\0\0W\0\0\0\0\0\0\0\0\0\x01\0B\0C\0D\0E\0N\0F\0G\0H\0I\0J\0K\0L\0M\0O\0P\0Q\0A\0\0\0\x02\0\0\0\0\0l\0k\0\x03\0\0\0\x04\0\0\0\0\0\x05\0\0\0\x06\0\0\0\0\0\0\0\0\0\0\0\\\0^\0\0\0\0\0\0\0\x19\0\x18\0\x15\0\x16\0\x1a\0\f\0\r\0\x17\0\x07\0\b\0\0\0\t\0\0\0\0\0\n\0\x0b\0\0\0\0\0V\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0n\0\0\0\0\0\0\0\0\0\0\0\0\0_\0\0\0\0\x004\x003\x001\0\0\0\0\0d\0a\0b\0c\0Z\0Y\0\0\0\0\0j\0\0\0\0\0\0\0\0\0p\0\0\x008\0\0\0*\0\0\0$\0(\0&\0'\0)\0\0\0\0\0\0\0%\0o\0\0\0\0\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0g\0\0\0\0\0q\0\0\0\0\0\0\0#\0\0\0.\0\0\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  dgoto: "\r\0\x0f\0\x15\0\x19\0\x1c\0\x1f\0\"\0)\x003\x005\x008\0<\0>\0\x10\0\xae\0\xa5\0*\0\xb0\0+\0,\0-\x009\0\xb2\0.\0/\x000\x001\x002\0\x8d\0\xb3\0\xa9\0\xb5\0\xb6\0\xbe\0:\0\x88\0\x89\0W\0\x17\0C\0D\0\x9f\0k\0l\0\xa6\0",
  sindex: "\xd2\0\xf5\xfe\x13\xff\xed\xfe\n\xff\x1e\xff4\xff\x89\xff:\xffF\xff5\xffV\xff[\xff\0\0\x18\xff\0\0b\0\0\0\0\0\0\0\xe1\xff\0\0i\0K\xffB\xff\0\0k\0M\xff\0\0z\0\x06\xff\0\0{\0]\xff\0\0\x7f\0a\xff\xfe\xfe\r\xffd\xffl\xff\0\0\x89\xff\x89\xff\x89\xff\x89\xff\x89\xff\x86\0\xa0\xff\0\0\x89\xff\0\0\x88\0\0\0\x8b\0\x86\xff\0\0\x97\0}\xff5\xff\0\0\x9a\0\0\0\x9c\0\0\0~\xff\x97\xff\x9f\xff\x96\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x9a\xff\0\0\xe1\xff\x98\xff\0\0\0\0\0\0\xa4\xff\0\0\x9c\xff\xa7\xff\0\0\xaa\xff\0\0\xa5\xff\xa2\xff\xa5\xff\xb3\xff\xa1\xff\0\0\0\0\x05\xff\xbf\xff\xc3\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xfe\0\x005\xff\xa5\xff\0\0\0\0\xcb\xff9\xff\0\0\x1f\xff\xda\xff\xdf\xff,\xff\xed\xfe\xe1\xff\xe8\xff\x9c\xff\x9c\xffI\xff\0\0\xe2\xff\xa5\xff\xe2\xff\xa5\xff\xe6\xff9\xff\0\0\x10\xff\xa5\xff\0\0\0\0\0\0\xe2\xff\xe4\xff\0\0\0\0\0\0\0\0\0\0\0\0)\xff\xed\xff\0\0\xe2\xff\xed\xfe\xe8\xff\xe5\xff\0\0\xf5\xff\0\0\xe8\xff\0\0\xe7\xff\0\0\0\0\0\0\0\0\0\0\xe8\xff\xf5\xff\xc2\xff\0\0\0\0\xe2\xff\xe2\xff\0\0\xa5\xff\x13\xff\xf5\xff\xe8\xff\xe2\xff9\xff\xa5\xff\xe2\xff)\xff\0\0\xf5\xff\xf3\xff\0\0\xf5\xff\xf4\xff\xf5\xff\0\0\xe2\xff\0\0\xf5\xff\0\0\xe2\xff\xa5\xff\xe2\xff)\xff\xf9\xff\xe2\xff\xa5\xff\xe2\xff\xf6\xff\xe2\xff\x13\0\xe1\xff\xfd\xff\x0e\0)\xff\xa5\xff\xe2\xff\xe2\xff",
  rindex: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0*\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0,\x01<\x01F\x01G\x01H\x01\0\0\0\0\0\0N\x01\0\0\0\0\0\0\0\0\x0b\0\0\0\0\0\x0f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0E\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0I\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0:\0L\0\0\0\0\0I\0I\0\0\0\0\0e\0\0\0w\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0?\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0<\0L\0\0\0\0\0\0\0Q\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x01\0N\0\0\0\0\0\x89\0\x9b\0\0\0\0\0O\0\xad\0\0\0\xc4\xff\0\0\0\0c\0\0\0\0\0\x17\0\0\0\0\0u\0\0\0-\0\0\0\xb8\0\0\0\xc1\0\0\0\x87\0\0\0\x99\0\0\0\0\0\xab\0\0\0\xfc\xfe\0\0\b\xff\0\0\0\0\0\0\0\0\0\0\0\0m\xff\x9e\xff",
  gindex: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\xff\xfe\xff\xbe\x01\xff\xff\xc7\x01\x02\0\xbb\x01\xc6\x01\xca\xff\xc5\x01\xc8\x01\xc8\0\0\0\xa1\x01\0\0\xa3\xff\x1d\x01\x95\xff\0\0\0\0\x17\x01\0\0\xd6\xff\0\0\xa9\xff\0\0S\x01\0\0t\xff\0\0\0\x001\x01",
  tablesize: 470,
  table: "\x16\0!\0\x84\0\x1d\0\xc1\0|\0\xbb\0f\0#\0\x0e\0\x8f\x002\0\x18\0:\0\x96\x000\0\x1b\0\x11\0\x12\0\x13\0\x11\0\x12\0\x13\0e\0`\x009\0g\0:\0\x97\0h\0\x92\0\x99\0\xb4\0\xa8\0\x1e\0i\0\x93\0a\0\xd1\x009\0\xa4\0\xbd\0m\0@\0j\0 \0A\0\x14\0\xa7\0\xb8\0\x14\0\xb9\0\xcf\0A\0\xd6\0B\0\xbf\0!\0i\0\xc5\0h\0\x0e\0B\0/\0\xc8\0\xdf\0%\0\x8c\0\xc2\0\x98\0\x8c\0\xa3\0\xca\0\xac\0\x11\0\x12\0\x13\0\x1e\0!\0\x1b\0&\x006\x007\0\xce\0;\0\x9b\0\x9c\0\x9d\0\x9e\0\xad\0\xa8\0Z\0[\0\\\0\xcc\0\xaa\0\xab\0;\0E\0@\0\xd0\0\x1f\0\xd2\0'\0\x14\0X\0Y\0]\0^\0;\0;\0;\0;\0;\0;\0;\0\xd5\x005\0\xd7\0\x1c\0;\0\xd9\0_\0b\0c\0;\0;\0d\0e\0\xe0\0\xe1\0m\0\xdc\0n\0t\0?\0w\0\x1d\0\xaf\0x\0;\0\xb1\0!\0\x1b\0$\0%\0\xbc\0&\0y\0'\0(\0z\0{\0>\0}\0\x1e\0~\0\x7f\0<\0<\0<\0<\0<\0<\0<\0!\0\x1b\0$\0%\0<\0&\0=\0'\0,\0<\0<\0\x80\0\x82\0\x81\0\x83\0\xaf\0\x85\0\x86\0\xb1\0`\0\x8a\0\xbc\0\x87\0\x8b\0<\0\x8e\0\x8c\0\x91\0+\0\xac\0\x11\0\x12\0\x13\0\x1e\0!\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0;\0\x1b\0\x90\0\x1b\0\x94\0\xad\0\x01\0\x02\0\x03\0\x04\0\x05\0\x06\0\x07\0\b\0\t\0\n\0\x0b\0\f\0\x95\0\x9a\0\x14\0F\0G\0H\0I\0J\0K\0L\0M\0N\0O\0P\0Q\0R\0S\0T\0U\0o\0p\0q\0r\0s\0\xa1\0\xa2\0\xa8\0v\0\xba\0\xb7\0\xc0\0\xc6\0\xc9\0V\0!\0!\0!\0!\0!\0!\0!\0!\0!\0\xc3\0!\0!\0!\0!\0\xc7\0\xd3\0!\0!\0\xd4\0\xd8\0\xda\0\xdd\0e\0e\0e\0e\0e\0e\0e\0e\0e\0!\0e\0e\0e\0e\x002\x002\0e\0e\x000\0\xdb\0\xde\0\x13\0 \0 \0 \0 \0 \0 \0 \0 \0 \0e\0 \0 \0 \0 \0m\0\x12\0 \0 \0/\0/\0/\0/\0/\0/\0/\0\x0f\0\x10\0\x14\0m\0/\0i\0 \0h\0\x11\0/\0/\x006\x006\x006\x006\x006\x006\x006\0X\0i\x007\0h\x006\0f\0/\0\"\0-\x006\x006\0@\0@\0@\0@\0@\0@\0@\0\x1f\0\x1f\0\x1f\0\x1f\0@\0\x1f\x006\0\x1f\0\x1f\0@\0@\x005\x005\x005\x005\x005\x005\x005\0\x1c\0\x1c\0\x1c\0\x1c\x005\0\x1c\0@\0\x1c\0\x1c\x005\x005\0?\0?\0?\0?\0?\0?\0?\0\x1d\0\x1d\0\x1d\0\x1d\0?\0\x1d\x005\0\x1d\0\x1d\0?\0?\0>\0>\0>\0>\0>\0>\0>\0\x1e\0\x1e\0\x1e\0\x1e\0>\0\x1e\0?\0\x1e\0\x1e\0>\0>\0=\0=\0=\0=\0=\0=\0=\0,\0,\0,\0,\0=\0,\0>\0,\0,\0=\0=\0`\0`\0`\0`\0\x1a\0`\x004\0`\0`\0+\0+\0+\0+\0=\0+\0 \0+\0+\x006\0=\0u\0\xcb\0\xcd\0?\0\xa0\0\xc4\0",
  check: "\x02\0\0\0Y\0\x04\0\xa1\0;\0\x92\0\t\x01\x06\0\x14\x01g\0\0\0\x1f\x01\x11\x01\x0f\x01\0\0\x06\x01\x01\x01\x02\x01\x03\x01\x01\x01\x02\x01\x03\x01\0\0\x12\x01\x11\x01\x1c\x01\x1f\x01\x1d\x01\x1f\x01\x19\x01|\0\x8b\0\x11\x01\x04\x01\x16\x01\x1f\x01\x1f\x01\xc3\0\x1f\x01\x85\0\x94\0\0\0\x13\x01\x1f\x01\0\0\x16\x01\x1f\x01\x87\0\x8e\0\x1f\x01\x90\0\xc0\0\x16\x01\xd3\0\x1f\x01\x95\0\x05\x01\0\0\xa6\0\0\0\x14\x01\x1f\x01\0\0\xab\0\xde\0\b\x01\x1a\x01\xa1\0{\0\x1a\x01\x1b\x01\xb3\0\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\n\x01\0\0\x1d\x01\xbe\0\x0b\x01\x1c\x01\x1d\x01\x1e\x01\x1f\x01\x10\x01\x11\x01\x19\x01\x1a\x01\x1b\x01\xbb\0\x89\0\x8a\0\x0b\x01\0\0\0\0\xc1\0\0\0\xc3\0\f\x01\x1f\x01\0\0\x1f\x01\0\0\x1f\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\xd1\0\0\0\xd3\0\0\0\x0b\x01\xd6\0\0\0\0\0\x1f\x01\x10\x01\x11\x01\0\0\x1f\x01\xde\0\xdf\0\x1f\x01\xdb\0\x19\x01\0\0\0\0\0\0\0\0\x8b\0\0\0\x1f\x01\x8b\0\x05\x01\x06\x01\x07\x01\b\x01\x94\0\n\x01\x0e\x01\f\x01\r\x01\0\0\x1b\x01\0\0\0\0\0\0\0\0\x1f\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\0\0\f\x01\0\0\x10\x01\x11\x01\x19\x01\x1b\x01\x13\x01\x19\x01\xb5\0\x1d\x01\x12\x01\xb5\0\0\0\x12\x01\xbc\0\x1f\x01\x12\x01\x1f\x01\x1c\x01\x1a\x01\x1f\x01\0\0\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1c\x01\f\x01\x12\x01\x10\x01\x01\0\x02\0\x03\0\x04\0\x05\0\x06\0\x07\0\b\0\t\0\n\0\x0b\0\f\0\x1c\x01\x15\x01\x1f\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x07\x01\b\x01\t\x01\n\x01\x0b\x01\f\x01\r\x01\x0e\x01\x0f\x01\x10\x01*\0+\0,\0-\0.\0\x1d\x01\x19\x01\x11\x012\0\x15\x01\x1a\x01\x19\x01\x19\x01\x18\x01\x1f\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x07\x01\b\x01\x1d\x01\n\x01\x0b\x01\f\x01\r\x01\x1a\x01\x1d\x01\x10\x01\x11\x01\x1f\x01\x1b\x01\x1f\x01\x19\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x07\x01\b\x01\x1f\x01\n\x01\x0b\x01\f\x01\r\x01\x1a\x01\x1b\x01\x10\x01\x11\x01\x1a\x01\x17\x01\x1d\x01\0\0\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x07\x01\b\x01\x1f\x01\n\x01\x0b\x01\f\x01\r\x01\x11\x01\0\0\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\0\0\0\0\0\0\x1f\x01\x0b\x01\x11\x01\x1f\x01\x11\x01\0\0\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x13\x01\x1f\x01\x11\x01\x1f\x01\x0b\x01\x11\x01\x1f\x01\x11\x01\x11\x01\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1f\x01\f\x01\r\x01\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1f\x01\f\x01\r\x01\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1f\x01\f\x01\r\x01\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1f\x01\f\x01\r\x01\x10\x01\x11\x01\0\x01\x01\x01\x02\x01\x03\x01\x04\x01\x05\x01\x06\x01\x05\x01\x06\x01\x07\x01\b\x01\x0b\x01\n\x01\x1f\x01\f\x01\r\x01\x10\x01\x11\x01\x05\x01\x06\x01\x07\x01\b\x01\x03\0\n\x01\b\0\f\x01\r\x01\x05\x01\x06\x01\x07\x01\b\x01\x1f\x01\n\x01\x05\0\f\x01\r\x01\t\0\x0b\x000\0\xb5\0\xbc\0\f\0\x82\0\xa5\0",
  error_function: Parsing.parse_error,
  names_const: "REQUIRED\0OPTIONAL\0REPEATED\0MESSAGE\0ENUM\0PACKAGE\0PUBLIC\0OPTION\0EXTENSIONS\0EXTEND\0SYNTAX\0TO\0MAX\0MAP\0RBRACE\0LBRACE\0RBRACKET\0LBRACKET\0RPAREN\0LPAREN\0RANGLEB\0LANGLEB\0EQUAL\0SEMICOLON\0COMMA\0EOF\0",
  names_block: "ONE_OF\0IMPORT\0STRING\0INT\0FLOAT\0IDENT\0"
};

function proto_(lexfun, lexbuf) {
  return Parsing.yyparse(yytables, 7, lexfun, lexbuf);
}

function update_loc(lexbuf) {
  var pos = lexbuf.lex_curr_p;
  lexbuf.lex_curr_p = {
    pos_fname: pos.pos_fname,
    pos_lnum: pos.pos_lnum + 1 | 0,
    pos_bol: pos.pos_cnum,
    pos_cnum: pos.pos_cnum
  };
  
}

var __ocaml_lex_tables = {
  lex_base: "\0\0\xea\xff\xeb\xffN\0\xed\xff\xee\xff\x01\0\xa0\0\xf0\0;\x01\x88\x01\x9e\x01\xf2\xff\x10\0\xf5\xff\xf6\xff\xf7\xff\xf8\xff\xf9\xff\xfa\xff\xfb\xff\xfc\xff\xfd\xff\xfe\xff\xff\xff\xf3\xff\xf4\xff\x1a\0\xbe\x01\xc8\x01\x92\x01\xa8\x01#\0\xef\xff\xed\x01:\x02\x87\x02\xd4\x02!\x03n\x03\x05\0\x12\x01\xfd\xff\xfe\xff\xff\xff\x06\0\x07\0!\x01\xfc\xff\xfd\xff\x11\0\xff\xff\x0b\0\f\0\xfe\xff\xc2\x01\xfc\xff\xfd\xff\xfe\xff\xc9\x03\xff\xff",
  lex_backtrk: "\x0f\0\xff\xff\xff\xff\x13\0\xff\xff\xff\xff\x15\0\x13\0\x13\0\x0f\0\x0e\0\x0f\0\xff\xff\x15\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\0\x0f\0\xff\xff\xff\xff\xff\xff\xff\xff\x13\0\x0f\0\x13\0\x13\0\x10\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\x02\0\xff\xff\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x02\0\xff\xff",
  lex_default: "\x01\0\0\0\0\0\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff+\0\0\0\0\0\0\0\xff\xff\xff\xff1\0\0\0\0\0\xff\xff\0\0\xff\xff\xff\xff\0\x009\0\0\0\0\0\0\0\xff\xff\0\0",
  lex_trans: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\0\x05\0\x05\0\x04\0\x06\0(\0\x05\0,\0,\0(\0.\0.\x003\x003\0\0\x005\x005\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\0\0\0\f\0\0\0\0\0\0\0\0\0\0\0\x13\0\x14\0\0\0\x0b\0\x0e\0\x0b\0\t\0\r\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\x19\0\x0f\0\x12\0\x10\0\x11\0\x1a\x006\0\x03\0\x03\0\x03\0\x03\0\b\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x16\0\0\0\x15\0\0\0\0\0\0\0\x03\0\x03\0\x03\0\x03\0\b\0\x03\0\x03\0\x03\0\x07\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x18\0\"\0\x17\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0 \0!\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\x03\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\0\0\"\0\0\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\x03\0\x02\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0&\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\x1f\0,\0\x1f\0\"\0-\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0\0\x003\0\0\0\0\x004\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\x002\0\0\0\0\0\0\0\x03\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x03\0\x03\0\x03\0\x03\0\b\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\0\0\0\0\0\0\0\0\0\0\0\0\x03\0\x03\0\x03\0\x03\0\b\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x1d\0\0\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1d\0\x1c\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\0\0\x1c\0:\0\0\0\0\0\0\0\0\0\x1f\0\0\0\x1f\0\0\0\x1c\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\0\0\x1c\0\0\0\0\0\0\0\x1b\0\0\0\0\0\0\0\0\0\0\0\x1c\0\0\0\0\0\0\0\0\0*\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\"\0\0\0\0\0;\0\0\0\0\x000\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x1c\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\0\0\0\0\0\0\0\0\0\0\0\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\"\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0#\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\"\0\0\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0\0\x008\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\x03\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\"\0\0\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\0\0\0\0\0\0\0\0%\0\0\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\"\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0#\0\0\0#\0#\0#\0#\0#\0'\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\"\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\0\0\0\0\0\0\0\0#\0\0\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0<\0\0\0<\0\0\0\0\0\0\0\0\0<\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0<\0\0\0\0\0\0\0\0\0\0\0<\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0<\0\0\0\0\0\0\0<\0\0\0<\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  lex_check: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\x06\0\0\0\0\0\x06\0(\0-\0.\0(\0-\0.\x004\x005\0\xff\xff4\x005\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\r\0\0\0\0\0\0\0\0\0\r\x002\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x03\0\0\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x1b\0 \0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\xff\xff\xff\xff\xff\xff\xff\xff\x03\0\xff\xff\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\x03\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\0\xff\xff\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\xff\xff\xff\xff\xff\xff\xff\xff\x07\0\0\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\x07\0\b\0)\0\b\0\b\0)\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\xff\xff/\0\xff\xff\xff\xff/\0\xff\xff\xff\xff\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0/\0\xff\xff\xff\xff\xff\xff\b\0\xff\xff\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\b\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\n\0\xff\xff\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\n\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x1e\0\x0b\0\n\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\x1f\0\xff\xff\x0b\x007\0\xff\xff\xff\xff\xff\xff\xff\xff\x1c\0\xff\xff\x1c\0\xff\xff\n\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\xff\xff\x0b\0\xff\xff\xff\xff\xff\xff\x0b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\0\xff\xff\xff\xff\xff\xff\xff\xff)\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\"\0\xff\xff\xff\xff7\0\xff\xff\xff\xff/\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1d\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0\"\0#\0\xff\xff#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0\xff\xff\xff\xff\xff\xff\xff\xff#\0\xff\xff#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0#\0$\0\xff\xff$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0\xff\xff7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0\xff\xff\xff\xff\xff\xff\xff\xff$\0\xff\xff$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0$\0%\0\xff\xff%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0\xff\xff\xff\xff\xff\xff\xff\xff%\0\xff\xff%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0%\0&\0\xff\xff&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0\xff\xff\xff\xff\xff\xff\xff\xff&\0\xff\xff&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0'\0\xff\xff'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0\xff\xff\xff\xff\xff\xff\xff\xff'\0\xff\xff'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0'\0;\0\xff\xff;\0\xff\xff\xff\xff\xff\xff\xff\xff;\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff;\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff;\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff;\0\xff\xff\xff\xff\xff\xff;\0\xff\xff;\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
  lex_base_code: "",
  lex_backtrk_code: "",
  lex_default_code: "",
  lex_trans_code: "",
  lex_check_code: "",
  lex_code: ""
};

function __ocaml_lex_string_rec(_l, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var l = _l;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var c = Lexing.lexeme_char(lexbuf, 1);
          ___ocaml_lex_state = 55;
          _l = {
            hd: Char.escaped(c),
            tl: l
          };
          continue ;
      case 1 :
          return /* String_value */{
                  _0: $$String.concat("", List.rev(l))
                };
      case 2 :
          ___ocaml_lex_state = 55;
          _l = {
            hd: Lexing.lexeme(lexbuf),
            tl: l
          };
          continue ;
      case 3 :
          return /* String_eof */0;
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function __ocaml_lex_comment_rec(_l, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var l = _l;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          update_loc(lexbuf);
          return /* Comment_value */{
                  _0: $$String.concat("", List.rev(l))
                };
      case 1 :
          ___ocaml_lex_state = 41;
          _l = {
            hd: Lexing.lexeme(lexbuf),
            tl: l
          };
          continue ;
      case 2 :
          return /* Comment_eof */0;
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function __ocaml_lex_multi_line_comment_rec(_l, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var l = _l;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          update_loc(lexbuf);
          ___ocaml_lex_state = 47;
          continue ;
      case 1 :
          Lexing.lexeme(lexbuf);
          return /* Comment_value */{
                  _0: $$String.concat("", List.rev(l))
                };
      case 2 :
          ___ocaml_lex_state = 47;
          _l = {
            hd: Lexing.lexeme(lexbuf),
            tl: l
          };
          continue ;
      case 3 :
          return /* Comment_eof */0;
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function lexer(lexbuf) {
  var ___ocaml_lex_state = 0;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          return /* LBRACE */15;
      case 1 :
          return /* RBRACE */14;
      case 2 :
          return /* LBRACKET */17;
      case 3 :
          return /* RBRACKET */16;
      case 4 :
          return /* RPAREN */18;
      case 5 :
          return /* LPAREN */19;
      case 6 :
          return /* LANGLEB */21;
      case 7 :
          return /* RANGLEB */20;
      case 8 :
          return /* EQUAL */22;
      case 9 :
          return /* SEMICOLON */23;
      case 10 :
          return /* COMMA */24;
      case 11 :
          var match = __ocaml_lex_comment_rec(/* [] */0, lexbuf, 41);
          if (!match) {
            return /* EOF */25;
          }
          ___ocaml_lex_state = 0;
          continue ;
      case 12 :
          var match$1 = __ocaml_lex_multi_line_comment_rec(/* [] */0, lexbuf, 47);
          if (!match$1) {
            return /* EOF */25;
          }
          ___ocaml_lex_state = 0;
          continue ;
      case 13 :
          var s = __ocaml_lex_string_rec(/* [] */0, lexbuf, 55);
          if (s) {
            return {
                    TAG: /* STRING */2,
                    _0: s._0
                  };
          } else {
            return /* EOF */25;
          }
      case 14 :
          return {
                  TAG: /* INT */3,
                  _0: Caml_format.caml_int_of_string(Lexing.lexeme(lexbuf))
                };
      case 15 :
          return {
                  TAG: /* FLOAT */4,
                  _0: Caml_format.caml_float_of_string(Lexing.lexeme(lexbuf))
                };
      case 16 :
          return {
                  TAG: /* FLOAT */4,
                  _0: Number.NaN
                };
      case 17 :
          update_loc(lexbuf);
          ___ocaml_lex_state = 0;
          continue ;
      case 18 :
          ___ocaml_lex_state = 0;
          continue ;
      case 19 :
          var loc = from_lexbuf(lexbuf);
          var ident = Lexing.lexeme(lexbuf);
          switch (ident) {
            case "enum" :
                return /* ENUM */4;
            case "extend" :
                return /* EXTEND */9;
            case "extensions" :
                return /* EXTENSIONS */8;
            case "import" :
                return {
                        TAG: /* IMPORT */1,
                        _0: loc
                      };
            case "map" :
                return /* MAP */13;
            case "max" :
                return /* MAX */12;
            case "message" :
                return /* MESSAGE */3;
            case "oneof" :
                return {
                        TAG: /* ONE_OF */0,
                        _0: loc
                      };
            case "option" :
                return /* OPTION */7;
            case "optional" :
                return /* OPTIONAL */1;
            case "package" :
                return /* PACKAGE */5;
            case "public" :
                return /* PUBLIC */6;
            case "repeated" :
                return /* REPEATED */2;
            case "required" :
                return /* REQUIRED */0;
            case "syntax" :
                return /* SYNTAX */10;
            case "to" :
                return /* TO */11;
            default:
              return {
                      TAG: /* IDENT */5,
                      _0: [
                        loc,
                        ident
                      ]
                    };
          }
      case 20 :
          return /* EOF */25;
      case 21 :
          var s$1 = Curry._1(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Unknown character found ",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "Unknown character found %s"
                  }), Lexing.lexeme(lexbuf));
          throw {
                RE_EXN_ID: "Failure",
                _1: s$1,
                Error: new Error()
              };
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function let_decl_of_and(param) {
  if (param !== undefined) {
    return "and";
  } else {
    return "let rec";
  }
}

function string_of_basic_type(param) {
  switch (param) {
    case /* Bt_string */0 :
        return "string";
    case /* Bt_float */1 :
        return "float";
    case /* Bt_int */2 :
        return "int";
    case /* Bt_int32 */3 :
        return "int32";
    case /* Bt_int64 */4 :
        return "int64";
    case /* Bt_bytes */5 :
        return "bytes";
    case /* Bt_bool */6 :
        return "bool";
    
  }
}

function string_of_field_type(bt) {
  if (typeof bt === "number") {
    return "unit";
  } else if (bt.TAG === /* Ft_basic_type */0) {
    return string_of_basic_type(bt._0);
  } else {
    var param = bt._0;
    var module_ = param.udt_module;
    if (module_ !== undefined) {
      return module_ + ("." + param.udt_type_name);
    } else {
      return param.udt_type_name;
    }
  }
}

function string_of_record_field_type(param) {
  switch (param.TAG | 0) {
    case /* Rft_required */0 :
        return string_of_field_type(param._0[0]);
    case /* Rft_optional */1 :
        return string_of_field_type(param._0[0]) + " option";
    case /* Rft_repeated_field */2 :
        var match = param._0;
        return string_of_field_type(match[1]) + (" " + (
                  match[0] ? "Pbrt.Repeated_field.t" : "list"
                ));
    case /* Rft_associative_field */3 :
        var match$1 = param._0;
        if (match$1[0]) {
          return Curry._3(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* '(' */40,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: ", ",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: ") ",
                                    _1: {
                                      TAG: /* String */2,
                                      _0: /* No_padding */0,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          },
                          _1: "(%s, %s) %s"
                        }), string_of_basic_type(match$1[2][0]), string_of_field_type(match$1[3][0]), "Hashtbl.t");
        } else {
          return Curry._3(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* '(' */40,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: " * ",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: ") ",
                                    _1: {
                                      TAG: /* String */2,
                                      _0: /* No_padding */0,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          },
                          _1: "(%s * %s) %s"
                        }), string_of_basic_type(match$1[2][0]), string_of_field_type(match$1[3][0]), "list");
        }
    case /* Rft_variant_field */4 :
        return param._0.v_name;
    
  }
}

function function_name_of_user_defined(prefix, param) {
  var module_ = param.udt_module;
  if (module_ !== undefined) {
    return Curry._3(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* '.' */46,
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* '_' */95,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    },
                    _1: "%s.%s_%s"
                  }), module_, prefix, param.udt_type_name);
  } else {
    return Curry._2(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* '_' */95,
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "%s_%s"
                  }), prefix, param.udt_type_name);
  }
}

function string_of_payload_kind(capitalize, payload_kind, packed) {
  var s;
  if (typeof payload_kind === "number") {
    switch (payload_kind) {
      case /* Pk_bits32 */0 :
          s = packed ? "bytes" : "bits32";
          break;
      case /* Pk_bits64 */1 :
          s = packed ? "bytes" : "bits64";
          break;
      case /* Pk_bytes */2 :
          s = "bytes";
          break;
      
    }
  } else {
    s = packed ? "bytes" : "varint";
  }
  if (capitalize !== undefined) {
    return Caml_bytes.bytes_to_string(Bytes.capitalize(Caml_bytes.bytes_of_string(s)));
  } else {
    return s;
  }
}

function line$1(scope, s) {
  scope.items = {
    hd: {
      TAG: /* Line */0,
      _0: s
    },
    tl: scope.items
  };
  
}

function scope(scope$1, f) {
  var sub_scope = {
    items: /* [] */0
  };
  Curry._1(f, sub_scope);
  scope$1.items = {
    hd: {
      TAG: /* Scope */1,
      _0: sub_scope
    },
    tl: scope$1.items
  };
  
}

function indentation_prefix(n) {
  switch (n) {
    case 0 :
        return "";
    case 1 :
        return "  ";
    case 2 :
        return "    ";
    case 3 :
        return "      ";
    case 4 :
        return "        ";
    case 5 :
        return "          ";
    case 6 :
        return "            ";
    case 7 :
        return "              ";
    case 8 :
        return "                ";
    default:
      return " ".repeat(n);
  }
}

function print(scope) {
  var loop = function (_acc, i, _param) {
    while(true) {
      var param = _param;
      var acc = _acc;
      if (!param) {
        return acc;
      }
      var s = param.hd;
      if (s.TAG === /* Line */0) {
        _param = param.tl;
        _acc = {
          hd: indentation_prefix(i) + s._0,
          tl: acc
        };
        continue ;
      }
      var items = s._0.items;
      var sub = loop(/* [] */0, i + 1 | 0, items);
      _param = param.tl;
      _acc = Pervasives.$at(sub, acc);
      continue ;
    };
  };
  return $$String.concat("\n", loop(/* [] */0, 0, scope.items));
}

function runtime_function(param) {
  switch (param[0]) {
    case "Decode" :
        var match = param[1];
        if (typeof match === "number") {
          switch (match) {
            case /* Pk_bits32 */0 :
                switch (param[2]) {
                  case /* Bt_float */1 :
                      return "Pbrt.Decoder.float_as_bits32";
                  case /* Bt_int */2 :
                      return "Pbrt.Decoder.int_as_bits32";
                  case /* Bt_int32 */3 :
                      return "Pbrt.Decoder.int32_as_bits32";
                  case /* Bt_string */0 :
                  case /* Bt_int64 */4 :
                  case /* Bt_bytes */5 :
                  case /* Bt_bool */6 :
                      throw {
                            RE_EXN_ID: "Failure",
                            _1: "Invalid encoding/OCaml type combination",
                            Error: new Error()
                          };
                  
                }
            case /* Pk_bits64 */1 :
                switch (param[2]) {
                  case /* Bt_float */1 :
                      return "Pbrt.Decoder.float_as_bits64";
                  case /* Bt_int */2 :
                      return "Pbrt.Decoder.int_as_bits64";
                  case /* Bt_int64 */4 :
                      return "Pbrt.Decoder.int64_as_bits64";
                  case /* Bt_string */0 :
                  case /* Bt_int32 */3 :
                  case /* Bt_bytes */5 :
                  case /* Bt_bool */6 :
                      throw {
                            RE_EXN_ID: "Failure",
                            _1: "Invalid encoding/OCaml type combination",
                            Error: new Error()
                          };
                  
                }
            case /* Pk_bytes */2 :
                var match$1 = param[2];
                if (match$1 === 5) {
                  return "Pbrt.Decoder.bytes";
                }
                if (match$1 === 0) {
                  return "Pbrt.Decoder.string";
                }
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            
          }
        } else if (match._0) {
          switch (param[2]) {
            case /* Bt_int */2 :
                return "Pbrt.Decoder.int_as_zigzag";
            case /* Bt_int32 */3 :
                return "Pbrt.Decoder.int32_as_zigzag";
            case /* Bt_int64 */4 :
                return "Pbrt.Decoder.int64_as_zigzag";
            case /* Bt_string */0 :
            case /* Bt_float */1 :
            case /* Bt_bytes */5 :
            case /* Bt_bool */6 :
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            
          }
        } else {
          switch (param[2]) {
            case /* Bt_int */2 :
                return "Pbrt.Decoder.int_as_varint";
            case /* Bt_int32 */3 :
                return "Pbrt.Decoder.int32_as_varint";
            case /* Bt_int64 */4 :
                return "Pbrt.Decoder.int64_as_varint";
            case /* Bt_string */0 :
            case /* Bt_float */1 :
            case /* Bt_bytes */5 :
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            case /* Bt_bool */6 :
                return "Pbrt.Decoder.bool";
            
          }
        }
    case "Encode" :
        var match$2 = param[1];
        if (typeof match$2 === "number") {
          switch (match$2) {
            case /* Pk_bits32 */0 :
                switch (param[2]) {
                  case /* Bt_float */1 :
                      return "Pbrt.Encoder.float_as_bits32";
                  case /* Bt_int */2 :
                      return "Pbrt.Encoder.int_as_bits32";
                  case /* Bt_int32 */3 :
                      return "Pbrt.Encoder.int32_as_bits32";
                  case /* Bt_string */0 :
                  case /* Bt_int64 */4 :
                  case /* Bt_bytes */5 :
                  case /* Bt_bool */6 :
                      throw {
                            RE_EXN_ID: "Failure",
                            _1: "Invalid encoding/OCaml type combination",
                            Error: new Error()
                          };
                  
                }
            case /* Pk_bits64 */1 :
                switch (param[2]) {
                  case /* Bt_float */1 :
                      return "Pbrt.Encoder.float_as_bits64";
                  case /* Bt_int */2 :
                      return "Pbrt.Encoder.int_as_bits64";
                  case /* Bt_int64 */4 :
                      return "Pbrt.Encoder.int64_as_bits64";
                  case /* Bt_string */0 :
                  case /* Bt_int32 */3 :
                  case /* Bt_bytes */5 :
                  case /* Bt_bool */6 :
                      throw {
                            RE_EXN_ID: "Failure",
                            _1: "Invalid encoding/OCaml type combination",
                            Error: new Error()
                          };
                  
                }
            case /* Pk_bytes */2 :
                var match$3 = param[2];
                if (match$3 === 5) {
                  return "Pbrt.Encoder.bytes";
                }
                if (match$3 === 0) {
                  return "Pbrt.Encoder.string";
                }
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            
          }
        } else if (match$2._0) {
          switch (param[2]) {
            case /* Bt_int */2 :
                return "Pbrt.Encoder.int_as_zigzag";
            case /* Bt_int32 */3 :
                return "Pbrt.Encoder.int32_as_zigzag";
            case /* Bt_int64 */4 :
                return "Pbrt.Encoder.int64_as_zigzag";
            case /* Bt_string */0 :
            case /* Bt_float */1 :
            case /* Bt_bytes */5 :
            case /* Bt_bool */6 :
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            
          }
        } else {
          switch (param[2]) {
            case /* Bt_int */2 :
                return "Pbrt.Encoder.int_as_varint";
            case /* Bt_int32 */3 :
                return "Pbrt.Encoder.int32_as_varint";
            case /* Bt_int64 */4 :
                return "Pbrt.Encoder.int64_as_varint";
            case /* Bt_string */0 :
            case /* Bt_float */1 :
            case /* Bt_bytes */5 :
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Invalid encoding/OCaml type combination",
                      Error: new Error()
                    };
            case /* Bt_bool */6 :
                return "Pbrt.Encoder.bool";
            
          }
        }
    default:
      throw {
            RE_EXN_ID: "Failure",
            _1: "Invalid encoding/OCaml type combination",
            Error: new Error()
          };
  }
}

function decode_basic_type(bt, pk) {
  return runtime_function([
              "Decode",
              pk,
              bt
            ]);
}

function decode_field_f(field_type, pk) {
  if (typeof field_type === "number") {
    return "Pbrt.Decoder.empty_nested d";
  }
  if (field_type.TAG === /* Ft_basic_type */0) {
    return decode_basic_type(field_type._0, pk) + " d";
  }
  var t = field_type._0;
  var f_name = function_name_of_user_defined("decode", t);
  if (t.udt_nested) {
    return f_name + " (Pbrt.Decoder.nested d)";
  } else {
    return f_name + " d";
  }
}

function gen_decode_record(and_, param, sc) {
  var r_fields = param.r_fields;
  var r_name = param.r_name;
  var all_lists = List.fold_left((function (acc, param) {
          var rf_field_type = param.rf_field_type;
          switch (rf_field_type.TAG | 0) {
            case /* Rft_repeated_field */2 :
            case /* Rft_associative_field */3 :
                break;
            default:
              return acc;
          }
          if (rf_field_type._0[0]) {
            return acc;
          } else {
            return {
                    hd: param.rf_label,
                    tl: acc
                  };
          }
        }), /* [] */0, r_fields);
  var process_field_common = function (sc, encoding_number, pk_as_string, f) {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "| Some (",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: ", Pbrt.",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") -> (",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "| Some (%i, Pbrt.%s) -> ("
                }), encoding_number, pk_as_string));
    scope(sc, (function (sc) {
            Curry._1(f, sc);
            return line$1(sc, "loop ()");
          }));
    line$1(sc, ")");
    line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "| Some (",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_i */3,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: ", pk) -> raise (",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "| Some (%i, pk) -> raise ("
                }), encoding_number));
    scope(sc, (function (sc) {
            return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                _0: {
                                  TAG: /* String_literal */11,
                                  _0: "Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload (",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* String_literal */11,
                                      _0: ", pk))",
                                      _1: /* End_of_format */0
                                    }
                                  }
                                },
                                _1: "Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload (%s, pk))"
                              }), Curry._2(Printf.sprintf(/* Format */{
                                    _0: {
                                      TAG: /* String_literal */11,
                                      _0: "\"Message(",
                                      _1: {
                                        TAG: /* String */2,
                                        _0: /* No_padding */0,
                                        _1: {
                                          TAG: /* String_literal */11,
                                          _0: "), field(",
                                          _1: {
                                            TAG: /* Int */4,
                                            _0: /* Int_i */3,
                                            _1: /* No_padding */0,
                                            _2: /* No_precision */0,
                                            _3: {
                                              TAG: /* String_literal */11,
                                              _0: ")\"",
                                              _1: /* End_of_format */0
                                            }
                                          }
                                        }
                                      }
                                    },
                                    _1: "\"Message(%s), field(%i)\""
                                  }), r_name, encoding_number)));
          }));
    return line$1(sc, ")");
  };
  var mutable_record_name = r_name + "_mutable";
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " decode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " d =",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s decode_%s d ="
              }), let_decl_of_and(and_), r_name));
  return scope(sc, (function (sc) {
                line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* String_literal */11,
                                _0: "let v = default_",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: " () in",
                                    _1: /* End_of_format */0
                                  }
                                }
                              },
                              _1: "let v = default_%s () in"
                            }), mutable_record_name));
                line$1(sc, "let rec loop () = ");
                scope(sc, (function (sc) {
                        line$1(sc, "match Pbrt.Decoder.key d with");
                        line$1(sc, "| None -> (");
                        scope(sc, (function (sc) {
                                return List.iter((function (field_name) {
                                              return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                  _0: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: "v.",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* String_literal */11,
                                                                        _0: " <- List.rev v.",
                                                                        _1: {
                                                                          TAG: /* String */2,
                                                                          _0: /* No_padding */0,
                                                                          _1: {
                                                                            TAG: /* Char_literal */12,
                                                                            _0: /* ';' */59,
                                                                            _1: /* End_of_format */0
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  },
                                                                  _1: "v.%s <- List.rev v.%s;"
                                                                }), field_name, field_name));
                                            }), all_lists);
                              }));
                        line$1(sc, ")");
                        List.iter((function (param) {
                                var rf_field_type = param.rf_field_type;
                                var rf_label = param.rf_label;
                                switch (rf_field_type.TAG | 0) {
                                  case /* Rft_required */0 :
                                      var param$1 = rf_field_type._0;
                                      var pk = param$1[2];
                                      var field_type = param$1[0];
                                      return process_field_common(sc, param$1[1], string_of_payload_kind(Caml_option.some(undefined), pk, false), (function (sc) {
                                                    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                        _0: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: "v.",
                                                                          _1: {
                                                                            TAG: /* String */2,
                                                                            _0: /* No_padding */0,
                                                                            _1: {
                                                                              TAG: /* String_literal */11,
                                                                              _0: " <- ",
                                                                              _1: {
                                                                                TAG: /* String */2,
                                                                                _0: /* No_padding */0,
                                                                                _1: {
                                                                                  TAG: /* Char_literal */12,
                                                                                  _0: /* ';' */59,
                                                                                  _1: /* End_of_format */0
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        },
                                                                        _1: "v.%s <- %s;"
                                                                      }), rf_label, decode_field_f(field_type, pk)));
                                                  }));
                                  case /* Rft_optional */1 :
                                      var param$2 = rf_field_type._0;
                                      var pk$1 = param$2[2];
                                      var field_type$1 = param$2[0];
                                      return process_field_common(sc, param$2[1], string_of_payload_kind(Caml_option.some(undefined), pk$1, false), (function (sc) {
                                                    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                        _0: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: "v.",
                                                                          _1: {
                                                                            TAG: /* String */2,
                                                                            _0: /* No_padding */0,
                                                                            _1: {
                                                                              TAG: /* String_literal */11,
                                                                              _0: " <- Some (",
                                                                              _1: {
                                                                                TAG: /* String */2,
                                                                                _0: /* No_padding */0,
                                                                                _1: {
                                                                                  TAG: /* String_literal */11,
                                                                                  _0: ");",
                                                                                  _1: /* End_of_format */0
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        },
                                                                        _1: "v.%s <- Some (%s);"
                                                                      }), rf_label, decode_field_f(field_type$1, pk$1)));
                                                  }));
                                  case /* Rft_repeated_field */2 :
                                      var param$3 = rf_field_type._0;
                                      var is_packed = param$3[4];
                                      var pk$2 = param$3[3];
                                      var encoding_number = param$3[2];
                                      var field_type$2 = param$3[1];
                                      if (param$3[0]) {
                                        if (is_packed) {
                                          return process_field_common(sc, encoding_number, "Bytes", (function (sc) {
                                                        line$1(sc, "Pbrt.Decoder.packed_fold (fun () d -> ");
                                                        scope(sc, (function (sc) {
                                                                return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                                    _0: {
                                                                                      TAG: /* String_literal */11,
                                                                                      _0: "Pbrt.Repeated_field.add (",
                                                                                      _1: {
                                                                                        TAG: /* String */2,
                                                                                        _0: /* No_padding */0,
                                                                                        _1: {
                                                                                          TAG: /* String_literal */11,
                                                                                          _0: ") v.",
                                                                                          _1: {
                                                                                            TAG: /* String */2,
                                                                                            _0: /* No_padding */0,
                                                                                            _1: {
                                                                                              TAG: /* Char_literal */12,
                                                                                              _0: /* ';' */59,
                                                                                              _1: /* End_of_format */0
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    },
                                                                                    _1: "Pbrt.Repeated_field.add (%s) v.%s;"
                                                                                  }), decode_field_f(field_type$2, pk$2), rf_label));
                                                              }));
                                                        return line$1(sc, ") () d;");
                                                      }));
                                        } else {
                                          return process_field_common(sc, encoding_number, string_of_payload_kind(Caml_option.some(undefined), pk$2, false), (function (sc) {
                                                        return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                            _0: {
                                                                              TAG: /* String_literal */11,
                                                                              _0: "Pbrt.Repeated_field.add (",
                                                                              _1: {
                                                                                TAG: /* String */2,
                                                                                _0: /* No_padding */0,
                                                                                _1: {
                                                                                  TAG: /* String_literal */11,
                                                                                  _0: ") v.",
                                                                                  _1: {
                                                                                    TAG: /* String */2,
                                                                                    _0: /* No_padding */0,
                                                                                    _1: {
                                                                                      TAG: /* String_literal */11,
                                                                                      _0: "; ",
                                                                                      _1: /* End_of_format */0
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            },
                                                                            _1: "Pbrt.Repeated_field.add (%s) v.%s; "
                                                                          }), decode_field_f(field_type$2, pk$2), rf_label));
                                                      }));
                                        }
                                      } else if (is_packed) {
                                        return process_field_common(sc, encoding_number, "Bytes", (function (sc) {
                                                      return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                          _0: {
                                                                            TAG: /* String_literal */11,
                                                                            _0: "v.",
                                                                            _1: {
                                                                              TAG: /* String */2,
                                                                              _0: /* No_padding */0,
                                                                              _1: {
                                                                                TAG: /* String_literal */11,
                                                                                _0: " <- Pbrt.Decoder.packed_fold (fun l d -> (",
                                                                                _1: {
                                                                                  TAG: /* String */2,
                                                                                  _0: /* No_padding */0,
                                                                                  _1: {
                                                                                    TAG: /* String_literal */11,
                                                                                    _0: ")::l) [] d;",
                                                                                    _1: /* End_of_format */0
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          },
                                                                          _1: "v.%s <- Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d;"
                                                                        }), rf_label, decode_field_f(field_type$2, pk$2)));
                                                    }));
                                      } else {
                                        return process_field_common(sc, encoding_number, string_of_payload_kind(Caml_option.some(undefined), pk$2, false), (function (sc) {
                                                      return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                                          _0: {
                                                                            TAG: /* String_literal */11,
                                                                            _0: "v.",
                                                                            _1: {
                                                                              TAG: /* String */2,
                                                                              _0: /* No_padding */0,
                                                                              _1: {
                                                                                TAG: /* String_literal */11,
                                                                                _0: " <- (",
                                                                                _1: {
                                                                                  TAG: /* String */2,
                                                                                  _0: /* No_padding */0,
                                                                                  _1: {
                                                                                    TAG: /* String_literal */11,
                                                                                    _0: ") :: v.",
                                                                                    _1: {
                                                                                      TAG: /* String */2,
                                                                                      _0: /* No_padding */0,
                                                                                      _1: {
                                                                                        TAG: /* Char_literal */12,
                                                                                        _0: /* ';' */59,
                                                                                        _1: /* End_of_format */0
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          },
                                                                          _1: "v.%s <- (%s) :: v.%s;"
                                                                        }), rf_label, decode_field_f(field_type$2, pk$2), rf_label));
                                                    }));
                                      }
                                  case /* Rft_associative_field */3 :
                                      var param$4 = rf_field_type._0;
                                      var match = param$4[3];
                                      var value_pk = match[1];
                                      var value_type = match[0];
                                      var match$1 = param$4[2];
                                      var at = param$4[0];
                                      var decode_key_f = decode_basic_type(match$1[0], match$1[1]);
                                      return process_field_common(sc, param$4[1], "Bytes", (function (sc) {
                                                    line$1(sc, "let decode_value = (fun d ->");
                                                    scope(sc, (function (sc) {
                                                            return line$1(sc, decode_field_f(value_type, value_pk));
                                                          }));
                                                    line$1(sc, ") in");
                                                    var decode_expression = Curry._1(Printf.sprintf(/* Format */{
                                                              _0: {
                                                                TAG: /* String_literal */11,
                                                                _0: "(Pbrt.Decoder.map_entry d ~decode_key:",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: " ~decode_value)",
                                                                    _1: /* End_of_format */0
                                                                  }
                                                                }
                                                              },
                                                              _1: "(Pbrt.Decoder.map_entry d ~decode_key:%s ~decode_value)"
                                                            }), decode_key_f);
                                                    if (at) {
                                                      line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                                    _0: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: "let a, b = ",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: " in",
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    },
                                                                    _1: "let a, b = %s in"
                                                                  }), decode_expression));
                                                      return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                                          _0: {
                                                                            TAG: /* String_literal */11,
                                                                            _0: "Hashtbl.add v.",
                                                                            _1: {
                                                                              TAG: /* String */2,
                                                                              _0: /* No_padding */0,
                                                                              _1: {
                                                                                TAG: /* String_literal */11,
                                                                                _0: " a b;",
                                                                                _1: /* End_of_format */0
                                                                              }
                                                                            }
                                                                          },
                                                                          _1: "Hashtbl.add v.%s a b;"
                                                                        }), rf_label));
                                                    } else {
                                                      line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                                    _0: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: "v.",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: " <- (",
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    },
                                                                    _1: "v.%s <- ("
                                                                  }), rf_label));
                                                      scope(sc, (function (sc) {
                                                              return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                                  _0: {
                                                                                    TAG: /* String */2,
                                                                                    _0: /* No_padding */0,
                                                                                    _1: {
                                                                                      TAG: /* String_literal */11,
                                                                                      _0: "::v.",
                                                                                      _1: {
                                                                                        TAG: /* String */2,
                                                                                        _0: /* No_padding */0,
                                                                                        _1: {
                                                                                          TAG: /* Char_literal */12,
                                                                                          _0: /* ';' */59,
                                                                                          _1: /* End_of_format */0
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  },
                                                                                  _1: "%s::v.%s;"
                                                                                }), decode_expression, rf_label));
                                                            }));
                                                      return line$1(sc, ");");
                                                    }
                                                  }));
                                  case /* Rft_variant_field */4 :
                                      var param$5 = rf_field_type._0;
                                      return List.iter((function (param) {
                                                    var pk = param.vc_payload_kind;
                                                    var vc_field_type = param.vc_field_type;
                                                    var vc_constructor = param.vc_constructor;
                                                    return process_field_common(sc, param.vc_encoding_number, string_of_payload_kind(Caml_option.some(undefined), pk, false), (function (sc) {
                                                                  if (vc_field_type) {
                                                                    return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                                                        _0: {
                                                                                          TAG: /* String_literal */11,
                                                                                          _0: "v.",
                                                                                          _1: {
                                                                                            TAG: /* String */2,
                                                                                            _0: /* No_padding */0,
                                                                                            _1: {
                                                                                              TAG: /* String_literal */11,
                                                                                              _0: " <- ",
                                                                                              _1: {
                                                                                                TAG: /* String */2,
                                                                                                _0: /* No_padding */0,
                                                                                                _1: {
                                                                                                  TAG: /* String_literal */11,
                                                                                                  _0: " (",
                                                                                                  _1: {
                                                                                                    TAG: /* String */2,
                                                                                                    _0: /* No_padding */0,
                                                                                                    _1: {
                                                                                                      TAG: /* String_literal */11,
                                                                                                      _0: ");",
                                                                                                      _1: /* End_of_format */0
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        },
                                                                                        _1: "v.%s <- %s (%s);"
                                                                                      }), rf_label, vc_constructor, decode_field_f(vc_field_type._0, pk)));
                                                                  } else {
                                                                    line$1(sc, "Pbrt.Decoder.empty_nested d;");
                                                                    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                                                        _0: {
                                                                                          TAG: /* String_literal */11,
                                                                                          _0: "v.",
                                                                                          _1: {
                                                                                            TAG: /* String */2,
                                                                                            _0: /* No_padding */0,
                                                                                            _1: {
                                                                                              TAG: /* String_literal */11,
                                                                                              _0: " <- ",
                                                                                              _1: {
                                                                                                TAG: /* String */2,
                                                                                                _0: /* No_padding */0,
                                                                                                _1: {
                                                                                                  TAG: /* Char_literal */12,
                                                                                                  _0: /* ';' */59,
                                                                                                  _1: /* End_of_format */0
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        },
                                                                                        _1: "v.%s <- %s;"
                                                                                      }), rf_label, vc_constructor));
                                                                  }
                                                                }));
                                                  }), param$5.v_constructors);
                                  
                                }
                              }), r_fields);
                        return line$1(sc, "| Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()");
                      }));
                line$1(sc, "in");
                line$1(sc, "loop ();");
                line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* String_literal */11,
                                _0: "let v:",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: " = Obj.magic v in",
                                    _1: /* End_of_format */0
                                  }
                                }
                              },
                              _1: "let v:%s = Obj.magic v in"
                            }), r_name));
                return line$1(sc, "v");
              }));
}

function gen_decode_variant(and_, param, sc) {
  var v_constructors = param.v_constructors;
  var v_name = param.v_name;
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " decode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " d = ",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s decode_%s d = "
              }), let_decl_of_and(and_), v_name));
  return scope(sc, (function (sc) {
                line$1(sc, Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "let rec loop () = ",
                            _1: /* End_of_format */0
                          },
                          _1: "let rec loop () = "
                        }));
                scope(sc, (function (sc) {
                        line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                      _0: {
                                        TAG: /* String_literal */11,
                                        _0: "let ret:",
                                        _1: {
                                          TAG: /* String */2,
                                          _0: /* No_padding */0,
                                          _1: {
                                            TAG: /* String_literal */11,
                                            _0: " = match Pbrt.Decoder.key d with",
                                            _1: /* End_of_format */0
                                          }
                                        }
                                      },
                                      _1: "let ret:%s = match Pbrt.Decoder.key d with"
                                    }), v_name));
                        scope(sc, (function (sc) {
                                line$1(sc, "| None -> failwith \"None of the known key is found\"");
                                List.iter((function (ctor) {
                                        var vc_encoding_number = ctor.vc_encoding_number;
                                        var vc_field_type = ctor.vc_field_type;
                                        var vc_constructor = ctor.vc_constructor;
                                        if (vc_field_type) {
                                          return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                              _0: {
                                                                TAG: /* String_literal */11,
                                                                _0: "| Some (",
                                                                _1: {
                                                                  TAG: /* Int */4,
                                                                  _0: /* Int_i */3,
                                                                  _1: /* No_padding */0,
                                                                  _2: /* No_precision */0,
                                                                  _3: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: ", _) -> ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* String_literal */11,
                                                                        _0: " (",
                                                                        _1: {
                                                                          TAG: /* String */2,
                                                                          _0: /* No_padding */0,
                                                                          _1: {
                                                                            TAG: /* Char_literal */12,
                                                                            _0: /* ')' */41,
                                                                            _1: /* End_of_format */0
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              },
                                                              _1: "| Some (%i, _) -> %s (%s)"
                                                            }), vc_encoding_number, vc_constructor, decode_field_f(vc_field_type._0, ctor.vc_payload_kind)));
                                        } else {
                                          return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                              _0: {
                                                                TAG: /* String_literal */11,
                                                                _0: "| Some (",
                                                                _1: {
                                                                  TAG: /* Int */4,
                                                                  _0: /* Int_i */3,
                                                                  _1: /* No_padding */0,
                                                                  _2: /* No_precision */0,
                                                                  _3: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: ", _) -> (Pbrt.Decoder.empty_nested d ; ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* ')' */41,
                                                                        _1: /* End_of_format */0
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              },
                                                              _1: "| Some (%i, _) -> (Pbrt.Decoder.empty_nested d ; %s)"
                                                            }), vc_encoding_number, vc_constructor));
                                        }
                                      }), v_constructors);
                                line$1(sc, "| Some (n, payload_kind) -> (");
                                line$1(sc, "  Pbrt.Decoder.skip d payload_kind; ");
                                line$1(sc, "  loop () ");
                                return line$1(sc, ")");
                              }));
                        line$1(sc, "in");
                        return line$1(sc, "ret");
                      }));
                line$1(sc, "in");
                return line$1(sc, "loop ()");
              }));
}

function gen_decode_const_variant(and_, param, sc) {
  var cv_constructors = param.cv_constructors;
  var cv_name = param.cv_name;
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " decode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " d = ",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s decode_%s d = "
              }), let_decl_of_and(and_), cv_name));
  return scope(sc, (function (sc) {
                line$1(sc, "match Pbrt.Decoder.int_as_varint d with");
                List.iter((function (param) {
                        return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                            _0: {
                                              TAG: /* String_literal */11,
                                              _0: "| ",
                                              _1: {
                                                TAG: /* Int */4,
                                                _0: /* Int_i */3,
                                                _1: /* No_padding */0,
                                                _2: /* No_precision */0,
                                                _3: {
                                                  TAG: /* String_literal */11,
                                                  _0: " -> (",
                                                  _1: {
                                                    TAG: /* String */2,
                                                    _0: /* No_padding */0,
                                                    _1: {
                                                      TAG: /* Char_literal */12,
                                                      _0: /* ':' */58,
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* Char_literal */12,
                                                          _0: /* ')' */41,
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            },
                                            _1: "| %i -> (%s:%s)"
                                          }), param[1], param[0], cv_name));
                      }), cv_constructors);
                return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                    _0: {
                                      TAG: /* String_literal */11,
                                      _0: "| _ -> failwith \"Unknown value for enum ",
                                      _1: {
                                        TAG: /* String */2,
                                        _0: /* No_padding */0,
                                        _1: {
                                          TAG: /* Char_literal */12,
                                          _0: /* '"' */34,
                                          _1: /* End_of_format */0
                                        }
                                      }
                                    },
                                    _1: "| _ -> failwith \"Unknown value for enum %s\""
                                  }), cv_name));
              }));
}

function gen_struct(and_, t, sc) {
  var r = t.spec;
  var tmp;
  switch (r.TAG | 0) {
    case /* Record */0 :
        tmp = [
          gen_decode_record(and_, r._0, sc),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          gen_decode_variant(and_, r._0, sc),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          gen_decode_const_variant(and_, r._0, sc),
          true
        ];
        break;
    
  }
  return tmp[1];
}

function gen_sig(and_, t, sc) {
  var f = function (type_name) {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "val decode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " : Pbrt.Decoder.t -> ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "val decode_%s : Pbrt.Decoder.t -> %s"
                }), type_name, type_name));
    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "(** [decode_",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " decoder] decodes a [",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: "] value from [decoder] *)",
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)"
                      }), type_name, type_name));
  };
  var match = t.spec;
  var tmp;
  switch (match.TAG | 0) {
    case /* Record */0 :
        tmp = [
          f(match._0.r_name),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          f(match._0.v_name),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          f(match._0.cv_name),
          true
        ];
        break;
    
  }
  return tmp[1];
}

var Codegen_decode = {
  gen_sig: gen_sig,
  gen_struct: gen_struct,
  ocamldoc_title: "Protobuf Decoding"
};

var __log__ = {
  contents: undefined
};

function log(x) {
  var oc = __log__.contents;
  if (oc !== undefined) {
    return Printf.fprintf(Caml_option.valFromOption(oc), x);
  } else {
    return Printf.ifprintf(Pervasives.stdout, x);
  }
}

function endline(s) {
  return Curry._1(log(/* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* '\n' */10,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "%s\n"
                }), s);
}

function gen_pp_field(field_type) {
  if (typeof field_type !== "number" && field_type.TAG !== /* Ft_basic_type */0) {
    return function_name_of_user_defined("pp", field_type._0);
  }
  return Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "Pbrt.Pp.pp_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: /* End_of_format */0
                    }
                  },
                  _1: "Pbrt.Pp.pp_%s"
                }), string_of_field_type(field_type));
}

function gen_pp_record(and_, param, sc) {
  var r_fields = param.r_fields;
  var r_name = param.r_name;
  Curry._1(log(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "gen_pp, record_name: ",
              _1: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* '\n' */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "gen_pp, record_name: %s\n"
          }), r_name);
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " pp_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " fmt (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") = ",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s pp_%s fmt (v:%s) = "
              }), let_decl_of_and(and_), r_name, r_name));
  return scope(sc, (function (sc) {
                line$1(sc, "let pp_i fmt () =");
                scope(sc, (function (sc) {
                        line$1(sc, "Format.pp_open_vbox fmt 1;");
                        List.iter((function (record_field) {
                                var rf_field_type = record_field.rf_field_type;
                                var rf_label = record_field.rf_label;
                                var var_name = Curry._1(Printf.sprintf(/* Format */{
                                          _0: {
                                            TAG: /* String_literal */11,
                                            _0: "v.",
                                            _1: {
                                              TAG: /* String */2,
                                              _0: /* No_padding */0,
                                              _1: /* End_of_format */0
                                            }
                                          },
                                          _1: "v.%s"
                                        }), rf_label);
                                switch (rf_field_type.TAG | 0) {
                                  case /* Rft_required */0 :
                                      var field_string_of = gen_pp_field(rf_field_type._0[0]);
                                      return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* String_literal */11,
                                                            _0: "Pbrt.Pp.pp_record_field \"",
                                                            _1: {
                                                              TAG: /* String */2,
                                                              _0: /* No_padding */0,
                                                              _1: {
                                                                TAG: /* String_literal */11,
                                                                _0: "\" ",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: " fmt ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* ';' */59,
                                                                        _1: /* End_of_format */0
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          },
                                                          _1: "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;"
                                                        }), rf_label, field_string_of, var_name));
                                  case /* Rft_optional */1 :
                                      var field_string_of$1 = gen_pp_field(rf_field_type._0[0]);
                                      return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* String_literal */11,
                                                            _0: "Pbrt.Pp.pp_record_field \"",
                                                            _1: {
                                                              TAG: /* String */2,
                                                              _0: /* No_padding */0,
                                                              _1: {
                                                                TAG: /* String_literal */11,
                                                                _0: "\" (Pbrt.Pp.pp_option ",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: ") fmt ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* ';' */59,
                                                                        _1: /* End_of_format */0
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          },
                                                          _1: "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_option %s) fmt %s;"
                                                        }), rf_label, field_string_of$1, var_name));
                                  case /* Rft_repeated_field */2 :
                                      var match = rf_field_type._0;
                                      var field_string_of$2 = gen_pp_field(match[1]);
                                      if (match[0]) {
                                        return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                            _0: {
                                                              TAG: /* String_literal */11,
                                                              _0: "Pbrt.Pp.pp_record_field \"",
                                                              _1: {
                                                                TAG: /* String */2,
                                                                _0: /* No_padding */0,
                                                                _1: {
                                                                  TAG: /* String_literal */11,
                                                                  _0: "\" (Pbrt.Pp.pp_list ",
                                                                  _1: {
                                                                    TAG: /* String */2,
                                                                    _0: /* No_padding */0,
                                                                    _1: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: ") fmt (Pbrt.Repeated_field.to_list ",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: ");",
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            },
                                                            _1: "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt (Pbrt.Repeated_field.to_list %s);"
                                                          }), rf_label, field_string_of$2, var_name));
                                      } else {
                                        return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                            _0: {
                                                              TAG: /* String_literal */11,
                                                              _0: "Pbrt.Pp.pp_record_field \"",
                                                              _1: {
                                                                TAG: /* String */2,
                                                                _0: /* No_padding */0,
                                                                _1: {
                                                                  TAG: /* String_literal */11,
                                                                  _0: "\" (Pbrt.Pp.pp_list ",
                                                                  _1: {
                                                                    TAG: /* String */2,
                                                                    _0: /* No_padding */0,
                                                                    _1: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: ") fmt ",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* Char_literal */12,
                                                                          _0: /* ';' */59,
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            },
                                                            _1: "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.pp_list %s) fmt %s;"
                                                          }), rf_label, field_string_of$2, var_name));
                                      }
                                  case /* Rft_associative_field */3 :
                                      var match$1 = rf_field_type._0;
                                      var pp_runtime_function = match$1[0] ? "pp_hastable" : "pp_associative_list";
                                      var pp_key = gen_pp_field({
                                            TAG: /* Ft_basic_type */0,
                                            _0: match$1[2][0]
                                          });
                                      var pp_value = gen_pp_field(match$1[3][0]);
                                      return line$1(sc, Curry._5(Printf.sprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* String_literal */11,
                                                            _0: "Pbrt.Pp.pp_record_field \"",
                                                            _1: {
                                                              TAG: /* String */2,
                                                              _0: /* No_padding */0,
                                                              _1: {
                                                                TAG: /* String_literal */11,
                                                                _0: "\" (Pbrt.Pp.",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* Char_literal */12,
                                                                    _0: /* ' ' */32,
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* ' ' */32,
                                                                        _1: {
                                                                          TAG: /* String */2,
                                                                          _0: /* No_padding */0,
                                                                          _1: {
                                                                            TAG: /* String_literal */11,
                                                                            _0: ") fmt ",
                                                                            _1: {
                                                                              TAG: /* String */2,
                                                                              _0: /* No_padding */0,
                                                                              _1: {
                                                                                TAG: /* Char_literal */12,
                                                                                _0: /* ';' */59,
                                                                                _1: /* End_of_format */0
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          },
                                                          _1: "Pbrt.Pp.pp_record_field \"%s\" (Pbrt.Pp.%s %s %s) fmt %s;"
                                                        }), rf_label, pp_runtime_function, pp_key, pp_value, var_name));
                                  case /* Rft_variant_field */4 :
                                      return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* String_literal */11,
                                                            _0: "Pbrt.Pp.pp_record_field \"",
                                                            _1: {
                                                              TAG: /* String */2,
                                                              _0: /* No_padding */0,
                                                              _1: {
                                                                TAG: /* String_literal */11,
                                                                _0: "\" ",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: " fmt ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* ';' */59,
                                                                        _1: /* End_of_format */0
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          },
                                                          _1: "Pbrt.Pp.pp_record_field \"%s\" %s fmt %s;"
                                                        }), rf_label, "pp_" + rf_field_type._0.v_name, var_name));
                                  
                                }
                              }), r_fields);
                        return line$1(sc, "Format.pp_close_box fmt ()");
                      }));
                line$1(sc, "in");
                return line$1(sc, "Pbrt.Pp.pp_brk pp_i fmt ()");
              }));
}

function gen_pp_variant(and_, param, sc) {
  var v_constructors = param.v_constructors;
  var v_name = param.v_name;
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " pp_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " fmt (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") =",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s pp_%s fmt (v:%s) ="
              }), let_decl_of_and(and_), v_name, v_name));
  return scope(sc, (function (sc) {
                line$1(sc, "match v with");
                return List.iter((function (param) {
                              var vc_field_type = param.vc_field_type;
                              var vc_constructor = param.vc_constructor;
                              if (!vc_field_type) {
                                return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "| ",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: "  -> Format.fprintf fmt \"",
                                                          _1: {
                                                            TAG: /* String */2,
                                                            _0: /* No_padding */0,
                                                            _1: {
                                                              TAG: /* Char_literal */12,
                                                              _0: /* '"' */34,
                                                              _1: /* End_of_format */0
                                                            }
                                                          }
                                                        }
                                                      }
                                                    },
                                                    _1: "| %s  -> Format.fprintf fmt \"%s\""
                                                  }), vc_constructor, vc_constructor));
                              }
                              var field_string_of = gen_pp_field(vc_field_type._0);
                              return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* String_literal */11,
                                                    _0: "| ",
                                                    _1: {
                                                      TAG: /* String */2,
                                                      _0: /* No_padding */0,
                                                      _1: {
                                                        TAG: /* String_literal */11,
                                                        _0: " x -> Format.fprintf fmt \"",
                                                        _1: {
                                                          TAG: /* Formatting_gen */18,
                                                          _0: {
                                                            TAG: /* Open_box */1,
                                                            _0: /* Format */{
                                                              _0: /* End_of_format */0,
                                                              _1: ""
                                                            }
                                                          },
                                                          _1: {
                                                            TAG: /* String */2,
                                                            _0: /* No_padding */0,
                                                            _1: {
                                                              TAG: /* Char_literal */12,
                                                              _0: /* '(' */40,
                                                              _1: {
                                                                TAG: /* Char_literal */12,
                                                                _0: /* '%' */37,
                                                                _1: {
                                                                  TAG: /* String_literal */11,
                                                                  _0: "a)",
                                                                  _1: {
                                                                    TAG: /* Formatting_lit */17,
                                                                    _0: /* Close_box */0,
                                                                    _1: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: "\" ",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: " x",
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  },
                                                  _1: "| %s x -> Format.fprintf fmt \"@[%s(%%a)@]\" %s x"
                                                }), vc_constructor, vc_constructor, field_string_of));
                            }), v_constructors);
              }));
}

function gen_pp_const_variant(and_, param, sc) {
  var cv_constructors = param.cv_constructors;
  var cv_name = param.cv_name;
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " pp_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " fmt (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") =",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s pp_%s fmt (v:%s) ="
              }), let_decl_of_and(and_), cv_name, cv_name));
  return scope(sc, (function (sc) {
                line$1(sc, "match v with");
                return List.iter((function (param) {
                              var name = param[0];
                              return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* String_literal */11,
                                                    _0: "| ",
                                                    _1: {
                                                      TAG: /* String */2,
                                                      _0: /* No_padding */0,
                                                      _1: {
                                                        TAG: /* String_literal */11,
                                                        _0: " -> Format.fprintf fmt \"",
                                                        _1: {
                                                          TAG: /* String */2,
                                                          _0: /* No_padding */0,
                                                          _1: {
                                                            TAG: /* Char_literal */12,
                                                            _0: /* '"' */34,
                                                            _1: /* End_of_format */0
                                                          }
                                                        }
                                                      }
                                                    }
                                                  },
                                                  _1: "| %s -> Format.fprintf fmt \"%s\""
                                                }), name, name));
                            }), cv_constructors);
              }));
}

function gen_struct$1(and_, t, sc) {
  var r = t.spec;
  switch (r.TAG | 0) {
    case /* Record */0 :
        gen_pp_record(and_, r._0, sc);
        break;
    case /* Variant */1 :
        gen_pp_variant(and_, r._0, sc);
        break;
    case /* Const_variant */2 :
        gen_pp_const_variant(and_, r._0, sc);
        break;
    
  }
  return true;
}

function gen_sig$1(and_, t, sc) {
  var f = function (type_name) {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "val pp_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " : Format.formatter -> ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: " -> unit ",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "val pp_%s : Format.formatter -> %s -> unit "
                }), type_name, type_name));
    return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "(** [pp_",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " v] formats v] *)",
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "(** [pp_%s v] formats v] *)"
                      }), type_name));
  };
  var v = t.spec;
  switch (v.TAG | 0) {
    case /* Record */0 :
        f(v._0.r_name);
        break;
    case /* Variant */1 :
        f(v._0.v_name);
        break;
    case /* Const_variant */2 :
        f(v._0.cv_name);
        break;
    
  }
  return true;
}

var Codegen_pp = {
  gen_sig: gen_sig$1,
  gen_struct: gen_struct$1,
  ocamldoc_title: "Formatters"
};

function height(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          l: l,
          v: x,
          d: d,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (lr) {
        return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add(x, data, m) {
  if (!m) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            d: data,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = Caml_obj.caml_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */{
              l: l,
              v: x,
              d: data,
              r: r,
              h: m.h
            };
    }
  }
  if (c < 0) {
    var ll = add(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal(ll, v, d, r);
    }
  }
  var rr = add(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal(l, v, d, rr);
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_compare(x, param.v);
      if (c === 0) {
        return param.d;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function map$1(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map$1(f, param.l);
  var d$prime = Curry._1(f, param.d);
  var r$prime = map$1(f, param.r);
  return /* Node */{
          l: l$prime,
          v: param.v,
          d: d$prime,
          r: r$prime,
          h: param.h
        };
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m.v, m.d, fold(f, m.l, accu));
    _m = m.r;
    continue ;
  };
}

function min_value(param) {
  var x = param[0];
  if (x !== undefined) {
    var y = param[1];
    if (y !== undefined) {
      return Caml_option.some(Caml_obj.caml_min(Caml_option.valFromOption(x), Caml_option.valFromOption(y)));
    }
    throw {
          RE_EXN_ID: "Failure",
          _1: "min_value error",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "min_value error",
        Error: new Error()
      };
}

function eq_value(param) {
  var x = param[0];
  if (x !== undefined) {
    var y = param[1];
    if (y !== undefined) {
      return Caml_obj.caml_equal(Caml_option.valFromOption(x), Caml_option.valFromOption(y));
    }
    throw {
          RE_EXN_ID: "Failure",
          _1: "eq_value error",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "eq_value error",
        Error: new Error()
      };
}

function string_of_option(f, x) {
  if (x !== undefined) {
    return Curry._1(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Some(",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* ')' */41,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "Some(%s)"
                  }), Curry._1(f, Caml_option.valFromOption(x)));
  } else {
    return "None";
  }
}

function reset(g) {
  return map$1((function (core) {
                return {
                        core: core,
                        index: undefined,
                        lowlink: undefined,
                        on_stack: false
                      };
              }), g);
}

function strong_connect(g, sccs, stack, index, v) {
  Curry._2(log(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[Graph] processing v [",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_i */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: "], index: ",
                  _1: {
                    TAG: /* Int */4,
                    _0: /* Int_i */3,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* '\n' */10,
                      _1: /* End_of_format */0
                    }
                  }
                }
              }
            },
            _1: "[Graph] processing v [%i], index: %i\n"
          }), v.core.id, index);
  v.index = index;
  v.lowlink = index;
  var stack$1 = {
    hd: v,
    tl: stack
  };
  v.on_stack = true;
  var match = List.fold_left((function (param, id) {
          var index = param[2];
          var stack = param[1];
          var sccs = param[0];
          var w = find(id, g);
          Curry._2(log(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "[Graph] sub w [",
                      _1: {
                        TAG: /* Int */4,
                        _0: /* Int_i */3,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* String_literal */11,
                          _0: "], w.index: ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* '\n' */10,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    },
                    _1: "[Graph] sub w [%i], w.index: %s\n"
                  }), w.core.id, string_of_option((function (prim) {
                      return String(prim);
                    }), w.index));
          var match = w.index;
          if (match !== undefined) {
            if (w.on_stack) {
              v.lowlink = min_value([
                    v.lowlink,
                    w.index
                  ]);
            }
            return [
                    sccs,
                    stack,
                    index
                  ];
          }
          var match$1 = strong_connect(g, sccs, stack, index + 1 | 0, w);
          v.lowlink = min_value([
                v.lowlink,
                w.lowlink
              ]);
          return [
                  match$1[0],
                  match$1[1],
                  match$1[2]
                ];
        }), [
        sccs,
        stack$1,
        index
      ], v.core.sub);
  var index$1 = match[2];
  var stack$2 = match[1];
  var sccs$1 = match[0];
  Curry._3(log(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[Graph] after sub for v [",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_i */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: "], lowlink: ",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: ", index: ",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* '\n' */10,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  }
                }
              }
            },
            _1: "[Graph] after sub for v [%i], lowlink: %s, index: %s\n"
          }), v.core.id, string_of_option((function (prim) {
              return String(prim);
            }), v.lowlink), string_of_option((function (prim) {
              return String(prim);
            }), v.index));
  Curry._1(log(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "[Graph]   -> stack : ",
              _1: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* '\n' */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "[Graph]   -> stack : %s\n"
          }), "[" + ($$String.concat(";", List.map((function (param) {
                    return String(param.core.id);
                  }), stack$2)) + "]"));
  if (!eq_value([
          v.lowlink,
          v.index
        ])) {
    return [
            sccs$1,
            stack$2,
            index$1
          ];
  }
  var match$1 = List.fold_left((function (param, n) {
          var splitted = param[2];
          var stack = param[1];
          var scc = param[0];
          if (splitted) {
            return [
                    scc,
                    {
                      hd: n,
                      tl: stack
                    },
                    splitted
                  ];
          } else {
            n.on_stack = false;
            if (n.core.id === v.core.id) {
              return [
                      {
                        hd: n.core.id,
                        tl: scc
                      },
                      stack,
                      true
                    ];
            } else {
              return [
                      {
                        hd: n.core.id,
                        tl: scc
                      },
                      stack,
                      false
                    ];
            }
          }
        }), [
        /* [] */0,
        /* [] */0,
        false
      ], stack$2);
  return [
          {
            hd: match$1[0],
            tl: sccs$1
          },
          List.rev(match$1[1]),
          index$1
        ];
}

function tarjan(g) {
  var g$1 = reset(g);
  return fold((function (param, n, param$1) {
                  var index = param$1[2];
                  var stack = param$1[1];
                  var sccs = param$1[0];
                  var match = n.index;
                  if (match !== undefined) {
                    return [
                            sccs,
                            stack,
                            index
                          ];
                  } else {
                    return strong_connect(g$1, sccs, stack, index, n);
                  }
                }), g$1, [
                /* [] */0,
                /* [] */0,
                0
              ])[0];
}

function field_name(param) {
  return param.field_parsed.field_name;
}

function field_number(param) {
  return param.field_parsed.field_number;
}

function field_type(param) {
  return param.field_type;
}

function field_label(param) {
  return param.field_parsed.field_label;
}

function field_default(param) {
  return param.field_default;
}

function field_options(param) {
  return param.field_options;
}

function find_field_option(field_options, option_name) {
  var x;
  try {
    x = List.assoc(option_name, field_options);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return ;
    }
    throw exn;
  }
  return Caml_option.some(x);
}

function field_option(param, option_name) {
  return find_field_option(param.field_options, option_name);
}

function type_id_of_type(param) {
  return param.id;
}

function type_of_id(all_types, id) {
  return List.find((function (t) {
                return type_id_of_type(t) === id;
              }), all_types);
}

function string_of_unresolved(param) {
  return Curry._3(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "unresolved:{scope ",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: ", type_name: ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ", from_root: ",
                            _1: {
                              TAG: /* Bool */9,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* '}' */125,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "unresolved:{scope %s, type_name: %s, from_root: %b}"
                }), string_of_string_list(param.scope), param.type_name, param.from_root);
}

function scope_of_package(s) {
  if (s !== undefined) {
    return {
            packages: List.rev(rev_split_by_char(/* '.' */46, s)),
            message_names: /* [] */0
          };
  } else {
    return {
            packages: /* [] */0,
            message_names: /* [] */0
          };
  }
}

function unresolved_of_string(s) {
  var match = rev_split_by_char(/* '.' */46, s);
  if (match) {
    return {
            scope: List.rev(match.tl),
            type_name: match.hd,
            from_root: Caml_string.get(s, 0) === /* '.' */46
          };
  }
  throw {
        RE_EXN_ID: Compilation_error,
        _1: {
          TAG: /* Programatic_error */4,
          _0: /* Invalid_string_split */0
        },
        Error: new Error()
      };
}

function field_type_of_string(s) {
  switch (s) {
    case "bool" :
        return /* Field_type_bool */12;
    case "bytes" :
        return /* Field_type_bytes */14;
    case "double" :
        return /* Field_type_double */0;
    case "fixed32" :
        return /* Field_type_fixed32 */8;
    case "fixed64" :
        return /* Field_type_fixed64 */9;
    case "float" :
        return /* Field_type_float */1;
    case "int32" :
        return /* Field_type_int32 */2;
    case "int64" :
        return /* Field_type_int64 */3;
    case "sfixed32" :
        return /* Field_type_sfixed32 */10;
    case "sfixed64" :
        return /* Field_type_sfixed64 */11;
    case "sint32" :
        return /* Field_type_sint32 */6;
    case "sint64" :
        return /* Field_type_sint64 */7;
    case "string" :
        return /* Field_type_string */13;
    case "uint32" :
        return /* Field_type_uint32 */4;
    case "uint64" :
        return /* Field_type_uint64 */5;
    default:
      return /* Field_type_type */{
              _0: unresolved_of_string(s)
            };
  }
}

function compile_default_p2(all_types, field) {
  var field_name$1 = field_name(field);
  var field_type$1 = field_type(field);
  var field_default$1 = field_default(field);
  if (field_default$1 === undefined) {
    return ;
  }
  var exit = 0;
  if (typeof field_type$1 === "number") {
    switch (field_type$1) {
      case /* Field_type_double */0 :
      case /* Field_type_float */1 :
          exit = 1;
          break;
      case /* Field_type_uint32 */4 :
      case /* Field_type_uint64 */5 :
          exit = 3;
          break;
      case /* Field_type_int32 */2 :
      case /* Field_type_int64 */3 :
      case /* Field_type_sint32 */6 :
      case /* Field_type_sint64 */7 :
      case /* Field_type_fixed32 */8 :
      case /* Field_type_fixed64 */9 :
      case /* Field_type_sfixed32 */10 :
      case /* Field_type_sfixed64 */11 :
          exit = 2;
          break;
      case /* Field_type_bool */12 :
          if (field_default$1.TAG === /* Constant_bool */1) {
            return field_default$1;
          } else {
            return invalid_default_value(field_name$1, "invalid default type (bool expected)", undefined);
          }
      case /* Field_type_string */13 :
          if (field_default$1.TAG === /* Constant_string */0) {
            return field_default$1;
          } else {
            return invalid_default_value(field_name$1, "invalid default type (string expected)", undefined);
          }
      case /* Field_type_bytes */14 :
          return invalid_default_value(field_name$1, "default value not supported for bytes", undefined);
      
    }
  } else {
    if (field_default$1.TAG !== /* Constant_litteral */4) {
      return invalid_default_value(field_name$1, "default value not supported for message", undefined);
    }
    var default_enum_value = field_default$1._0;
    var match = type_of_id(all_types, field_type$1._0);
    var spec = match.spec;
    if (spec.TAG !== /* Enum */0) {
      return invalid_default_value(field_name$1, "field of type message cannot have a default litteral value", undefined);
    }
    var default_enum_value$1 = apply_until((function (param) {
            var enum_value_name = param.enum_value_name;
            if (enum_value_name === default_enum_value) {
              return enum_value_name;
            }
            
          }), spec._0.enum_values);
    if (default_enum_value$1 !== undefined) {
      return field_default$1;
    } else {
      return invalid_default_value(field_name$1, "Invalid default enum value", undefined);
    }
  }
  switch (exit) {
    case 1 :
        switch (field_default$1.TAG | 0) {
          case /* Constant_int */2 :
              return {
                      TAG: /* Constant_float */3,
                      _0: field_default$1._0
                    };
          case /* Constant_float */3 :
              return field_default$1;
          default:
            return invalid_default_value(field_name$1, "invalid default type (float/int expected)", undefined);
        }
    case 2 :
        if (field_default$1.TAG === /* Constant_int */2) {
          return field_default$1;
        } else {
          return invalid_default_value(field_name$1, "invalid default type (int expected)", undefined);
        }
    case 3 :
        if (field_default$1.TAG === /* Constant_int */2) {
          if (field_default$1._0 >= 0) {
            return field_default$1;
          } else {
            return invalid_default_value(field_name$1, "negative default value for unsigned int", undefined);
          }
        } else {
          return invalid_default_value(field_name$1, "invalid default type (int expected)", undefined);
        }
    
  }
}

function get_default(field_name, field_options, field_type) {
  var constant;
  try {
    constant = List.assoc("default", field_options);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return ;
    }
    throw exn;
  }
  return Caml_option.some(constant);
}

function compile_field_p1(field_parsed) {
  var field_options = field_parsed.field_options;
  var field_type = field_type_of_string(field_parsed.field_type);
  var field_default = get_default(field_parsed.field_name, field_options, field_type);
  return {
          field_parsed: field_parsed,
          field_type: field_type,
          field_default: field_default,
          field_options: field_options
        };
}

function compile_map_p1(param) {
  return {
          map_name: param.map_name,
          map_number: param.map_number,
          map_key_type: field_type_of_string(param.map_key_type),
          map_value_type: field_type_of_string(param.map_value_type),
          map_options: param.map_options
        };
}

function compile_oneof_p1(param) {
  return {
          oneof_name: param.oneof_name,
          oneof_fields: List.map(compile_field_p1, param.oneof_fields)
        };
}

function not_found(f) {
  try {
    Curry._1(f, undefined);
    return false;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return true;
    }
    throw exn;
  }
}

function list_assoc2(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param.hd;
      if (Caml_obj.caml_equal(match[1], x)) {
        return match[0];
      }
      _param = param.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function type_of_spec(file_name, file_options, id, scope, spec) {
  return {
          scope: scope,
          id: id,
          file_name: file_name,
          file_options: file_options,
          spec: spec
        };
}

function compile_enum_p1(file_name, file_options, scope, param) {
  var enum_values = List.map((function (enum_value) {
          return {
                  enum_value_name: enum_value.enum_value_name,
                  enum_value_int: enum_value.enum_value_int
                };
        }), param.enum_values);
  return type_of_spec(file_name, file_options, param.enum_id, scope, {
              TAG: /* Enum */0,
              _0: {
                enum_name: param.enum_name,
                enum_values: enum_values
              }
            });
}

function compile_message_p1(file_name, file_options, message_scope, param) {
  var message_name = param.message_name;
  var sub_scope_packages = message_scope.packages;
  var sub_scope_message_names = Pervasives.$at(message_scope.message_names, {
        hd: message_name,
        tl: /* [] */0
      });
  var sub_scope = {
    packages: sub_scope_packages,
    message_names: sub_scope_message_names
  };
  var match = List.fold_left((function (param, f) {
          var all_types = param[2];
          var extensions = param[1];
          var message_body = param[0];
          switch (f.TAG | 0) {
            case /* Message_field */0 :
                var field = {
                  TAG: /* Message_field */0,
                  _0: compile_field_p1(f._0)
                };
                return [
                        {
                          hd: field,
                          tl: message_body
                        },
                        extensions,
                        all_types
                      ];
            case /* Message_map_field */1 :
                var field$1 = {
                  TAG: /* Message_map_field */2,
                  _0: compile_map_p1(f._0)
                };
                return [
                        {
                          hd: field$1,
                          tl: message_body
                        },
                        extensions,
                        all_types
                      ];
            case /* Message_oneof_field */2 :
                var field$2 = {
                  TAG: /* Message_oneof_field */1,
                  _0: compile_oneof_p1(f._0)
                };
                return [
                        {
                          hd: field$2,
                          tl: message_body
                        },
                        extensions,
                        all_types
                      ];
            case /* Message_sub */3 :
                var all_sub_types = compile_message_p1(file_name, file_options, sub_scope, f._0);
                return [
                        message_body,
                        extensions,
                        Pervasives.$at(all_types, all_sub_types)
                      ];
            case /* Message_enum */4 :
                return [
                        message_body,
                        extensions,
                        Pervasives.$at(all_types, {
                              hd: compile_enum_p1(file_name, file_options, sub_scope, f._0),
                              tl: /* [] */0
                            })
                      ];
            case /* Message_extension */5 :
                return [
                        message_body,
                        Pervasives.$at(extensions, f._0),
                        all_types
                      ];
            
          }
        }), [
        /* [] */0,
        /* [] */0,
        /* [] */0
      ], param.message_body);
  var message_body = List.rev(match[0]);
  var validate_duplicate = function (number_index, field) {
    var number = field_number(field);
    var name = field_name(field);
    if (not_found(function (param) {
            List.assoc(number, number_index);
            
          }) && not_found(function (param) {
            list_assoc2(name, number_index);
            
          })) {
      return {
              hd: [
                number,
                name
              ],
              tl: number_index
            };
    } else {
      var previous_field_name = "";
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Duplicated_field_number */1,
              _0: {
                field_name: name,
                previous_field_name: previous_field_name,
                message_name: message_name
              }
            },
            Error: new Error()
          };
    }
  };
  List.fold_left((function (number_index, f) {
          switch (f.TAG | 0) {
            case /* Message_field */0 :
                return validate_duplicate(number_index, f._0);
            case /* Message_oneof_field */1 :
                return List.fold_left(validate_duplicate, number_index, f._0.oneof_fields);
            case /* Message_map_field */2 :
                return number_index;
            
          }
        }), /* [] */0, message_body);
  return Pervasives.$at(match[2], {
              hd: type_of_spec(file_name, file_options, param.id, message_scope, {
                    TAG: /* Message */1,
                    _0: {
                      extensions: match[1],
                      message_name: message_name,
                      message_body: message_body
                    }
                  }),
              tl: /* [] */0
            });
}

function compile_proto_p1(file_name, param) {
  var file_options = param.file_options;
  var scope = scope_of_package(param.package);
  var pbtt_msgs = List.fold_right((function (e, pbtt_msgs) {
          return {
                  hd: compile_enum_p1(file_name, file_options, scope, e),
                  tl: pbtt_msgs
                };
        }), param.enums, /* [] */0);
  return List.fold_left((function (pbtt_msgs, pbpt_msg) {
                return Pervasives.$at(pbtt_msgs, compile_message_p1(file_name, file_options, scope, pbpt_msg));
              }), pbtt_msgs, param.messages);
}

function type_scope_of_type(param) {
  return param.scope;
}

function is_empty_message(param) {
  var match = param.spec;
  if (match.TAG === /* Enum */0) {
    return false;
  } else {
    return 0 === List.length(match._0.message_body);
  }
}

function type_name_of_type(param) {
  var match = param.spec;
  if (match.TAG === /* Enum */0) {
    return match._0.enum_name;
  } else {
    return match._0.message_name;
  }
}

function find_all_types_in_field_scope(all_types, scope) {
  return List.filter(function (t) {
                var match = type_scope_of_type(t);
                var dec_scope = Pervasives.$at(match.packages, match.message_names);
                return Caml_obj.caml_equal(dec_scope, scope);
              })(all_types);
}

function compile_message_p2(types, param, message) {
  var message_name = message.message_name;
  var message_scope = Pervasives.$at(param.packages, Pervasives.$at(param.message_names, {
            hd: message_name,
            tl: /* [] */0
          }));
  var search_scopes = function (field_scope, from_root) {
    if (from_root) {
      return {
              hd: field_scope,
              tl: /* [] */0
            };
    }
    var loop = function (_scopes, _l) {
      while(true) {
        var l = _l;
        var scopes = _scopes;
        if (!l) {
          return {
                  hd: field_scope,
                  tl: scopes
                };
        }
        _l = pop_last(l);
        _scopes = {
          hd: Pervasives.$at(l, field_scope),
          tl: scopes
        };
        continue ;
      };
    };
    return List.rev(loop(/* [] */0, message_scope));
  };
  var compile_field_p2 = function (field_name, field_type) {
    Curry._1(log(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "[pbtt] field_name: ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "[pbtt] field_name: %s\n"
            }), field_name);
    if (typeof field_type === "number") {
      if (typeof field_type === "number") {
        return field_type;
      }
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Programatic_error */4,
              _0: /* Unexpected_field_type */1
            },
            Error: new Error()
          };
    }
    var unresolved = field_type._0;
    var type_name = unresolved.type_name;
    endline("[pbtt] " + string_of_unresolved(unresolved));
    var search_scopes$1 = search_scopes(unresolved.scope, unresolved.from_root);
    Curry._1(log(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "[pbtt] message scope: ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* '\n' */10,
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "[pbtt] message scope: %s\n"
            }), string_of_string_list(message_scope));
    List.iteri((function (i, scope) {
            return Curry._2(log(/* Format */{
                            _0: {
                              TAG: /* String_literal */11,
                              _0: "[pbtt] search_scope[",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_i */3,
                                _1: {
                                  TAG: /* Lit_padding */0,
                                  _0: /* Right */1,
                                  _1: 2
                                },
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* String_literal */11,
                                  _0: "] : ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* Char_literal */12,
                                      _0: /* '\n' */10,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            },
                            _1: "[pbtt] search_scope[%2i] : %s\n"
                          }), i, string_of_string_list(scope));
          }), search_scopes$1);
    var id = apply_until((function (scope) {
            var types$1 = find_all_types_in_field_scope(types, scope);
            try {
              var t = List.find((function (t) {
                      return type_name === type_name_of_type(t);
                    }), types$1);
              return type_id_of_type(t);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === "Not_found") {
                return ;
              }
              throw exn;
            }
          }), search_scopes$1);
    if (id !== undefined) {
      return /* Field_type_type */{
              _0: id
            };
    } else {
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Unresolved_type */0,
              _0: {
                field_name: field_name,
                type_: type_name,
                message_name: message_name
              }
            },
            Error: new Error()
          };
    }
  };
  var message_body = List.fold_left((function (message_body, field) {
          switch (field.TAG | 0) {
            case /* Message_field */0 :
                var field$1 = field._0;
                var field_name$1 = field_name(field$1);
                var field_type$1 = field_type(field$1);
                var field_field_parsed = field$1.field_parsed;
                var field_field_type = compile_field_p2(field_name$1, field_type$1);
                var field_field_default = field$1.field_default;
                var field_field_options = field$1.field_options;
                var field$2 = {
                  field_parsed: field_field_parsed,
                  field_type: field_field_type,
                  field_default: field_field_default,
                  field_options: field_field_options
                };
                var field_field_parsed$1 = field_field_parsed;
                var field_field_type$1 = field_field_type;
                var field_field_default$1 = compile_default_p2(types, field$2);
                var field_field_options$1 = field_field_options;
                var field$3 = {
                  field_parsed: field_field_parsed$1,
                  field_type: field_field_type$1,
                  field_default: field_field_default$1,
                  field_options: field_field_options$1
                };
                return {
                        hd: {
                          TAG: /* Message_field */0,
                          _0: field$3
                        },
                        tl: message_body
                      };
            case /* Message_oneof_field */1 :
                var oneof = field._0;
                var oneof_fields = List.fold_left((function (oneof_fields, field) {
                        var field_name$2 = field_name(field);
                        var field_type$2 = field_type(field);
                        var field_type$3 = compile_field_p2(field_name$2, field_type$2);
                        return {
                                hd: {
                                  field_parsed: field.field_parsed,
                                  field_type: field_type$3,
                                  field_default: field.field_default,
                                  field_options: field.field_options
                                },
                                tl: oneof_fields
                              };
                      }), /* [] */0, oneof.oneof_fields);
                var oneof_fields$1 = List.rev(oneof_fields);
                return {
                        hd: {
                          TAG: /* Message_oneof_field */1,
                          _0: {
                            oneof_name: oneof.oneof_name,
                            oneof_fields: oneof_fields$1
                          }
                        },
                        tl: message_body
                      };
            case /* Message_map_field */2 :
                var map = field._0;
                var map_name = map.map_name;
                var map_key_type = compile_field_p2(map_name, map.map_key_type);
                var map_value_type = compile_field_p2(map_name, map.map_value_type);
                var resolved_map = {
                  TAG: /* Message_map_field */2,
                  _0: {
                    map_name: map_name,
                    map_number: map.map_number,
                    map_key_type: map_key_type,
                    map_value_type: map_value_type,
                    map_options: map.map_options
                  }
                };
                return {
                        hd: resolved_map,
                        tl: message_body
                      };
            
          }
        }), /* [] */0, message.message_body);
  var message_body$1 = List.rev(message_body);
  return {
          extensions: message.extensions,
          message_name: message.message_name,
          message_body: message_body$1
        };
}

function node_of_proto_type(param) {
  var match = param.spec;
  var id = param.id;
  if (match.TAG === /* Enum */0) {
    return {
            id: id,
            sub: /* [] */0
          };
  }
  var sub = List.flatten(List.map((function (param) {
              switch (param.TAG | 0) {
                case /* Message_field */0 :
                    var field_type = param._0.field_type;
                    if (typeof field_type === "number") {
                      return /* [] */0;
                    } else {
                      return {
                              hd: field_type._0,
                              tl: /* [] */0
                            };
                    }
                case /* Message_oneof_field */1 :
                    return List.flatten(List.map((function (param) {
                                      var field_type = param.field_type;
                                      if (typeof field_type === "number") {
                                        return /* [] */0;
                                      } else {
                                        return {
                                                hd: field_type._0,
                                                tl: /* [] */0
                                              };
                                      }
                                    }), param._0.oneof_fields));
                case /* Message_map_field */2 :
                    var map_value_type = param._0.map_value_type;
                    if (typeof map_value_type === "number") {
                      return /* [] */0;
                    } else {
                      return {
                              hd: map_value_type._0,
                              tl: /* [] */0
                            };
                    }
                
              }
            }), match._0.message_body));
  return {
          id: id,
          sub: sub
        };
}

function group(proto) {
  var g = List.map(node_of_proto_type, proto);
  var g$1 = List.fold_left((function (m, n) {
          return add(n.id, n, m);
        }), /* Empty */0, g);
  var sccs = tarjan(g$1);
  return List.map((function (l) {
                return List.map((function (id) {
                              return List.find((function (param) {
                                            return id === param.id;
                                          }), proto);
                            }), l);
              }), sccs);
}

function type_decl_of_and(param) {
  if (param !== undefined) {
    return "and";
  } else {
    return "type";
  }
}

function gen_type_record(mutable_, and_, param, sc) {
  var r_fields = param.r_fields;
  var r_name = param.r_name;
  var mutable_$1 = mutable_ !== undefined;
  var is_imperative_type = function (param) {
    switch (param.TAG | 0) {
      case /* Rft_repeated_field */2 :
      case /* Rft_associative_field */3 :
          if (param._0[0]) {
            return true;
          } else {
            return false;
          }
      default:
        return false;
    }
  };
  var field_prefix = function (field_type, field_mutable) {
    if (field_mutable || !(is_imperative_type(field_type) || !mutable_$1)) {
      return "mutable ";
    } else {
      return "";
    }
  };
  var r_name$1 = mutable_$1 ? r_name + "_mutable" : r_name;
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* ' ' */32,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " = {",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s %s = {"
              }), type_decl_of_and(and_), r_name$1));
  scope(sc, (function (sc) {
          return List.iter((function (param) {
                        var rf_field_type = param.rf_field_type;
                        var prefix = field_prefix(rf_field_type, param.rf_mutable);
                        var type_string = string_of_record_field_type(rf_field_type);
                        return line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                                            _0: {
                                              TAG: /* String */2,
                                              _0: /* No_padding */0,
                                              _1: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* String_literal */11,
                                                  _0: " : ",
                                                  _1: {
                                                    TAG: /* String */2,
                                                    _0: /* No_padding */0,
                                                    _1: {
                                                      TAG: /* Char_literal */12,
                                                      _0: /* ';' */59,
                                                      _1: /* End_of_format */0
                                                    }
                                                  }
                                                }
                                              }
                                            },
                                            _1: "%s%s : %s;"
                                          }), prefix, param.rf_label, type_string));
                      }), r_fields);
        }));
  return line$1(sc, "}");
}

function gen_type_variant(and_, variant, sc) {
  var v_constructors = variant.v_constructors;
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* ' ' */32,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " =",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s %s ="
              }), type_decl_of_and(and_), variant.v_name));
  return scope(sc, (function (sc) {
                return List.iter((function (param) {
                              var vc_field_type = param.vc_field_type;
                              var vc_constructor = param.vc_constructor;
                              if (!vc_field_type) {
                                return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "| ",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: /* End_of_format */0
                                                      }
                                                    },
                                                    _1: "| %s"
                                                  }), vc_constructor));
                              }
                              var type_string = string_of_field_type(vc_field_type._0);
                              return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* String_literal */11,
                                                    _0: "| ",
                                                    _1: {
                                                      TAG: /* String */2,
                                                      _0: /* No_padding */0,
                                                      _1: {
                                                        TAG: /* String_literal */11,
                                                        _0: " of ",
                                                        _1: {
                                                          TAG: /* String */2,
                                                          _0: /* No_padding */0,
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    }
                                                  },
                                                  _1: "| %s of %s"
                                                }), vc_constructor, type_string));
                            }), v_constructors);
              }));
}

function gen_type_const_variant(and_, param, sc) {
  var cv_constructors = param.cv_constructors;
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* Char_literal */12,
                    _0: /* ' ' */32,
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " =",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                },
                _1: "%s %s ="
              }), type_decl_of_and(and_), param.cv_name));
  return scope(sc, (function (sc) {
                return List.iter((function (param) {
                              return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* String_literal */11,
                                                    _0: "| ",
                                                    _1: {
                                                      TAG: /* String */2,
                                                      _0: /* No_padding */0,
                                                      _1: {
                                                        TAG: /* Char_literal */12,
                                                        _0: /* ' ' */32,
                                                        _1: /* End_of_format */0
                                                      }
                                                    }
                                                  },
                                                  _1: "| %s "
                                                }), param[0]));
                            }), cv_constructors);
              }));
}

function gen_struct$2(and_, t, scope) {
  var r = t.spec;
  switch (r.TAG | 0) {
    case /* Record */0 :
        var r$1 = r._0;
        gen_type_record(undefined, and_, r$1, scope);
        line$1(scope, "");
        gen_type_record(Caml_option.some(undefined), Caml_option.some(undefined), r$1, scope);
        break;
    case /* Variant */1 :
        gen_type_variant(and_, r._0, scope);
        break;
    case /* Const_variant */2 :
        gen_type_const_variant(and_, r._0, scope);
        break;
    
  }
  return true;
}

function gen_sig$2(and_, t, scope) {
  var r = t.spec;
  switch (r.TAG | 0) {
    case /* Record */0 :
        gen_type_record(undefined, and_, r._0, scope);
        break;
    case /* Variant */1 :
        gen_type_variant(and_, r._0, scope);
        break;
    case /* Const_variant */2 :
        gen_type_const_variant(and_, r._0, scope);
        break;
    
  }
  return true;
}

var Codegen_type = {
  gen_sig: gen_sig$2,
  gen_struct: gen_struct$2,
  ocamldoc_title: "Types"
};

function gen_encode_field_key(sc, number, pk, is_packed) {
  var s = string_of_payload_kind(undefined, pk, is_packed);
  var s$1 = Caml_bytes.bytes_to_string(Bytes.lowercase(Caml_bytes.bytes_of_string(s)));
  return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "Pbrt.Encoder.key (",
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_i */3,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: ", Pbrt.",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: ") encoder; ",
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      },
                      _1: "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; "
                    }), number, Caml_bytes.bytes_to_string(Bytes.capitalize(Caml_bytes.bytes_of_string(s$1)))));
}

function encode_basic_type(bt, pk) {
  return runtime_function([
              "Encode",
              pk,
              bt
            ]);
}

function gen_encode_field_type(with_key, sc, var_name, encoding_number, pk, is_packed, field_type) {
  var encode_key = function (sc) {
    if (with_key !== undefined) {
      return gen_encode_field_key(sc, encoding_number, pk, is_packed);
    }
    
  };
  if (typeof field_type === "number") {
    encode_key(sc);
    return line$1(sc, "Pbrt.Encoder.empty_nested encoder;");
  }
  if (field_type.TAG === /* Ft_basic_type */0) {
    encode_key(sc);
    var rt = encode_basic_type(field_type._0, pk);
    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* ' ' */32,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: " encoder;",
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        },
                        _1: "%s %s encoder;"
                      }), rt, var_name));
  }
  var ud = field_type._0;
  encode_key(sc);
  var f_name = function_name_of_user_defined("encode", ud);
  if (ud.udt_nested) {
    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "Pbrt.Encoder.nested (",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* ' ' */32,
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: ") encoder;",
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "Pbrt.Encoder.nested (%s %s) encoder;"
                      }), f_name, var_name));
  } else {
    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* ' ' */32,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: " encoder;",
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        },
                        _1: "%s %s encoder;"
                      }), f_name, var_name));
  }
}

function gen_encode_record(and_, param, sc) {
  var r_fields = param.r_fields;
  var r_name = param.r_name;
  Curry._1(log(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "gen_encode_record record_name: ",
              _1: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* '\n' */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "gen_encode_record record_name: %s\n"
          }), r_name);
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " encode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") encoder = ",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s encode_%s (v:%s) encoder = "
              }), let_decl_of_and(and_), r_name, r_name));
  return scope(sc, (function (sc) {
                List.iter((function (record_field) {
                        var rf_field_type = record_field.rf_field_type;
                        var rf_label = record_field.rf_label;
                        switch (rf_field_type.TAG | 0) {
                          case /* Rft_required */0 :
                              var match = rf_field_type._0;
                              var var_name = Curry._1(Printf.sprintf(/* Format */{
                                        _0: {
                                          TAG: /* String_literal */11,
                                          _0: "v.",
                                          _1: {
                                            TAG: /* String */2,
                                            _0: /* No_padding */0,
                                            _1: /* End_of_format */0
                                          }
                                        },
                                        _1: "v.%s"
                                      }), rf_label);
                              return gen_encode_field_type(Caml_option.some(undefined), sc, var_name, match[1], match[2], false, match[0]);
                          case /* Rft_optional */1 :
                              var match$1 = rf_field_type._0;
                              var pk = match$1[2];
                              var encoding_number = match$1[1];
                              var field_type = match$1[0];
                              line$1(sc, "(");
                              scope(sc, (function (sc) {
                                      line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "match v.",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: " with ",
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    },
                                                    _1: "match v.%s with "
                                                  }), rf_label));
                                      line$1(sc, Printf.sprintf(/* Format */{
                                                _0: {
                                                  TAG: /* String_literal */11,
                                                  _0: "| Some x -> (",
                                                  _1: /* End_of_format */0
                                                },
                                                _1: "| Some x -> ("
                                              }));
                                      scope(sc, (function (sc) {
                                              return gen_encode_field_type(Caml_option.some(undefined), sc, "x", encoding_number, pk, false, field_type);
                                            }));
                                      line$1(sc, ")");
                                      return line$1(sc, "| None -> ();");
                                    }));
                              return line$1(sc, ");");
                          case /* Rft_repeated_field */2 :
                              var match$2 = rf_field_type._0;
                              var is_packed = match$2[4];
                              var pk$1 = match$2[3];
                              var encoding_number$1 = match$2[2];
                              var field_type$1 = match$2[1];
                              if (match$2[0]) {
                                if (is_packed) {
                                  gen_encode_field_key(sc, encoding_number$1, pk$1, is_packed);
                                  line$1(sc, "Pbrt.Encoder.nested (fun encoder ->");
                                  scope(sc, (function (sc) {
                                          line$1(sc, "Pbrt.Repeated_field.iter (fun x -> ");
                                          scope(sc, (function (sc) {
                                                  return gen_encode_field_type(undefined, sc, "x", encoding_number$1, pk$1, is_packed, field_type$1);
                                                }));
                                          return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                              _0: {
                                                                TAG: /* String_literal */11,
                                                                _0: ") v.",
                                                                _1: {
                                                                  TAG: /* String */2,
                                                                  _0: /* No_padding */0,
                                                                  _1: {
                                                                    TAG: /* Char_literal */12,
                                                                    _0: /* ';' */59,
                                                                    _1: /* End_of_format */0
                                                                  }
                                                                }
                                                              },
                                                              _1: ") v.%s;"
                                                            }), rf_label));
                                        }));
                                  return line$1(sc, ") encoder;");
                                } else {
                                  line$1(sc, "Pbrt.Repeated_field.iter (fun x -> ");
                                  scope(sc, (function (sc) {
                                          return gen_encode_field_type(Caml_option.some(undefined), sc, "x", encoding_number$1, pk$1, is_packed, field_type$1);
                                        }));
                                  return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* String_literal */11,
                                                        _0: ") v.",
                                                        _1: {
                                                          TAG: /* String */2,
                                                          _0: /* No_padding */0,
                                                          _1: {
                                                            TAG: /* Char_literal */12,
                                                            _0: /* ';' */59,
                                                            _1: /* End_of_format */0
                                                          }
                                                        }
                                                      },
                                                      _1: ") v.%s;"
                                                    }), rf_label));
                                }
                              } else if (is_packed) {
                                gen_encode_field_key(sc, encoding_number$1, pk$1, is_packed);
                                line$1(sc, "Pbrt.Encoder.nested (fun encoder ->");
                                scope(sc, (function (sc) {
                                        line$1(sc, "List.iter (fun x -> ");
                                        scope(sc, (function (sc) {
                                                return gen_encode_field_type(undefined, sc, "x", encoding_number$1, pk$1, is_packed, field_type$1);
                                              }));
                                        return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                            _0: {
                                                              TAG: /* String_literal */11,
                                                              _0: ") v.",
                                                              _1: {
                                                                TAG: /* String */2,
                                                                _0: /* No_padding */0,
                                                                _1: {
                                                                  TAG: /* Char_literal */12,
                                                                  _0: /* ';' */59,
                                                                  _1: /* End_of_format */0
                                                                }
                                                              }
                                                            },
                                                            _1: ") v.%s;"
                                                          }), rf_label));
                                      }));
                                return line$1(sc, ") encoder;");
                              } else {
                                line$1(sc, "List.iter (fun x -> ");
                                scope(sc, (function (sc) {
                                        return gen_encode_field_type(Caml_option.some(undefined), sc, "x", encoding_number$1, pk$1, is_packed, field_type$1);
                                      }));
                                return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: ") v.",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* Char_literal */12,
                                                          _0: /* ';' */59,
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    },
                                                    _1: ") v.%s;"
                                                  }), rf_label));
                              }
                          case /* Rft_associative_field */3 :
                              var match$3 = rf_field_type._0;
                              var match$4 = match$3[3];
                              var value_pk = match$4[1];
                              var value_type = match$4[0];
                              var match$5 = match$3[2];
                              var key_pk = match$5[1];
                              var encoding_number$2 = match$3[1];
                              line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                            _0: {
                                              TAG: /* String_literal */11,
                                              _0: "let encode_key = ",
                                              _1: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* String_literal */11,
                                                  _0: " in",
                                                  _1: /* End_of_format */0
                                                }
                                              }
                                            },
                                            _1: "let encode_key = %s in"
                                          }), encode_basic_type(match$5[0], key_pk)));
                              line$1(sc, "let encode_value = (fun x encoder ->");
                              scope(sc, (function (sc) {
                                      return gen_encode_field_type(undefined, sc, "x", -1, value_pk, false, value_type);
                                    }));
                              line$1(sc, ") in");
                              if (match$3[0]) {
                                line$1(sc, "Hashtbl.iter (fun k v ->");
                              } else {
                                line$1(sc, "List.iter (fun (k, v) ->");
                              }
                              scope(sc, (function (sc) {
                                      gen_encode_field_key(sc, encoding_number$2, /* Pk_bytes */2, false);
                                      line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "let map_entry = (k, Pbrt.",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: "), (v, Pbrt.",
                                                          _1: {
                                                            TAG: /* String */2,
                                                            _0: /* No_padding */0,
                                                            _1: {
                                                              TAG: /* String_literal */11,
                                                              _0: ") in",
                                                              _1: /* End_of_format */0
                                                            }
                                                          }
                                                        }
                                                      }
                                                    },
                                                    _1: "let map_entry = (k, Pbrt.%s), (v, Pbrt.%s) in"
                                                  }), string_of_payload_kind(Caml_option.some(undefined), key_pk, false), string_of_payload_kind(Caml_option.some(undefined), value_pk, false)));
                                      return line$1(sc, "Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder");
                                    }));
                              return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* String_literal */11,
                                                    _0: ") v.",
                                                    _1: {
                                                      TAG: /* String */2,
                                                      _0: /* No_padding */0,
                                                      _1: {
                                                        TAG: /* Char_literal */12,
                                                        _0: /* ';' */59,
                                                        _1: /* End_of_format */0
                                                      }
                                                    }
                                                  },
                                                  _1: ") v.%s;"
                                                }), rf_label));
                          case /* Rft_variant_field */4 :
                              var v_constructors = rf_field_type._0.v_constructors;
                              line$1(sc, "(");
                              scope(sc, (function (sc) {
                                      line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "match v.",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: " with",
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    },
                                                    _1: "match v.%s with"
                                                  }), rf_label));
                                      return List.iter((function (param) {
                                                    var vc_payload_kind = param.vc_payload_kind;
                                                    var vc_encoding_number = param.vc_encoding_number;
                                                    var vc_field_type = param.vc_field_type;
                                                    var vc_constructor = param.vc_constructor;
                                                    if (vc_field_type) {
                                                      var field_type = vc_field_type._0;
                                                      line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                                    _0: {
                                                                      TAG: /* String_literal */11,
                                                                      _0: "| ",
                                                                      _1: {
                                                                        TAG: /* String */2,
                                                                        _0: /* No_padding */0,
                                                                        _1: {
                                                                          TAG: /* String_literal */11,
                                                                          _0: " x -> (",
                                                                          _1: /* End_of_format */0
                                                                        }
                                                                      }
                                                                    },
                                                                    _1: "| %s x -> ("
                                                                  }), vc_constructor));
                                                      scope(sc, (function (sc) {
                                                              return gen_encode_field_type(Caml_option.some(undefined), sc, "x", vc_encoding_number, vc_payload_kind, false, field_type);
                                                            }));
                                                      return line$1(sc, ")");
                                                    }
                                                    line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                                                  _0: {
                                                                    TAG: /* String_literal */11,
                                                                    _0: "| ",
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* String_literal */11,
                                                                        _0: " -> (",
                                                                        _1: /* End_of_format */0
                                                                      }
                                                                    }
                                                                  },
                                                                  _1: "| %s -> ("
                                                                }), vc_constructor));
                                                    scope(sc, (function (sc) {
                                                            gen_encode_field_key(sc, vc_encoding_number, vc_payload_kind, false);
                                                            return line$1(sc, "Pbrt.Encoder.empty_nested encoder");
                                                          }));
                                                    return line$1(sc, ")");
                                                  }), v_constructors);
                                    }));
                              return line$1(sc, ");");
                          
                        }
                      }), r_fields);
                return line$1(sc, "()");
              }));
}

function gen_encode_variant(and_, variant, sc) {
  var v_constructors = variant.v_constructors;
  var v_name = variant.v_name;
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " encode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") encoder = ",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s encode_%s (v:%s) encoder = "
              }), let_decl_of_and(and_), v_name, v_name));
  return scope(sc, (function (sc) {
                line$1(sc, "match v with");
                return List.iter((function (param) {
                              var vc_payload_kind = param.vc_payload_kind;
                              var vc_encoding_number = param.vc_encoding_number;
                              var vc_field_type = param.vc_field_type;
                              var vc_constructor = param.vc_constructor;
                              if (vc_field_type) {
                                var field_type = vc_field_type._0;
                                line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                              _0: {
                                                TAG: /* String_literal */11,
                                                _0: "| ",
                                                _1: {
                                                  TAG: /* String */2,
                                                  _0: /* No_padding */0,
                                                  _1: {
                                                    TAG: /* String_literal */11,
                                                    _0: " x -> (",
                                                    _1: /* End_of_format */0
                                                  }
                                                }
                                              },
                                              _1: "| %s x -> ("
                                            }), vc_constructor));
                                scope(sc, (function (sc) {
                                        return gen_encode_field_type(Caml_option.some(undefined), sc, "x", vc_encoding_number, vc_payload_kind, false, field_type);
                                      }));
                                return line$1(sc, ")");
                              }
                              line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                            _0: {
                                              TAG: /* String_literal */11,
                                              _0: "| ",
                                              _1: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* String_literal */11,
                                                  _0: " -> (",
                                                  _1: /* End_of_format */0
                                                }
                                              }
                                            },
                                            _1: "| %s -> ("
                                          }), vc_constructor));
                              scope(sc, (function (sc) {
                                      gen_encode_field_key(sc, vc_encoding_number, vc_payload_kind, false);
                                      return line$1(sc, "Pbrt.Encoder.empty_nested encoder");
                                    }));
                              return line$1(sc, ")");
                            }), v_constructors);
              }));
}

function gen_encode_const_variant(and_, param, sc) {
  var cv_constructors = param.cv_constructors;
  var cv_name = param.cv_name;
  line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " encode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " (v:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: ") encoder =",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                },
                _1: "%s encode_%s (v:%s) encoder ="
              }), let_decl_of_and(and_), cv_name, cv_name));
  return scope(sc, (function (sc) {
                line$1(sc, "match v with");
                return List.iter((function (param) {
                              var value = param[1];
                              var name = param[0];
                              return line$1(sc, value > 0 ? Curry._2(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "| ",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: " -> Pbrt.Encoder.int_as_varint ",
                                                          _1: {
                                                            TAG: /* Int */4,
                                                            _0: /* Int_i */3,
                                                            _1: /* No_padding */0,
                                                            _2: /* No_precision */0,
                                                            _3: {
                                                              TAG: /* String_literal */11,
                                                              _0: " encoder",
                                                              _1: /* End_of_format */0
                                                            }
                                                          }
                                                        }
                                                      }
                                                    },
                                                    _1: "| %s -> Pbrt.Encoder.int_as_varint %i encoder"
                                                  }), name, value) : Curry._2(Printf.sprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* String_literal */11,
                                                      _0: "| ",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* String_literal */11,
                                                          _0: " -> Pbrt.Encoder.int_as_varint (",
                                                          _1: {
                                                            TAG: /* Int */4,
                                                            _0: /* Int_i */3,
                                                            _1: /* No_padding */0,
                                                            _2: /* No_precision */0,
                                                            _3: {
                                                              TAG: /* String_literal */11,
                                                              _0: ") encoder",
                                                              _1: /* End_of_format */0
                                                            }
                                                          }
                                                        }
                                                      }
                                                    },
                                                    _1: "| %s -> Pbrt.Encoder.int_as_varint (%i) encoder"
                                                  }), name, value));
                            }), cv_constructors);
              }));
}

function gen_struct$3(and_, t, sc) {
  var r = t.spec;
  var tmp;
  switch (r.TAG | 0) {
    case /* Record */0 :
        tmp = [
          gen_encode_record(and_, r._0, sc),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          gen_encode_variant(and_, r._0, sc),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          gen_encode_const_variant(and_, r._0, sc),
          true
        ];
        break;
    
  }
  return tmp[1];
}

function gen_sig$3(and_, t, sc) {
  var f = function (type_name) {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "val encode_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " : ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: " -> Pbrt.Encoder.t -> unit",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "val encode_%s : %s -> Pbrt.Encoder.t -> unit"
                }), type_name, type_name));
    return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "(** [encode_",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " v encoder] encodes [v] with the given [encoder] *)",
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)"
                      }), type_name));
  };
  var v = t.spec;
  var tmp;
  switch (v.TAG | 0) {
    case /* Record */0 :
        tmp = [
          f(v._0.r_name),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          f(v._0.v_name),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          f(v._0.cv_name),
          true
        ];
        break;
    
  }
  return tmp[1];
}

var Codegen_encode = {
  gen_sig: gen_sig$3,
  gen_struct: gen_struct$3,
  ocamldoc_title: "Protobuf Toding"
};

function default_value_of_field_type(field_name, field_type, field_default) {
  if (typeof field_type === "number") {
    return "()";
  } else if (field_type.TAG === /* Ft_basic_type */0) {
    var basic_type = field_type._0;
    switch (basic_type) {
      case /* Bt_string */0 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_string */0) {
              return Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* Char_literal */12,
                                _0: /* '"' */34,
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* Char_literal */12,
                                    _0: /* '"' */34,
                                    _1: /* End_of_format */0
                                  }
                                }
                              },
                              _1: "\"%s\""
                            }), field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "\"\"";
          }
      case /* Bt_float */1 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_float */3) {
              return Pervasives.string_of_float(field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "0.";
          }
      case /* Bt_int */2 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_int */2) {
              return String(field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "0";
          }
      case /* Bt_int32 */3 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_int */2) {
              return Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* Int */4,
                                _0: /* Int_i */3,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* Char_literal */12,
                                  _0: /* 'l' */108,
                                  _1: /* End_of_format */0
                                }
                              },
                              _1: "%il"
                            }), field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "0l";
          }
      case /* Bt_int64 */4 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_int */2) {
              return Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* Int */4,
                                _0: /* Int_i */3,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* Char_literal */12,
                                  _0: /* 'L' */76,
                                  _1: /* End_of_format */0
                                }
                              },
                              _1: "%iL"
                            }), field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "0L";
          }
      case /* Bt_bytes */5 :
          if (field_default !== undefined) {
            if (field_default.TAG === /* Constant_string */0) {
              return Curry._1(Printf.sprintf(/* Format */{
                              _0: {
                                TAG: /* String_literal */11,
                                _0: "Bytes.of_string \"",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* Char_literal */12,
                                    _0: /* '"' */34,
                                    _1: /* End_of_format */0
                                  }
                                }
                              },
                              _1: "Bytes.of_string \"%s\""
                            }), field_default._0);
            } else {
              return invalid_default_value(field_name, "invalid default type", undefined);
            }
          } else {
            return "Bytes.create 64";
          }
      case /* Bt_bool */6 :
          if (field_default === undefined) {
            return "false";
          }
          if (field_default.TAG !== /* Constant_bool */1) {
            return invalid_default_value(field_name, "invalid default type", undefined);
          }
          var b = field_default._0;
          if (b) {
            return "true";
          } else {
            return "false";
          }
      
    }
  } else {
    return function_name_of_user_defined("default", field_type._0) + " ()";
  }
}

function record_field_default_info(record_field) {
  var rf_field_type = record_field.rf_field_type;
  var rf_label = record_field.rf_label;
  var type_string = string_of_record_field_type(rf_field_type);
  var dfvft = function (field_type, defalut_value) {
    return default_value_of_field_type(rf_label, field_type, defalut_value);
  };
  var default_value;
  switch (rf_field_type.TAG | 0) {
    case /* Rft_required */0 :
        var match = rf_field_type._0;
        default_value = dfvft(match[0], match[3]);
        break;
    case /* Rft_optional */1 :
        var match$1 = rf_field_type._0;
        var default_value$1 = match$1[3];
        default_value = default_value$1 !== undefined ? Curry._1(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Some (",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* ')' */41,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "Some (%s)"
                  }), dfvft(match$1[0], default_value$1)) : "None";
        break;
    case /* Rft_repeated_field */2 :
        var match$2 = rf_field_type._0;
        default_value = match$2[0] ? Curry._1(Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Pbrt.Repeated_field.make (",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* ')' */41,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "Pbrt.Repeated_field.make (%s)"
                  }), dfvft(match$2[1], undefined)) : "[]";
        break;
    case /* Rft_associative_field */3 :
        default_value = rf_field_type._0[0] ? "Hashtbl.create 128" : "[]";
        break;
    case /* Rft_variant_field */4 :
        var v_constructors = rf_field_type._0.v_constructors;
        if (v_constructors) {
          var match$3 = v_constructors.hd;
          var vc_field_type = match$3.vc_field_type;
          var vc_constructor = match$3.vc_constructor;
          default_value = vc_field_type ? Curry._2(Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: " (",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* ')' */41,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      },
                      _1: "%s (%s)"
                    }), vc_constructor, dfvft(vc_field_type._0, undefined)) : vc_constructor;
        } else {
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "codegen_default.ml",
                  74,
                  15
                ],
                Error: new Error()
              };
        }
        break;
    
  }
  return [
          rf_label,
          default_value,
          type_string
        ];
}

function gen_default_record(mutable_, and_, param, sc) {
  var r_name = param.r_name;
  var fields_default_info = List.map(record_field_default_info, param.r_fields);
  if (mutable_ !== undefined) {
    var rn = r_name + "_mutable";
    line$1(sc, Curry._3(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " default_",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: " () : ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " = {",
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    }
                  },
                  _1: "%s default_%s () : %s = {"
                }), let_decl_of_and(and_), rn, rn));
    scope(sc, (function (sc) {
            return List.iter((function (param) {
                          return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                              _0: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* String_literal */11,
                                                  _0: " = ",
                                                  _1: {
                                                    TAG: /* String */2,
                                                    _0: /* No_padding */0,
                                                    _1: {
                                                      TAG: /* Char_literal */12,
                                                      _0: /* ';' */59,
                                                      _1: /* End_of_format */0
                                                    }
                                                  }
                                                }
                                              },
                                              _1: "%s = %s;"
                                            }), param[0], param[1]));
                        }), fields_default_info);
          }));
  } else {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " default_",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* ' ' */32,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "%s default_%s "
                }), let_decl_of_and(and_), r_name));
    scope(sc, (function (sc) {
            List.iter((function (param) {
                    var fname = param[0];
                    return line$1(sc, Curry._4(Printf.sprintf(/* Format */{
                                        _0: {
                                          TAG: /* Char_literal */12,
                                          _0: /* '?' */63,
                                          _1: {
                                            TAG: /* String */2,
                                            _0: /* No_padding */0,
                                            _1: {
                                              TAG: /* String_literal */11,
                                              _0: ":((",
                                              _1: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* Char_literal */12,
                                                  _0: /* ':' */58,
                                                  _1: {
                                                    TAG: /* String */2,
                                                    _0: /* No_padding */0,
                                                    _1: {
                                                      TAG: /* String_literal */11,
                                                      _0: ") = ",
                                                      _1: {
                                                        TAG: /* String */2,
                                                        _0: /* No_padding */0,
                                                        _1: {
                                                          TAG: /* Char_literal */12,
                                                          _0: /* ')' */41,
                                                          _1: /* End_of_format */0
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        },
                                        _1: "?%s:((%s:%s) = %s)"
                                      }), fname, fname, param[2], param[1]));
                  }), fields_default_info);
            return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                _0: {
                                  TAG: /* String_literal */11,
                                  _0: "() : ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* String_literal */11,
                                      _0: "  = {",
                                      _1: /* End_of_format */0
                                    }
                                  }
                                },
                                _1: "() : %s  = {"
                              }), r_name));
          }));
    scope(sc, (function (sc) {
            return List.iter((function (param) {
                          return line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                              _0: {
                                                TAG: /* String */2,
                                                _0: /* No_padding */0,
                                                _1: {
                                                  TAG: /* Char_literal */12,
                                                  _0: /* ';' */59,
                                                  _1: /* End_of_format */0
                                                }
                                              },
                                              _1: "%s;"
                                            }), param[0]));
                        }), fields_default_info);
          }));
  }
  return line$1(sc, "}");
}

function gen_default_variant(and_, param, sc) {
  var v_constructors = param.v_constructors;
  var v_name = param.v_name;
  if (v_constructors) {
    var match = v_constructors.hd;
    var vc_field_type = match.vc_field_type;
    var vc_constructor = match.vc_constructor;
    var decl = let_decl_of_and(and_);
    if (!vc_field_type) {
      return line$1(sc, Curry._4(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " default_",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: " (): ",
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* String_literal */11,
                                      _0: " = ",
                                      _1: {
                                        TAG: /* String */2,
                                        _0: /* No_padding */0,
                                        _1: /* End_of_format */0
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          },
                          _1: "%s default_%s (): %s = %s"
                        }), decl, v_name, v_name, vc_constructor));
    }
    var default_value = default_value_of_field_type(v_name, vc_field_type._0, undefined);
    return line$1(sc, Curry._5(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* String_literal */11,
                            _0: " default_",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: " () : ",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: " = ",
                                    _1: {
                                      TAG: /* String */2,
                                      _0: /* No_padding */0,
                                      _1: {
                                        TAG: /* String_literal */11,
                                        _0: " (",
                                        _1: {
                                          TAG: /* String */2,
                                          _0: /* No_padding */0,
                                          _1: {
                                            TAG: /* Char_literal */12,
                                            _0: /* ')' */41,
                                            _1: /* End_of_format */0
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "%s default_%s () : %s = %s (%s)"
                      }), decl, v_name, v_name, vc_constructor, default_value));
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "programmatic TODO error",
        Error: new Error()
      };
}

function gen_default_const_variant(and_, param, sc) {
  var cv_constructors = param.cv_constructors;
  var cv_name = param.cv_name;
  var first_constructor_name;
  if (cv_constructors) {
    first_constructor_name = cv_constructors.hd[0];
  } else {
    throw {
          RE_EXN_ID: "Failure",
          _1: "programmatic TODO error",
          Error: new Error()
        };
  }
  return line$1(sc, Curry._4(Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: " default_",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " () = (",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* Char_literal */12,
                                  _0: /* ':' */58,
                                  _1: {
                                    TAG: /* String */2,
                                    _0: /* No_padding */0,
                                    _1: {
                                      TAG: /* Char_literal */12,
                                      _0: /* ')' */41,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      },
                      _1: "%s default_%s () = (%s:%s)"
                    }), let_decl_of_and(and_), cv_name, first_constructor_name, cv_name));
}

function gen_struct$4(and_, t, sc) {
  var r = t.spec;
  var tmp;
  switch (r.TAG | 0) {
    case /* Record */0 :
        var r$1 = r._0;
        tmp = [
          (gen_default_record(undefined, and_, r$1, sc), line$1(sc, ""), gen_default_record(Caml_option.some(undefined), Caml_option.some(undefined), r$1, sc)),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          gen_default_variant(and_, r._0, sc),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          gen_default_const_variant(undefined, r._0, sc),
          true
        ];
        break;
    
  }
  return tmp[1];
}

function gen_sig_record(sc, param) {
  var r_name = param.r_name;
  line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "val default_",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " : ",
                      _1: /* End_of_format */0
                    }
                  }
                },
                _1: "val default_%s : "
              }), r_name));
  var fields_default_info = List.map(record_field_default_info, param.r_fields);
  scope(sc, (function (sc) {
          List.iter((function (param) {
                  return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                                      _0: {
                                        TAG: /* Char_literal */12,
                                        _0: /* '?' */63,
                                        _1: {
                                          TAG: /* String */2,
                                          _0: /* No_padding */0,
                                          _1: {
                                            TAG: /* Char_literal */12,
                                            _0: /* ':' */58,
                                            _1: {
                                              TAG: /* String */2,
                                              _0: /* No_padding */0,
                                              _1: {
                                                TAG: /* String_literal */11,
                                                _0: " ->",
                                                _1: /* End_of_format */0
                                              }
                                            }
                                          }
                                        }
                                      },
                                      _1: "?%s:%s ->"
                                    }), param[0], param[2]));
                }), fields_default_info);
          line$1(sc, "unit ->");
          return line$1(sc, r_name);
        }));
  line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "(** [default_",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " ()] is the default value for type [",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* String_literal */11,
                          _0: "] *)",
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  }
                },
                _1: "(** [default_%s ()] is the default value for type [%s] *)"
              }), r_name, r_name));
  
}

function gen_sig$4(and_, t, sc) {
  var f = function (type_name) {
    line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "val default_",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* String_literal */11,
                        _0: " : unit -> ",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "val default_%s : unit -> %s"
                }), type_name, type_name));
    return line$1(sc, Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "(** [default_",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* String_literal */11,
                              _0: " ()] is the default value for type [",
                              _1: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* String_literal */11,
                                  _0: "] *)",
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "(** [default_%s ()] is the default value for type [%s] *)"
                      }), type_name, type_name));
  };
  var r = t.spec;
  var tmp;
  switch (r.TAG | 0) {
    case /* Record */0 :
        tmp = [
          gen_sig_record(sc, r._0),
          true
        ];
        break;
    case /* Variant */1 :
        tmp = [
          f(r._0.v_name),
          true
        ];
        break;
    case /* Const_variant */2 :
        tmp = [
          f(r._0.cv_name),
          true
        ];
        break;
    
  }
  return tmp[1];
}

var Codegen_default = {
  gen_sig: gen_sig$4,
  gen_struct: gen_struct$4,
  ocamldoc_title: "Default values"
};

function rev_split_by_naming_convention(s) {
  var is_uppercase = function (c) {
    if (64 < c) {
      return c < 91;
    } else {
      return false;
    }
  };
  var add_sub_string = function (start_i, end_i, l) {
    if (start_i === end_i) {
      return l;
    } else {
      return {
              hd: $$String.sub(s, start_i, end_i - start_i | 0),
              tl: l
            };
    }
  };
  var match = string_fold_lefti((function (param, i, c) {
          var start_i = param[1];
          var l = param[0];
          if (c !== 95) {
            if (param[2] || !is_uppercase(c)) {
              return [
                      l,
                      start_i,
                      is_uppercase(c)
                    ];
            } else {
              return [
                      add_sub_string(start_i, i, l),
                      i,
                      true
                    ];
            }
          } else {
            return [
                    add_sub_string(start_i, i, l),
                    i + 1 | 0,
                    false
                  ];
          }
        }), [
        /* [] */0,
        0,
        false
      ], s);
  var len = s.length;
  return add_sub_string(match[1], len, match[0]);
}

function fix_ocaml_keyword_conflict(s) {
  switch (s) {
    case "and" :
    case "as" :
    case "asr" :
    case "assert" :
    case "begin" :
    case "class" :
    case "constraint" :
    case "do" :
    case "done" :
    case "downto" :
    case "else" :
    case "end" :
    case "exception" :
    case "external" :
    case "false" :
    case "for" :
    case "fun" :
    case "function" :
    case "functor" :
    case "if" :
    case "in" :
    case "include" :
    case "inherit" :
    case "initializer" :
    case "land" :
    case "lazy" :
    case "let" :
    case "lor" :
    case "lsl" :
    case "lsr" :
    case "lxor" :
    case "match" :
    case "method" :
    case "mod" :
    case "module" :
    case "mutable" :
    case "new" :
    case "nonrec" :
    case "object" :
    case "of" :
    case "open" :
    case "or" :
    case "private" :
    case "rec" :
    case "sig" :
    case "struct" :
    case "then" :
    case "to" :
    case "true" :
    case "try" :
    case "type" :
    case "val" :
    case "virtual" :
    case "when" :
    case "while" :
    case "with" :
        return s + "_";
    default:
      return s;
  }
}

function constructor_name(s) {
  var s$1 = $$String.concat("_", List.rev(rev_split_by_naming_convention(s)));
  var s$2 = Caml_bytes.bytes_to_string(Bytes.lowercase(Caml_bytes.bytes_of_string(s$1)));
  return Caml_bytes.bytes_to_string(Bytes.capitalize(Caml_bytes.bytes_of_string(s$2)));
}

function label_name_of_field_name(s) {
  var s$1 = $$String.concat("_", List.rev(rev_split_by_naming_convention(s)));
  return fix_ocaml_keyword_conflict(Caml_bytes.bytes_to_string(Bytes.lowercase(Caml_bytes.bytes_of_string(s$1))));
}

function module_of_file_name(file_name) {
  var file_name$1 = Curry._1(Filename.basename, file_name);
  var dot_index;
  try {
    dot_index = $$String.rindex(file_name$1, /* '.' */46);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Invalid_file_name */6,
              _0: file_name$1
            },
            Error: new Error()
          };
    }
    throw exn;
  }
  return constructor_name($$String.sub(file_name$1, 0, dot_index) + "_pb");
}

function type_name(message_scope, name) {
  var all_names = Pervasives.$at(message_scope, {
        hd: name,
        tl: /* [] */0
      });
  var all_names$1 = List.map((function (s) {
          return List.map($$String.lowercase, List.rev(rev_split_by_naming_convention(s)));
        }), all_names);
  var all_names$2 = List.flatten(all_names$1);
  if (all_names$2) {
    if (all_names$2.tl) {
      return $$String.concat("_", all_names$2);
    } else {
      return fix_ocaml_keyword_conflict(all_names$2.hd);
    }
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Programmatic error",
        Error: new Error()
      };
}

function encoding_info_of_field_type(all_types, field_type) {
  if (typeof field_type === "number") {
    switch (field_type) {
      case /* Field_type_sint32 */6 :
      case /* Field_type_sint64 */7 :
          return /* Pk_varint */{
                  _0: true
                };
      case /* Field_type_float */1 :
      case /* Field_type_fixed32 */8 :
      case /* Field_type_sfixed32 */10 :
          return /* Pk_bits32 */0;
      case /* Field_type_double */0 :
      case /* Field_type_fixed64 */9 :
      case /* Field_type_sfixed64 */11 :
          return /* Pk_bits64 */1;
      case /* Field_type_int32 */2 :
      case /* Field_type_int64 */3 :
      case /* Field_type_uint32 */4 :
      case /* Field_type_uint64 */5 :
      case /* Field_type_bool */12 :
          return /* Pk_varint */{
                  _0: false
                };
      case /* Field_type_string */13 :
      case /* Field_type_bytes */14 :
          return /* Pk_bytes */2;
      
    }
  } else {
    var match = type_of_id(all_types, field_type._0);
    if (match.spec.TAG === /* Enum */0) {
      return /* Pk_varint */{
              _0: false
            };
    } else {
      return /* Pk_bytes */2;
    }
  }
}

function encoding_of_field(all_types, field) {
  var match = field_option(field, "packed");
  var packed;
  if (match !== undefined) {
    if (match.TAG === /* Constant_bool */1) {
      packed = match._0;
    } else {
      var field_name$1 = field_name(field);
      throw {
            RE_EXN_ID: Compilation_error,
            _1: {
              TAG: /* Invalid_packed_option */8,
              _0: field_name$1
            },
            Error: new Error()
          };
    }
  } else {
    packed = false;
  }
  var pk = encoding_info_of_field_type(all_types, field_type(field));
  return [
          pk,
          field_number(field),
          packed,
          field_default(field)
        ];
}

function compile_field_type(field_name, all_types, file_options, field_options, file_name, field_type) {
  var match = find_field_option(field_options, "ocaml_type");
  var ocaml_type = match !== undefined && match.TAG === /* Constant_litteral */4 && match._0 === "int_t" ? "Int_t" : "None";
  var match$1 = file_option(file_options, "int32_type");
  var int32_type = match$1 !== undefined && match$1.TAG === /* Constant_litteral */4 && match$1._0 === "int_t" ? ({
        TAG: /* Ft_basic_type */0,
        _0: /* Bt_int */2
      }) : ({
        TAG: /* Ft_basic_type */0,
        _0: /* Bt_int32 */3
      });
  var match$2 = file_option(file_options, "int64_type");
  var int64_type = match$2 !== undefined && match$2.TAG === /* Constant_litteral */4 && match$2._0 === "int_t" ? ({
        TAG: /* Ft_basic_type */0,
        _0: /* Bt_int */2
      }) : ({
        TAG: /* Ft_basic_type */0,
        _0: /* Bt_int64 */4
      });
  if (typeof field_type !== "number") {
    var i = field_type._0;
    var module_ = module_of_file_name(file_name);
    var t;
    try {
      t = type_of_id(all_types, i);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        throw {
              RE_EXN_ID: Compilation_error,
              _1: {
                TAG: /* Programatic_error */4,
                _0: /* No_type_found_for_id */2
              },
              Error: new Error()
            };
      }
      throw exn;
    }
    if (is_empty_message(t)) {
      return /* Ft_unit */0;
    }
    var udt_nested;
    udt_nested = t.spec.TAG === /* Enum */0 ? false : true;
    var field_type_module = module_of_file_name(t.file_name);
    var match$3 = type_scope_of_type(t);
    var udt_type_name = type_name(match$3.message_names, type_name_of_type(t));
    if (field_type_module === module_) {
      return {
              TAG: /* Ft_user_defined_type */1,
              _0: {
                udt_module: undefined,
                udt_type_name: udt_type_name,
                udt_nested: udt_nested
              }
            };
    } else {
      return {
              TAG: /* Ft_user_defined_type */1,
              _0: {
                udt_module: field_type_module,
                udt_type_name: udt_type_name,
                udt_nested: udt_nested
              }
            };
    }
  }
  switch (field_type) {
    case /* Field_type_double */0 :
    case /* Field_type_float */1 :
        return {
                TAG: /* Ft_basic_type */0,
                _0: /* Bt_float */1
              };
    case /* Field_type_int32 */2 :
    case /* Field_type_uint32 */4 :
    case /* Field_type_sint32 */6 :
    case /* Field_type_fixed32 */8 :
        if (ocaml_type === "Int_t") {
          return {
                  TAG: /* Ft_basic_type */0,
                  _0: /* Bt_int */2
                };
        } else {
          return int32_type;
        }
    case /* Field_type_int64 */3 :
    case /* Field_type_uint64 */5 :
    case /* Field_type_sint64 */7 :
    case /* Field_type_fixed64 */9 :
        if (ocaml_type === "Int_t") {
          return {
                  TAG: /* Ft_basic_type */0,
                  _0: /* Bt_int */2
                };
        } else {
          return int64_type;
        }
    case /* Field_type_sfixed32 */10 :
        return unsupported_field_type(field_name, "sfixed32", "OCaml", undefined);
    case /* Field_type_sfixed64 */11 :
        return unsupported_field_type(field_name, "sfixed64", "OCaml", undefined);
    case /* Field_type_bool */12 :
        return {
                TAG: /* Ft_basic_type */0,
                _0: /* Bt_bool */6
              };
    case /* Field_type_string */13 :
        return {
                TAG: /* Ft_basic_type */0,
                _0: /* Bt_string */0
              };
    case /* Field_type_bytes */14 :
        return {
                TAG: /* Ft_basic_type */0,
                _0: /* Bt_bytes */5
              };
    
  }
}

function is_mutable(field_name, field_options) {
  var match = find_field_option(field_options, "ocaml_mutable");
  if (match === undefined) {
    return false;
  }
  if (match.TAG === /* Constant_bool */1) {
    return match._0;
  }
  throw {
        RE_EXN_ID: Compilation_error,
        _1: {
          TAG: /* Invalid_mutable_option */11,
          _0: field_name
        },
        Error: new Error()
      };
}

function ocaml_container(field_options) {
  var match = find_field_option(field_options, "ocaml_container");
  if (match !== undefined && match.TAG === /* Constant_litteral */4) {
    return match._0;
  }
  
}

function variant_of_oneof(include_oneof_name, outer_message_names, all_types, file_options, file_name, oneof_field) {
  var v_constructors = List.map((function (field) {
          var pbtt_field_type = field_type(field);
          var field_type$1 = compile_field_type(field_name(field), all_types, file_options, field_options(field), file_name, pbtt_field_type);
          var match = encoding_of_field(all_types, field);
          var vc_constructor = constructor_name(field_name(field));
          return {
                  vc_constructor: vc_constructor,
                  vc_field_type: typeof field_type$1 === "number" ? /* Vct_nullary */0 : /* Vct_non_nullary_constructor */({
                        _0: field_type$1
                      }),
                  vc_encoding_number: match[1],
                  vc_payload_kind: match[0]
                };
        }), oneof_field.oneof_fields);
  var v_name = include_oneof_name !== undefined ? type_name(outer_message_names, oneof_field.oneof_name) : type_name(outer_message_names, "");
  return {
          v_name: v_name,
          v_constructors: v_constructors
        };
}

function compile_enum(file_name, scope, param) {
  var module_ = module_of_file_name(file_name);
  var cv_constructors = List.map((function (param) {
          return [
                  constructor_name(param.enum_value_name),
                  param.enum_value_int
                ];
        }), param.enum_values);
  return {
          module_: module_,
          spec: {
            TAG: /* Const_variant */2,
            _0: {
              cv_name: type_name(scope.message_names, param.enum_name),
              cv_constructors: cv_constructors
            }
          }
        };
}

var all_code_gen_1 = {
  hd: Codegen_default,
  tl: {
    hd: Codegen_decode,
    tl: {
      hd: Codegen_encode,
      tl: {
        hd: Codegen_pp,
        tl: /* [] */0
      }
    }
  }
};

var all_code_gen = {
  hd: Codegen_type,
  tl: all_code_gen_1
};

function compile(proto_definition) {
  var lexbuf = Lexing.from_string(proto_definition);
  var proto;
  try {
    proto = proto_(lexer, lexbuf);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    throw add_loc(from_lexbuf(lexbuf), exn);
  }
  var all_pbtt_msgs = compile_proto_p1("tmp.proto", proto);
  var all_pbtt_msgs$1 = List.map((function (param) {
          var m = param.spec;
          var file_options = param.file_options;
          var file_name = param.file_name;
          var id = param.id;
          var scope = param.scope;
          if (m.TAG === /* Enum */0) {
            return {
                    scope: scope,
                    id: id,
                    file_name: file_name,
                    file_options: file_options,
                    spec: m
                  };
          } else {
            return {
                    scope: scope,
                    id: id,
                    file_name: file_name,
                    file_options: file_options,
                    spec: {
                      TAG: /* Message */1,
                      _0: compile_message_p2(all_pbtt_msgs, scope, m._0)
                    }
                  };
          }
        }), all_pbtt_msgs);
  var grouped_pbtt_msgs = List.rev(group(all_pbtt_msgs$1));
  var grouped_ocaml_types = List.map((function (pbtt_msgs) {
          return List.map((function (pbtt_msg) {
                        var m = pbtt_msg.spec;
                        var file_name = pbtt_msg.file_name;
                        var scope = pbtt_msg.scope;
                        if (m.TAG === /* Enum */0) {
                          return {
                                  hd: compile_enum(file_name, scope, m._0),
                                  tl: /* [] */0
                                };
                        } else {
                          var file_options = pbtt_msg.file_options;
                          var message = m._0;
                          var module_ = module_of_file_name(file_name);
                          var message_names = scope.message_names;
                          var message_body = message.message_body;
                          var message_name = message.message_name;
                          if (!message_body) {
                            return /* [] */0;
                          }
                          var f = message_body.hd;
                          switch (f.TAG | 0) {
                            case /* Message_oneof_field */1 :
                                if (!message_body.tl) {
                                  var outer_message_names = Pervasives.$at(message_names, {
                                        hd: message_name,
                                        tl: /* [] */0
                                      });
                                  var variant = variant_of_oneof(undefined, outer_message_names, all_pbtt_msgs$1, file_options, file_name, f._0);
                                  return {
                                          hd: {
                                            module_: module_,
                                            spec: {
                                              TAG: /* Variant */1,
                                              _0: variant
                                            }
                                          },
                                          tl: /* [] */0
                                        };
                                }
                                break;
                            case /* Message_field */0 :
                            case /* Message_map_field */2 :
                                break;
                            
                          }
                          var match = List.fold_left((function (param, field) {
                                  var fields = param[1];
                                  var variants = param[0];
                                  switch (field.TAG | 0) {
                                    case /* Message_field */0 :
                                        var field$1 = field._0;
                                        var match = encoding_of_field(all_pbtt_msgs$1, field$1);
                                        var encoding_number = match[1];
                                        var pk = match[0];
                                        var field_name$1 = field_name(field$1);
                                        var field_options$1 = field_options(field$1);
                                        var field_type$1 = compile_field_type(field_name$1, all_pbtt_msgs$1, file_options, field_options$1, file_name, field_type(field$1));
                                        var field_default$1 = field_default(field$1);
                                        var mutable_ = is_mutable(field_name$1, field_options$1);
                                        var match$1 = field_label(field$1);
                                        var record_field_type;
                                        switch (match$1) {
                                          case "Repeated" :
                                              var match$2 = ocaml_container(field_options$1);
                                              var repeated_type;
                                              if (match$2 !== undefined) {
                                                if (match$2 === "repeated_field") {
                                                  repeated_type = /* Rt_repeated_field */1;
                                                } else {
                                                  throw {
                                                        RE_EXN_ID: "Failure",
                                                        _1: "Invalid ocaml_container attribute value",
                                                        Error: new Error()
                                                      };
                                                }
                                              } else {
                                                repeated_type = /* Rt_list */0;
                                              }
                                              record_field_type = {
                                                TAG: /* Rft_repeated_field */2,
                                                _0: [
                                                  repeated_type,
                                                  field_type$1,
                                                  encoding_number,
                                                  pk,
                                                  match[2]
                                                ]
                                              };
                                              break;
                                          case "Optional" :
                                              record_field_type = {
                                                TAG: /* Rft_optional */1,
                                                _0: [
                                                  field_type$1,
                                                  encoding_number,
                                                  pk,
                                                  field_default$1
                                                ]
                                              };
                                              break;
                                          case "Required" :
                                              record_field_type = {
                                                TAG: /* Rft_required */0,
                                                _0: [
                                                  field_type$1,
                                                  encoding_number,
                                                  pk,
                                                  field_default$1
                                                ]
                                              };
                                              break;
                                          
                                        }
                                        var record_field_rf_label = label_name_of_field_name(field_name$1);
                                        var record_field = {
                                          rf_label: record_field_rf_label,
                                          rf_field_type: record_field_type,
                                          rf_mutable: mutable_
                                        };
                                        return [
                                                variants,
                                                {
                                                  hd: record_field,
                                                  tl: fields
                                                }
                                              ];
                                    case /* Message_oneof_field */1 :
                                        var field$2 = field._0;
                                        var outer_message_names = Pervasives.$at(message_names, {
                                              hd: message_name,
                                              tl: /* [] */0
                                            });
                                        var variant = variant_of_oneof(Caml_option.some(undefined), outer_message_names, all_pbtt_msgs$1, file_options, file_name, field$2);
                                        var record_field_rf_label$1 = label_name_of_field_name(field$2.oneof_name);
                                        var record_field_rf_field_type = {
                                          TAG: /* Rft_variant_field */4,
                                          _0: variant
                                        };
                                        var record_field$1 = {
                                          rf_label: record_field_rf_label$1,
                                          rf_field_type: record_field_rf_field_type,
                                          rf_mutable: false
                                        };
                                        var variants_0 = {
                                          module_: module_,
                                          spec: {
                                            TAG: /* Variant */1,
                                            _0: variant
                                          }
                                        };
                                        var variants$1 = {
                                          hd: variants_0,
                                          tl: variants
                                        };
                                        var fields$1 = {
                                          hd: record_field$1,
                                          tl: fields
                                        };
                                        return [
                                                variants$1,
                                                fields$1
                                              ];
                                    case /* Message_map_field */2 :
                                        var mf = field._0;
                                        var map_options = mf.map_options;
                                        var map_value_type = mf.map_value_type;
                                        var map_key_type = mf.map_key_type;
                                        var map_name = mf.map_name;
                                        var key_type = compile_field_type(Curry._1(Printf.sprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* String_literal */11,
                                                        _0: "key of ",
                                                        _1: {
                                                          TAG: /* String */2,
                                                          _0: /* No_padding */0,
                                                          _1: /* End_of_format */0
                                                        }
                                                      },
                                                      _1: "key of %s"
                                                    }), map_name), all_pbtt_msgs$1, file_options, map_options, file_name, map_key_type);
                                        var key_pk = encoding_info_of_field_type(all_pbtt_msgs$1, map_key_type);
                                        var key_type$1;
                                        if (typeof key_type === "number") {
                                          throw {
                                                RE_EXN_ID: "Failure",
                                                _1: "Only Basic Types are supported for map keys",
                                                Error: new Error()
                                              };
                                        }
                                        if (key_type.TAG === /* Ft_basic_type */0) {
                                          key_type$1 = key_type._0;
                                        } else {
                                          throw {
                                                RE_EXN_ID: "Failure",
                                                _1: "Only Basic Types are supported for map keys",
                                                Error: new Error()
                                              };
                                        }
                                        var value_type = compile_field_type(Curry._1(Printf.sprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* String_literal */11,
                                                        _0: "value of ",
                                                        _1: {
                                                          TAG: /* String */2,
                                                          _0: /* No_padding */0,
                                                          _1: /* End_of_format */0
                                                        }
                                                      },
                                                      _1: "value of %s"
                                                    }), map_name), all_pbtt_msgs$1, file_options, map_options, file_name, map_value_type);
                                        var value_pk = encoding_info_of_field_type(all_pbtt_msgs$1, map_value_type);
                                        var match$3 = ocaml_container(map_options);
                                        var associative_type;
                                        if (match$3 !== undefined) {
                                          if (match$3 === "hashtbl") {
                                            associative_type = /* At_hashtable */1;
                                          } else {
                                            throw {
                                                  RE_EXN_ID: "Failure",
                                                  _1: "Invalid ocaml_container attribute value for map",
                                                  Error: new Error()
                                                };
                                          }
                                        } else {
                                          associative_type = /* At_list */0;
                                        }
                                        var record_field_type$1 = {
                                          TAG: /* Rft_associative_field */3,
                                          _0: [
                                            associative_type,
                                            mf.map_number,
                                            [
                                              key_type$1,
                                              key_pk
                                            ],
                                            [
                                              value_type,
                                              value_pk
                                            ]
                                          ]
                                        };
                                        var record_field_rf_label$2 = label_name_of_field_name(map_name);
                                        var record_field_rf_mutable = is_mutable(map_name, map_options);
                                        var record_field$2 = {
                                          rf_label: record_field_rf_label$2,
                                          rf_field_type: record_field_type$1,
                                          rf_mutable: record_field_rf_mutable
                                        };
                                        return [
                                                variants,
                                                {
                                                  hd: record_field$2,
                                                  tl: fields
                                                }
                                              ];
                                    
                                  }
                                }), [
                                /* [] */0,
                                /* [] */0
                              ], message_body);
                          var record_r_name = type_name(message_names, message_name);
                          var record_r_fields = List.rev(match[1]);
                          var record = {
                            r_name: record_r_name,
                            r_fields: record_r_fields
                          };
                          var type__spec = {
                            TAG: /* Record */0,
                            _0: record
                          };
                          var type_ = {
                            module_: module_,
                            spec: type__spec
                          };
                          return List.rev({
                                      hd: type_,
                                      tl: match[0]
                                    });
                        }
                      }), pbtt_msgs);
        }), grouped_pbtt_msgs);
  var all_ocaml_types = List.flatten(grouped_ocaml_types);
  var proto_file_name = "tmp.proto";
  var gen = function (otypes, sc, fs) {
    return List.iter((function (param) {
                  var ocamldoc_title = param[1];
                  var f = param[0];
                  if (ocamldoc_title !== undefined) {
                    line$1(sc, "");
                    line$1(sc, Curry._1(Printf.sprintf(/* Format */{
                                  _0: {
                                    TAG: /* String_literal */11,
                                    _0: "(** {2 ",
                                    _1: {
                                      TAG: /* String */2,
                                      _0: /* No_padding */0,
                                      _1: {
                                        TAG: /* String_literal */11,
                                        _0: "} *)",
                                        _1: /* End_of_format */0
                                      }
                                    }
                                  },
                                  _1: "(** {2 %s} *)"
                                }), ocamldoc_title));
                    line$1(sc, "");
                  }
                  return List.iter((function (types) {
                                List.fold_left((function (first, type_) {
                                        var has_encoded = first ? Curry._3(f, undefined, type_, sc) : Curry._3(f, Caml_option.some(undefined), type_, sc);
                                        line$1(sc, "");
                                        if (first) {
                                          return !has_encoded;
                                        } else {
                                          return false;
                                        }
                                      }), true, types);
                                
                              }), otypes);
                }), fs);
  };
  var sc = {
    items: /* [] */0
  };
  line$1(sc, "[@@@ocaml.warning \"-30\"]");
  line$1(sc, "");
  gen(all_ocaml_types, sc, List.map((function (m) {
              return [
                      m.gen_struct,
                      undefined
                    ];
            }), all_code_gen));
  var struct_string = print(sc);
  var sc$1 = {
    items: /* [] */0
  };
  line$1(sc$1, Curry._1(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "(** ",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " Generated Types and Encoding *)",
                      _1: /* End_of_format */0
                    }
                  }
                },
                _1: "(** %s Generated Types and Encoding *)"
              }), Curry._1(Filename.basename, proto_file_name)));
  gen(all_ocaml_types, sc$1, List.map((function (m) {
              return [
                      m.gen_sig,
                      m.ocamldoc_title
                    ];
            }), all_code_gen));
  var sig_string = print(sc$1);
  return [
          sig_string,
          struct_string
        ];
}

var match = compile("message T {required int32 j = 1; }");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

eq("File \"ocaml_protc_test.ml\", line 10, characters 5-12", match[0], "(** tmp.proto Generated Types and Encoding *)\n\n(** {2 Types} *)\n\ntype t = {\n  j : int32;\n}\n\n\n(** {2 Default values} *)\n\nval default_t : \n  ?j:int32 ->\n  unit ->\n  t\n(** [default_t ()] is the default value for type [t] *)\n\n\n(** {2 Protobuf Decoding} *)\n\nval decode_t : Pbrt.Decoder.t -> t\n(** [decode_t decoder] decodes a [t] value from [decoder] *)\n\n\n(** {2 Protobuf Toding} *)\n\nval encode_t : t -> Pbrt.Encoder.t -> unit\n(** [encode_t v encoder] encodes [v] with the given [encoder] *)\n\n\n(** {2 Formatters} *)\n\nval pp_t : Format.formatter -> t -> unit \n(** [pp_t v] formats v] *)\n");

eq("File \"ocaml_protc_test.ml\", line 46, characters 5-12", match[1], "[@@@ocaml.warning \"-30\"]\n\ntype t = {\n  j : int32;\n}\n\nand t_mutable = {\n  mutable j : int32;\n}\n\nlet rec default_t \n  ?j:((j:int32) = 0l)\n  () : t  = {\n  j;\n}\n\nand default_t_mutable () : t_mutable = {\n  j = 0l;\n}\n\nlet rec decode_t d =\n  let v = default_t_mutable () in\n  let rec loop () = \n    match Pbrt.Decoder.key d with\n    | None -> (\n    )\n    | Some (1, Pbrt.Varint) -> (\n      v.j <- Pbrt.Decoder.int32_as_varint d;\n      loop ()\n    )\n    | Some (1, pk) -> raise (\n      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload (\"Message(t), field(1)\", pk))\n    )\n    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()\n  in\n  loop ();\n  let v:t = Obj.magic v in\n  v\n\nlet rec encode_t (v:t) encoder = \n  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; \n  Pbrt.Encoder.int32_as_varint v.j encoder;\n  ()\n\nlet rec pp_t fmt (v:t) = \n  let pp_i fmt () =\n    Format.pp_open_vbox fmt 1;\n    Pbrt.Pp.pp_record_field \"j\" Pbrt.Pp.pp_int32 fmt v.j;\n    Format.pp_close_box fmt ()\n  in\n  Pbrt.Pp.pp_brk pp_i fmt ()\n");

Mt.from_pair_suites("Ocaml_proto_test", suites.contents);

/*  Not a pure module */
