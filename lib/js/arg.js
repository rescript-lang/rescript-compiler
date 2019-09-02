'use strict';

var Sys = require("./sys.js");
var List = require("./list.js");
var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var Bad = Caml_exceptions.create("Arg.Bad");

var Help = Caml_exceptions.create("Arg.Help");

var Stop = Caml_exceptions.create("Arg.Stop");

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l !== "[]") {
      var match = l.Arg0;
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      } else {
        _l = l.Arg1;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function make_symlist(prefix, sep, suffix, l) {
  if (l !== "[]") {
    return List.fold_left((function (x, y) {
                  return x + (sep + y);
                }), prefix + l.Arg0, l.Arg1) + suffix;
  } else {
    return "<none>";
  }
}

function help_action(param) {
  throw [
        Stop,
        /* constructor */{
          tag: "Unknown",
          Arg0: "-help"
        }
      ];
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = "[]";
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      add1 = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "-help",
          /* constructor */{
            tag: "Unit",
            Arg0: help_action
          },
          " Display this list of options"
        ],
        Arg1: "[]"
      };
    } else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = "[]";
  }
  catch (exn$1){
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      add2 = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "--help",
          /* constructor */{
            tag: "Unit",
            Arg0: help_action
          },
          " Display this list of options"
        ],
        Arg1: "[]"
      };
    } else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

function usage_b(buf, speclist, errmsg) {
  Curry._1(Printf.bprintf(buf, /* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String",
              Arg0: "No_padding",
              Arg1: /* constructor */{
                tag: "Char_literal",
                Arg0: /* "\n" */10,
                Arg1: "End_of_format"
              }
            },
            Arg1: "%s\n"
          }), errmsg);
  return List.iter((function (param) {
                var buf$1 = buf;
                var param$1 = param;
                var doc = param$1[2];
                if (doc.length !== 0) {
                  var spec = param$1[1];
                  var key = param$1[0];
                  if (/* XXX */spec.tag === "Symbol") {
                    return Curry._3(Printf.bprintf(buf$1, /* constructor */{
                                    tag: "Format",
                                    Arg0: /* constructor */{
                                      tag: "String_literal",
                                      Arg0: "  ",
                                      Arg1: /* constructor */{
                                        tag: "String",
                                        Arg0: "No_padding",
                                        Arg1: /* constructor */{
                                          tag: "Char_literal",
                                          Arg0: /* " " */32,
                                          Arg1: /* constructor */{
                                            tag: "String",
                                            Arg0: "No_padding",
                                            Arg1: /* constructor */{
                                              tag: "String",
                                              Arg0: "No_padding",
                                              Arg1: /* constructor */{
                                                tag: "Char_literal",
                                                Arg0: /* "\n" */10,
                                                Arg1: "End_of_format"
                                              }
                                            }
                                          }
                                        }
                                      }
                                    },
                                    Arg1: "  %s %s%s\n"
                                  }), key, make_symlist("{", "|", "}", spec.Arg0), doc);
                  } else {
                    return Curry._2(Printf.bprintf(buf$1, /* constructor */{
                                    tag: "Format",
                                    Arg0: /* constructor */{
                                      tag: "String_literal",
                                      Arg0: "  ",
                                      Arg1: /* constructor */{
                                        tag: "String",
                                        Arg0: "No_padding",
                                        Arg1: /* constructor */{
                                          tag: "Char_literal",
                                          Arg0: /* " " */32,
                                          Arg1: /* constructor */{
                                            tag: "String",
                                            Arg0: "No_padding",
                                            Arg1: /* constructor */{
                                              tag: "Char_literal",
                                              Arg0: /* "\n" */10,
                                              Arg1: "End_of_format"
                                            }
                                          }
                                        }
                                      }
                                    },
                                    Arg1: "  %s %s\n"
                                  }), key, doc);
                  }
                } else {
                  return 0;
                }
              }), add_help(speclist));
}

function usage_string(speclist, errmsg) {
  var b = $$Buffer.create(200);
  usage_b(b, speclist, errmsg);
  return $$Buffer.contents(b);
}

function usage(speclist, errmsg) {
  return Curry._1(Printf.eprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  },
                  Arg1: "%s"
                }), usage_string(speclist, errmsg));
}

var current = /* record */[/* contents */0];

function parse_argv_dynamic($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star !== undefined ? $staropt$star : current;
  var l = argv.length;
  var b = $$Buffer.create(200);
  var initpos = current$1[0];
  var stop = function (error) {
    var progname = initpos < l ? Caml_array.caml_array_get(argv, initpos) : "(?)";
    switch (/* XXX */error.tag) {
      case "Unknown" :
          var s = error.Arg0;
          switch (s) {
            case "--help" :
            case "-help" :
                break;
            default:
              Curry._2(Printf.bprintf(b, /* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: ": unknown option '",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "'.\n",
                                Arg1: "End_of_format"
                              }
                            }
                          }
                        },
                        Arg1: "%s: unknown option '%s'.\n"
                      }), progname, s);
          }
          break;
      case "Wrong" :
          Curry._4(Printf.bprintf(b, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ": wrong argument '",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: "'; option '",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "' expects ",
                                Arg1: /* constructor */{
                                  tag: "String",
                                  Arg0: "No_padding",
                                  Arg1: /* constructor */{
                                    tag: "String_literal",
                                    Arg0: ".\n",
                                    Arg1: "End_of_format"
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    },
                    Arg1: "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  }), progname, error.Arg1, error.Arg0, error.Arg2);
          break;
      case "Missing" :
          Curry._2(Printf.bprintf(b, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ": option '",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: "' needs an argument.\n",
                            Arg1: "End_of_format"
                          }
                        }
                      }
                    },
                    Arg1: "%s: option '%s' needs an argument.\n"
                  }), progname, error.Arg0);
          break;
      case "Message" :
          Curry._2(Printf.bprintf(b, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ": ",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "String_literal",
                            Arg0: ".\n",
                            Arg1: "End_of_format"
                          }
                        }
                      }
                    },
                    Arg1: "%s: %s.\n"
                  }), progname, error.Arg0);
          break;
      
    }
    usage_b(b, speclist[0], errmsg);
    if (Caml_obj.caml_equal(error, /* constructor */{
            tag: "Unknown",
            Arg0: "-help"
          }) || Caml_obj.caml_equal(error, /* constructor */{
            tag: "Unknown",
            Arg0: "--help"
          })) {
      throw [
            Help,
            $$Buffer.contents(b)
          ];
    }
    throw [
          Bad,
          $$Buffer.contents(b)
        ];
  };
  current$1[0] = current$1[0] + 1 | 0;
  while(current$1[0] < l) {
    var s = Caml_array.caml_array_get(argv, current$1[0]);
    if (s.length >= 1 && Caml_string.get(s, 0) === /* "-" */45) {
      var action;
      try {
        action = assoc3(s, speclist[0]);
      }
      catch (exn){
        if (exn === Caml_builtin_exceptions.not_found) {
          action = stop(/* constructor */{
                tag: "Unknown",
                Arg0: s
              });
        } else {
          throw exn;
        }
      }
      try {
        var treat_action = (function(s){
        return function treat_action(param) {
          switch (/* XXX */param.tag) {
            case "Unit" :
                return Curry._1(param.Arg0, /* () */0);
            case "Bool" :
                if ((current$1[0] + 1 | 0) < l) {
                  var arg = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param.Arg0, Pervasives.bool_of_string(arg));
                  }
                  catch (raw_exn){
                    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
                      if (exn[1] === "bool_of_string") {
                        throw [
                              Stop,
                              /* constructor */{
                                tag: "Wrong",
                                Arg0: s,
                                Arg1: arg,
                                Arg2: "a boolean"
                              }
                            ];
                      } else {
                        throw exn;
                      }
                    } else {
                      throw exn;
                    }
                  }
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Set" :
                param.Arg0[0] = true;
                return /* () */0;
            case "Clear" :
                param.Arg0[0] = false;
                return /* () */0;
            case "String" :
                if ((current$1[0] + 1 | 0) < l) {
                  Curry._1(param.Arg0, Caml_array.caml_array_get(argv, current$1[0] + 1 | 0));
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Set_string" :
                if ((current$1[0] + 1 | 0) < l) {
                  param.Arg0[0] = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Int" :
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$1 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param.Arg0, Caml_format.caml_int_of_string(arg$1));
                  }
                  catch (raw_exn$1){
                    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                    if (exn$1[0] === Caml_builtin_exceptions.failure) {
                      if (exn$1[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* constructor */{
                                tag: "Wrong",
                                Arg0: s,
                                Arg1: arg$1,
                                Arg2: "an integer"
                              }
                            ];
                      } else {
                        throw exn$1;
                      }
                    } else {
                      throw exn$1;
                    }
                  }
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Set_int" :
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$2 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    param.Arg0[0] = Caml_format.caml_int_of_string(arg$2);
                  }
                  catch (raw_exn$2){
                    var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
                    if (exn$2[0] === Caml_builtin_exceptions.failure) {
                      if (exn$2[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* constructor */{
                                tag: "Wrong",
                                Arg0: s,
                                Arg1: arg$2,
                                Arg2: "an integer"
                              }
                            ];
                      } else {
                        throw exn$2;
                      }
                    } else {
                      throw exn$2;
                    }
                  }
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Float" :
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$3 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param.Arg0, Caml_format.caml_float_of_string(arg$3));
                  }
                  catch (raw_exn$3){
                    var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
                    if (exn$3[0] === Caml_builtin_exceptions.failure) {
                      if (exn$3[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* constructor */{
                                tag: "Wrong",
                                Arg0: s,
                                Arg1: arg$3,
                                Arg2: "a float"
                              }
                            ];
                      } else {
                        throw exn$3;
                      }
                    } else {
                      throw exn$3;
                    }
                  }
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Set_float" :
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$4 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    param.Arg0[0] = Caml_format.caml_float_of_string(arg$4);
                  }
                  catch (raw_exn$4){
                    var exn$4 = Caml_js_exceptions.internalToOCamlException(raw_exn$4);
                    if (exn$4[0] === Caml_builtin_exceptions.failure) {
                      if (exn$4[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* constructor */{
                                tag: "Wrong",
                                Arg0: s,
                                Arg1: arg$4,
                                Arg2: "a float"
                              }
                            ];
                      } else {
                        throw exn$4;
                      }
                    } else {
                      throw exn$4;
                    }
                  }
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Tuple" :
                return List.iter(treat_action, param.Arg0);
            case "Symbol" :
                if ((current$1[0] + 1 | 0) < l) {
                  var symb = param.Arg0;
                  var arg$5 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  if (List.mem(arg$5, symb)) {
                    Curry._1(param.Arg1, Caml_array.caml_array_get(argv, current$1[0] + 1 | 0));
                    current$1[0] = current$1[0] + 1 | 0;
                    return /* () */0;
                  } else {
                    throw [
                          Stop,
                          /* constructor */{
                            tag: "Wrong",
                            Arg0: s,
                            Arg1: arg$5,
                            Arg2: "one of: " + make_symlist("", " ", "", symb)
                          }
                        ];
                  }
                } else {
                  throw [
                        Stop,
                        /* constructor */{
                          tag: "Missing",
                          Arg0: s
                        }
                      ];
                }
            case "Rest" :
                var f = param.Arg0;
                while(current$1[0] < (l - 1 | 0)) {
                  Curry._1(f, Caml_array.caml_array_get(argv, current$1[0] + 1 | 0));
                  current$1[0] = current$1[0] + 1 | 0;
                };
                return /* () */0;
            
          }
        }
        }(s));
        treat_action(action);
      }
      catch (raw_exn){
        var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn$1[0] === Bad) {
          stop(/* constructor */{
                tag: "Message",
                Arg0: exn$1[1]
              });
        } else if (exn$1[0] === Stop) {
          stop(exn$1[1]);
        } else {
          throw exn$1;
        }
      }
      current$1[0] = current$1[0] + 1 | 0;
    } else {
      try {
        Curry._1(anonfun, s);
      }
      catch (raw_exn$1){
        var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
        if (exn$2[0] === Bad) {
          stop(/* constructor */{
                tag: "Message",
                Arg0: exn$2[1]
              });
        } else {
          throw exn$2;
        }
      }
      current$1[0] = current$1[0] + 1 | 0;
    }
  };
  return /* () */0;
}

function parse_argv($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star !== undefined ? $staropt$star : current;
  return parse_argv_dynamic(current$1, argv, /* record */[/* contents */speclist], anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(undefined, Sys.argv, l, f, msg);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Bad) {
      Curry._1(Printf.eprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: "End_of_format"
                },
                Arg1: "%s"
              }), exn[1]);
      return Pervasives.exit(2);
    } else if (exn[0] === Help) {
      Curry._1(Printf.printf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: "End_of_format"
                },
                Arg1: "%s"
              }), exn[1]);
      return Pervasives.exit(0);
    } else {
      throw exn;
    }
  }
}

function parse_dynamic(l, f, msg) {
  try {
    return parse_argv_dynamic(undefined, Sys.argv, l, f, msg);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Bad) {
      Curry._1(Printf.eprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: "End_of_format"
                },
                Arg1: "%s"
              }), exn[1]);
      return Pervasives.exit(2);
    } else if (exn[0] === Help) {
      Curry._1(Printf.printf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: "End_of_format"
                },
                Arg1: "%s"
              }), exn[1]);
      return Pervasives.exit(0);
    } else {
      throw exn;
    }
  }
}

function second_word(s) {
  var len = s.length;
  try {
    var _n = $$String.index(s, /* " " */32);
    while(true) {
      var n = _n;
      if (n >= len) {
        return len;
      } else if (Caml_string.get(s, n) === /* " " */32) {
        _n = n + 1 | 0;
        continue ;
      } else {
        return n;
      }
    };
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return len;
    } else {
      throw exn;
    }
  }
}

function max_arg_len(cur, param) {
  var kwd = param[0];
  if (/* XXX */param[1].tag === "Symbol") {
    return Caml_primitive.caml_int_max(cur, kwd.length);
  } else {
    return Caml_primitive.caml_int_max(cur, kwd.length + second_word(param[2]) | 0);
  }
}

function align($staropt$star, speclist) {
  var limit = $staropt$star !== undefined ? $staropt$star : Pervasives.max_int;
  var completed = add_help(speclist);
  var len = List.fold_left(max_arg_len, 0, completed);
  var len$1 = len < limit ? len : limit;
  return List.map((function (param) {
                var len$2 = len$1;
                var ksd = param;
                var spec = ksd[1];
                var kwd = ksd[0];
                if (ksd[2] === "") {
                  return ksd;
                } else if (/* XXX */spec.tag === "Symbol") {
                  var msg = ksd[2];
                  var cutcol = second_word(msg);
                  var n = Caml_primitive.caml_int_max(0, len$2 - cutcol | 0) + 3 | 0;
                  var spaces = Caml_bytes.bytes_to_string(Bytes.make(n, /* " " */32));
                  return /* tuple */[
                          kwd,
                          spec,
                          "\n" + (spaces + msg)
                        ];
                } else {
                  var msg$1 = ksd[2];
                  var cutcol$1 = second_word(msg$1);
                  var kwd_len = kwd.length;
                  var diff = (len$2 - kwd_len | 0) - cutcol$1 | 0;
                  if (diff <= 0) {
                    return /* tuple */[
                            kwd,
                            spec,
                            msg$1
                          ];
                  } else {
                    var spaces$1 = Caml_bytes.bytes_to_string(Bytes.make(diff, /* " " */32));
                    var prefix = $$String.sub(msg$1, 0, cutcol$1);
                    var suffix = $$String.sub(msg$1, cutcol$1, msg$1.length - cutcol$1 | 0);
                    return /* tuple */[
                            kwd,
                            spec,
                            prefix + (spaces$1 + suffix)
                          ];
                  }
                }
              }), completed);
}

exports.parse = parse;
exports.parse_dynamic = parse_dynamic;
exports.parse_argv = parse_argv;
exports.parse_argv_dynamic = parse_argv_dynamic;
exports.Help = Help;
exports.Bad = Bad;
exports.usage = usage;
exports.usage_string = usage_string;
exports.align = align;
exports.current = current;
/* No side effect */
