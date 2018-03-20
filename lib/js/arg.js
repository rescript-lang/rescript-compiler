'use strict';

var Sys = require("./sys.js");
var List = require("./list.js");
var Block = require("./block.js");
var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Js_exn = require("./js_exn.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var Bad = Caml_exceptions.create("Arg.Bad");

var Help = Caml_exceptions.create("Arg.Help");

var Stop = Caml_exceptions.create("Arg.Stop");

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[0];
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      } else {
        _l = l[1];
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function make_symlist(prefix, sep, suffix, l) {
  if (l) {
    return List.fold_left((function (x, y) {
                  return x + (sep + y);
                }), prefix + l[0], l[1]) + suffix;
  } else {
    return "<none>";
  }
}

function help_action() {
  throw [
        Stop,
        /* Unknown */Block.__(0, ["-help"])
      ];
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      add1 = /* :: */[
        /* tuple */[
          "-help",
          /* Unit */Block.__(0, [help_action]),
          " Display this list of options"
        ],
        /* [] */0
      ];
    } else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (exn$1){
    if (exn$1 === Caml_builtin_exceptions.not_found) {
      add2 = /* :: */[
        /* tuple */[
          "--help",
          /* Unit */Block.__(0, [help_action]),
          " Display this list of options"
        ],
        /* [] */0
      ];
    } else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

function usage_b(buf, speclist, errmsg) {
  Curry._1(Printf.bprintf(buf, /* Format */[
            /* String */Block.__(2, [
                /* No_padding */0,
                /* Char_literal */Block.__(12, [
                    /* "\n" */10,
                    /* End_of_format */0
                  ])
              ]),
            "%s\n"
          ]), errmsg);
  return List.iter((function (param) {
                var buf$1 = buf;
                var param$1 = param;
                var doc = param$1[2];
                if (doc.length) {
                  var spec = param$1[1];
                  var key = param$1[0];
                  if (spec.tag === 11) {
                    return Curry._3(Printf.bprintf(buf$1, /* Format */[
                                    /* String_literal */Block.__(11, [
                                        "  ",
                                        /* String */Block.__(2, [
                                            /* No_padding */0,
                                            /* Char_literal */Block.__(12, [
                                                /* " " */32,
                                                /* String */Block.__(2, [
                                                    /* No_padding */0,
                                                    /* String */Block.__(2, [
                                                        /* No_padding */0,
                                                        /* Char_literal */Block.__(12, [
                                                            /* "\n" */10,
                                                            /* End_of_format */0
                                                          ])
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ]),
                                    "  %s %s%s\n"
                                  ]), key, make_symlist("{", "|", "}", spec[0]), doc);
                  } else {
                    return Curry._2(Printf.bprintf(buf$1, /* Format */[
                                    /* String_literal */Block.__(11, [
                                        "  ",
                                        /* String */Block.__(2, [
                                            /* No_padding */0,
                                            /* Char_literal */Block.__(12, [
                                                /* " " */32,
                                                /* String */Block.__(2, [
                                                    /* No_padding */0,
                                                    /* Char_literal */Block.__(12, [
                                                        /* "\n" */10,
                                                        /* End_of_format */0
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ]),
                                    "  %s %s\n"
                                  ]), key, doc);
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
  return Curry._1(Printf.eprintf(/* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%s"
                ]), usage_string(speclist, errmsg));
}

var current = [0];

function parse_argv_dynamic($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star ? $staropt$star[0] : current;
  var l = argv.length;
  var b = $$Buffer.create(200);
  var initpos = current$1[0];
  var stop = function (error) {
    var progname = initpos < l ? Caml_array.caml_array_get(argv, initpos) : "(?)";
    switch (error.tag | 0) {
      case 0 : 
          var s = error[0];
          switch (s) {
            case "--help" : 
            case "-help" : 
                break;
            default:
              Curry._2(Printf.bprintf(b, /* Format */[
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* String_literal */Block.__(11, [
                                ": unknown option '",
                                /* String */Block.__(2, [
                                    /* No_padding */0,
                                    /* String_literal */Block.__(11, [
                                        "'.\n",
                                        /* End_of_format */0
                                      ])
                                  ])
                              ])
                          ]),
                        "%s: unknown option '%s'.\n"
                      ]), progname, s);
          }
          break;
      case 1 : 
          Curry._4(Printf.bprintf(b, /* Format */[
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            ": wrong argument '",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    "'; option '",
                                    /* String */Block.__(2, [
                                        /* No_padding */0,
                                        /* String_literal */Block.__(11, [
                                            "' expects ",
                                            /* String */Block.__(2, [
                                                /* No_padding */0,
                                                /* String_literal */Block.__(11, [
                                                    ".\n",
                                                    /* End_of_format */0
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ])
                              ])
                          ])
                      ]),
                    "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  ]), progname, error[1], error[0], error[2]);
          break;
      case 2 : 
          Curry._2(Printf.bprintf(b, /* Format */[
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            ": option '",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    "' needs an argument.\n",
                                    /* End_of_format */0
                                  ])
                              ])
                          ])
                      ]),
                    "%s: option '%s' needs an argument.\n"
                  ]), progname, error[0]);
          break;
      case 3 : 
          Curry._2(Printf.bprintf(b, /* Format */[
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            ": ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    ".\n",
                                    /* End_of_format */0
                                  ])
                              ])
                          ])
                      ]),
                    "%s: %s.\n"
                  ]), progname, error[0]);
          break;
      
    }
    usage_b(b, speclist[0], errmsg);
    if (Caml_obj.caml_equal(error, /* Unknown */Block.__(0, ["-help"])) || Caml_obj.caml_equal(error, /* Unknown */Block.__(0, ["--help"]))) {
      throw [
            Help,
            $$Buffer.contents(b)
          ];
    } else {
      throw [
            Bad,
            $$Buffer.contents(b)
          ];
    }
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
          action = stop(/* Unknown */Block.__(0, [s]));
        } else {
          throw exn;
        }
      }
      try {
        var treat_action = (function(s){
        return function treat_action(param) {
          switch (param.tag | 0) {
            case 0 : 
                return Curry._1(param[0], /* () */0);
            case 1 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var arg = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param[0], Pervasives.bool_of_string(arg));
                  }
                  catch (raw_exn){
                    var exn = Js_exn.internalToOCamlException(raw_exn);
                    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
                      if (exn[1] === "bool_of_string") {
                        throw [
                              Stop,
                              /* Wrong */Block.__(1, [
                                  s,
                                  arg,
                                  "a boolean"
                                ])
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
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 2 : 
                param[0][0] = true;
                return /* () */0;
            case 3 : 
                param[0][0] = false;
                return /* () */0;
            case 4 : 
                if ((current$1[0] + 1 | 0) < l) {
                  Curry._1(param[0], Caml_array.caml_array_get(argv, current$1[0] + 1 | 0));
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 5 : 
                if ((current$1[0] + 1 | 0) < l) {
                  param[0][0] = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  current$1[0] = current$1[0] + 1 | 0;
                  return /* () */0;
                } else {
                  throw [
                        Stop,
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 6 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$1 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param[0], Caml_format.caml_int_of_string(arg$1));
                  }
                  catch (raw_exn$1){
                    var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
                    if (exn$1[0] === Caml_builtin_exceptions.failure) {
                      if (exn$1[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* Wrong */Block.__(1, [
                                  s,
                                  arg$1,
                                  "an integer"
                                ])
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
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 7 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$2 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    param[0][0] = Caml_format.caml_int_of_string(arg$2);
                  }
                  catch (raw_exn$2){
                    var exn$2 = Js_exn.internalToOCamlException(raw_exn$2);
                    if (exn$2[0] === Caml_builtin_exceptions.failure) {
                      if (exn$2[1] === "int_of_string") {
                        throw [
                              Stop,
                              /* Wrong */Block.__(1, [
                                  s,
                                  arg$2,
                                  "an integer"
                                ])
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
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 8 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$3 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    Curry._1(param[0], Caml_format.caml_float_of_string(arg$3));
                  }
                  catch (raw_exn$3){
                    var exn$3 = Js_exn.internalToOCamlException(raw_exn$3);
                    if (exn$3[0] === Caml_builtin_exceptions.failure) {
                      if (exn$3[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* Wrong */Block.__(1, [
                                  s,
                                  arg$3,
                                  "a float"
                                ])
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
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 9 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var arg$4 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  try {
                    param[0][0] = Caml_format.caml_float_of_string(arg$4);
                  }
                  catch (raw_exn$4){
                    var exn$4 = Js_exn.internalToOCamlException(raw_exn$4);
                    if (exn$4[0] === Caml_builtin_exceptions.failure) {
                      if (exn$4[1] === "float_of_string") {
                        throw [
                              Stop,
                              /* Wrong */Block.__(1, [
                                  s,
                                  arg$4,
                                  "a float"
                                ])
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
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 10 : 
                return List.iter(treat_action, param[0]);
            case 11 : 
                if ((current$1[0] + 1 | 0) < l) {
                  var symb = param[0];
                  var arg$5 = Caml_array.caml_array_get(argv, current$1[0] + 1 | 0);
                  if (List.mem(arg$5, symb)) {
                    Curry._1(param[1], Caml_array.caml_array_get(argv, current$1[0] + 1 | 0));
                    current$1[0] = current$1[0] + 1 | 0;
                    return /* () */0;
                  } else {
                    throw [
                          Stop,
                          /* Wrong */Block.__(1, [
                              s,
                              arg$5,
                              "one of: " + make_symlist("", " ", "", symb)
                            ])
                        ];
                  }
                } else {
                  throw [
                        Stop,
                        /* Missing */Block.__(2, [s])
                      ];
                }
            case 12 : 
                var f = param[0];
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
        var exn$1 = Js_exn.internalToOCamlException(raw_exn);
        if (exn$1[0] === Bad) {
          stop(/* Message */Block.__(3, [exn$1[1]]));
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
        var exn$2 = Js_exn.internalToOCamlException(raw_exn$1);
        if (exn$2[0] === Bad) {
          stop(/* Message */Block.__(3, [exn$2[1]]));
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
  var current$1 = $staropt$star ? $staropt$star[0] : current;
  return parse_argv_dynamic(/* Some */[current$1], argv, [speclist], anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(/* None */0, Sys.argv, l, f, msg);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Bad) {
      Curry._1(Printf.eprintf(/* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* End_of_format */0
                  ]),
                "%s"
              ]), exn[1]);
      return Pervasives.exit(2);
    } else if (exn[0] === Help) {
      Curry._1(Printf.printf(/* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* End_of_format */0
                  ]),
                "%s"
              ]), exn[1]);
      return Pervasives.exit(0);
    } else {
      throw exn;
    }
  }
}

function parse_dynamic(l, f, msg) {
  try {
    return parse_argv_dynamic(/* None */0, Sys.argv, l, f, msg);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Bad) {
      Curry._1(Printf.eprintf(/* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* End_of_format */0
                  ]),
                "%s"
              ]), exn[1]);
      return Pervasives.exit(2);
    } else if (exn[0] === Help) {
      Curry._1(Printf.printf(/* Format */[
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* End_of_format */0
                  ]),
                "%s"
              ]), exn[1]);
      return Pervasives.exit(0);
    } else {
      throw exn;
    }
  }
}

function second_word(s) {
  var len = s.length;
  try {
    var _n = Bytes.index(Caml_string.bytes_of_string(s), /* " " */32);
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
  if (param[1].tag === 11) {
    return Caml_primitive.caml_int_max(cur, kwd.length);
  } else {
    return Caml_primitive.caml_int_max(cur, kwd.length + second_word(param[2]) | 0);
  }
}

function align($staropt$star, speclist) {
  var limit = $staropt$star ? $staropt$star[0] : Pervasives.max_int;
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
                } else if (spec.tag === 11) {
                  var msg = ksd[2];
                  var cutcol = second_word(msg);
                  var n = Caml_primitive.caml_int_max(0, len$2 - cutcol | 0) + 3 | 0;
                  var spaces = Caml_string.bytes_to_string(Bytes.make(n, /* " " */32));
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
                    var spaces$1 = Caml_string.bytes_to_string(Bytes.make(diff, /* " " */32));
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
