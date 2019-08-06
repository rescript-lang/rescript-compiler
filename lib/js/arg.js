'use strict';

var Sys = require("./sys.js");
var List = require("./list.js");
var $$Array = require("./array.js");
var Block = require("./block.js");
var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_io = require("./caml_io.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
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

function split(s) {
  var i = $$String.index(s, /* "=" */61);
  var len = s.length;
  return /* tuple */[
          $$String.sub(s, 0, i),
          $$String.sub(s, i + 1 | 0, len - (i + 1 | 0) | 0)
        ];
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

function help_action(param) {
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
                if (doc.length !== 0) {
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

var current = /* record */[/* contents */0];

function bool_of_string_opt(x) {
  try {
    return Pervasives.bool_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function int_of_string_opt(x) {
  try {
    return Caml_format.caml_int_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function float_of_string_opt(x) {
  try {
    return Caml_format.caml_float_of_string(x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      return undefined;
    } else {
      throw exn;
    }
  }
}

function parse_and_expand_argv_dynamic_aux(allow_expand, current, argv, speclist, anonfun, errmsg) {
  var initpos = current[0];
  var convert_error = function (error) {
    var b = $$Buffer.create(200);
    var progname = initpos < argv[0].length ? Caml_array.caml_array_get(argv[0], initpos) : "(?)";
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
      return [
              Help,
              $$Buffer.contents(b)
            ];
    } else {
      return [
              Bad,
              $$Buffer.contents(b)
            ];
    }
  };
  current[0] = current[0] + 1 | 0;
  while(current[0] < argv[0].length) {
    try {
      var s = Caml_array.caml_array_get(argv[0], current[0]);
      if (s.length >= 1 && Caml_string.get(s, 0) === /* "-" */45) {
        var match;
        try {
          match = /* tuple */[
            assoc3(s, speclist[0]),
            undefined
          ];
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            try {
              var match$1 = split(s);
              match = /* tuple */[
                assoc3(match$1[0], speclist[0]),
                match$1[1]
              ];
            }
            catch (exn$1){
              if (exn$1 === Caml_builtin_exceptions.not_found) {
                throw [
                      Stop,
                      /* Unknown */Block.__(0, [s])
                    ];
              }
              throw exn$1;
            }
          } else {
            throw exn;
          }
        }
        var follow = match[1];
        var no_arg = (function(s,follow){
        return function no_arg(param) {
          if (follow !== undefined) {
            throw [
                  Stop,
                  /* Wrong */Block.__(1, [
                      s,
                      follow,
                      "no argument"
                    ])
                ];
          } else {
            return /* () */0;
          }
        }
        }(s,follow));
        var get_arg = (function(s,follow){
        return function get_arg(param) {
          if (follow !== undefined) {
            return follow;
          } else if ((current[0] + 1 | 0) < argv[0].length) {
            return Caml_array.caml_array_get(argv[0], current[0] + 1 | 0);
          } else {
            throw [
                  Stop,
                  /* Missing */Block.__(2, [s])
                ];
          }
        }
        }(s,follow));
        var consume_arg = (function(follow){
        return function consume_arg(param) {
          if (follow !== undefined) {
            return /* () */0;
          } else {
            current[0] = current[0] + 1 | 0;
            return /* () */0;
          }
        }
        }(follow));
        var treat_action = (function(s){
        return function treat_action(param) {
          switch (param.tag | 0) {
            case 0 : 
                return Curry._1(param[0], /* () */0);
            case 1 : 
                var arg = get_arg(/* () */0);
                var match = bool_of_string_opt(arg);
                if (match !== undefined) {
                  Curry._1(param[0], match);
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg,
                            "a boolean"
                          ])
                      ];
                }
                return consume_arg(/* () */0);
            case 2 : 
                no_arg(/* () */0);
                param[0][0] = true;
                return /* () */0;
            case 3 : 
                no_arg(/* () */0);
                param[0][0] = false;
                return /* () */0;
            case 4 : 
                var arg$1 = get_arg(/* () */0);
                Curry._1(param[0], arg$1);
                return consume_arg(/* () */0);
            case 5 : 
                param[0][0] = get_arg(/* () */0);
                return consume_arg(/* () */0);
            case 6 : 
                var arg$2 = get_arg(/* () */0);
                var match$1 = int_of_string_opt(arg$2);
                if (match$1 !== undefined) {
                  Curry._1(param[0], match$1);
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg$2,
                            "an integer"
                          ])
                      ];
                }
                return consume_arg(/* () */0);
            case 7 : 
                var arg$3 = get_arg(/* () */0);
                var match$2 = int_of_string_opt(arg$3);
                if (match$2 !== undefined) {
                  param[0][0] = match$2;
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg$3,
                            "an integer"
                          ])
                      ];
                }
                return consume_arg(/* () */0);
            case 8 : 
                var arg$4 = get_arg(/* () */0);
                var match$3 = float_of_string_opt(arg$4);
                if (match$3 !== undefined) {
                  Curry._1(param[0], match$3);
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg$4,
                            "a float"
                          ])
                      ];
                }
                return consume_arg(/* () */0);
            case 9 : 
                var arg$5 = get_arg(/* () */0);
                var match$4 = float_of_string_opt(arg$5);
                if (match$4 !== undefined) {
                  param[0][0] = match$4;
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg$5,
                            "a float"
                          ])
                      ];
                }
                return consume_arg(/* () */0);
            case 10 : 
                return List.iter(treat_action, param[0]);
            case 11 : 
                var symb = param[0];
                var arg$6 = get_arg(/* () */0);
                if (List.mem(arg$6, symb)) {
                  Curry._1(param[1], arg$6);
                  return consume_arg(/* () */0);
                } else {
                  throw [
                        Stop,
                        /* Wrong */Block.__(1, [
                            s,
                            arg$6,
                            "one of: " + make_symlist("", " ", "", symb)
                          ])
                      ];
                }
            case 12 : 
                var f = param[0];
                while(current[0] < (argv[0].length - 1 | 0)) {
                  Curry._1(f, Caml_array.caml_array_get(argv[0], current[0] + 1 | 0));
                  consume_arg(/* () */0);
                };
                return /* () */0;
            case 13 : 
                if (!allow_expand) {
                  throw [
                        Caml_builtin_exceptions.invalid_argument,
                        "Arg.Expand is is only allowed with Arg.parse_and_expand_argv_dynamic"
                      ];
                }
                var arg$7 = get_arg(/* () */0);
                var newarg = Curry._1(param[0], arg$7);
                consume_arg(/* () */0);
                var before = $$Array.sub(argv[0], 0, current[0] + 1 | 0);
                var after = $$Array.sub(argv[0], current[0] + 1 | 0, (argv[0].length - current[0] | 0) - 1 | 0);
                argv[0] = Caml_array.caml_array_concat(/* :: */[
                      before,
                      /* :: */[
                        newarg,
                        /* :: */[
                          after,
                          /* [] */0
                        ]
                      ]
                    ]);
                return /* () */0;
            
          }
        }
        }(s));
        treat_action(match[0]);
      } else {
        Curry._1(anonfun, s);
      }
    }
    catch (raw_exn){
      var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn$2[0] === Bad) {
        throw convert_error(/* Message */Block.__(3, [exn$2[1]]));
      }
      if (exn$2[0] === Stop) {
        throw convert_error(exn$2[1]);
      }
      throw exn$2;
    }
    current[0] = current[0] + 1 | 0;
  };
  return /* () */0;
}

function parse_and_expand_argv_dynamic(current, argv, speclist, anonfun, errmsg) {
  return parse_and_expand_argv_dynamic_aux(true, current, argv, speclist, anonfun, errmsg);
}

function parse_argv_dynamic($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star !== undefined ? $staropt$star : current;
  return parse_and_expand_argv_dynamic_aux(false, current$1, /* record */[/* contents */argv], speclist, anonfun, errmsg);
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
    return parse_argv_dynamic(undefined, Sys.argv, l, f, msg);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
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

function parse_expand(l, f, msg) {
  try {
    var argv = /* record */[/* contents */Sys.argv];
    var spec = /* record */[/* contents */l];
    var current$1 = /* record */[/* contents */current[0]];
    return parse_and_expand_argv_dynamic(current$1, argv, spec, f, msg);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
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
  var loop = function (_n) {
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
  };
  var exit = 0;
  var n;
  try {
    n = $$String.index(s, /* "\t" */9);
    exit = 1;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var exit$1 = 0;
      var n$1;
      try {
        n$1 = $$String.index(s, /* " " */32);
        exit$1 = 2;
      }
      catch (exn$1){
        if (exn$1 === Caml_builtin_exceptions.not_found) {
          return len;
        } else {
          throw exn$1;
        }
      }
      if (exit$1 === 2) {
        return loop(n$1 + 1 | 0);
      }
      
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return loop(n + 1 | 0);
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

function replace_leading_tab(s) {
  var seen = /* record */[/* contents */false];
  return $$String.map((function (c) {
                if (c !== 9 || seen[0]) {
                  return c;
                } else {
                  seen[0] = true;
                  return /* " " */32;
                }
              }), s);
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
                } else if (spec.tag === 11) {
                  var msg = ksd[2];
                  var cutcol = second_word(msg);
                  var n = Caml_primitive.caml_int_max(0, len$2 - cutcol | 0) + 3 | 0;
                  var spaces = Caml_bytes.bytes_to_string(Bytes.make(n, /* " " */32));
                  return /* tuple */[
                          kwd,
                          spec,
                          "\n" + (spaces + replace_leading_tab(msg))
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
                            replace_leading_tab(msg$1)
                          ];
                  } else {
                    var spaces$1 = Caml_bytes.bytes_to_string(Bytes.make(diff, /* " " */32));
                    var prefix = $$String.sub(replace_leading_tab(msg$1), 0, cutcol$1);
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

function trim_cr(s) {
  var len = s.length;
  if (len > 0 && Caml_string.get(s, len - 1 | 0) === /* "\r" */13) {
    return $$String.sub(s, 0, len - 1 | 0);
  } else {
    return s;
  }
}

function read_aux(trim, sep, file) {
  var ic = Pervasives.open_in_bin(file);
  var buf = $$Buffer.create(200);
  var words = /* record */[/* contents : [] */0];
  var stash = function (param) {
    var word = $$Buffer.contents(buf);
    var word$1 = trim ? trim_cr(word) : word;
    words[0] = /* :: */[
      word$1,
      words[0]
    ];
    buf[/* position */1] = 0;
    return /* () */0;
  };
  var read = function (param) {
    try {
      var c = Caml_external_polyfill.resolve("caml_ml_input_char")(ic);
      if (c === sep) {
        stash(/* () */0);
        return read(/* () */0);
      } else {
        $$Buffer.add_char(buf, c);
        return read(/* () */0);
      }
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.end_of_file) {
        if (buf[/* position */1] > 0) {
          return stash(/* () */0);
        } else {
          return 0;
        }
      } else {
        throw exn;
      }
    }
  };
  read(/* () */0);
  Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
  return $$Array.of_list(List.rev(words[0]));
}

function read_arg(param) {
  return read_aux(true, /* "\n" */10, param);
}

function read_arg0(param) {
  return read_aux(false, /* "\000" */0, param);
}

function write_aux(sep, file, args) {
  var oc = Pervasives.open_out_bin(file);
  $$Array.iter((function (s) {
          return Curry._2(Printf.fprintf(oc, /* Format */[
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char */Block.__(0, [/* End_of_format */0])
                            ]),
                          "%s%c"
                        ]), s, sep);
        }), args);
  Caml_io.caml_ml_flush(oc);
  return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
}

function write_arg(param, param$1) {
  return write_aux(/* "\n" */10, param, param$1);
}

function write_arg0(param, param$1) {
  return write_aux(/* "\000" */0, param, param$1);
}

exports.parse = parse;
exports.parse_dynamic = parse_dynamic;
exports.parse_argv = parse_argv;
exports.parse_argv_dynamic = parse_argv_dynamic;
exports.parse_and_expand_argv_dynamic = parse_and_expand_argv_dynamic;
exports.parse_expand = parse_expand;
exports.Help = Help;
exports.Bad = Bad;
exports.usage = usage;
exports.usage_string = usage_string;
exports.align = align;
exports.current = current;
exports.read_arg = read_arg;
exports.read_arg0 = read_arg0;
exports.write_arg = write_arg;
exports.write_arg0 = write_arg0;
/* No side effect */
