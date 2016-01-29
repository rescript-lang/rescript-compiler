// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives      = require("./pervasives");
var Caml_format     = require("../runtime/caml_format");
var Sys             = require("./sys");
var Printf          = require("./printf");
var Caml_primitive  = require("../runtime/caml_primitive");
var Buffer          = require("./buffer");
var $$String        = require("./string");
var List            = require("./list");

var Bad = [
  248,
  "Arg.Bad",
  ++ Caml_exceptions.caml_oo_last_id
];

var Help = [
  248,
  "Arg.Help",
  ++ Caml_exceptions.caml_oo_last_id
];

var Stop = [
  248,
  "Arg.Stop",
  ++ Caml_exceptions.caml_oo_last_id
];

function assoc3(x, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[1];
      if (Caml_primitive.caml_equal(match[1], x)) {
        return match[2];
      }
      else {
        _l = l[2];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function make_symlist(prefix, sep, suffix, l) {
  if (l) {
    return List.fold_left(function (x, y) {
                return x + (sep + y);
              }, prefix + l[1], l[2]) + suffix;
  }
  else {
    return "<none>";
  }
}

function help_action() {
  throw [
        0,
        Stop,
        [
          /* Unknown */0,
          "-help"
        ]
      ];
}

function add_help(speclist) {
  var add1;
  try {
    assoc3("-help", speclist);
    add1 = /* [] */0;
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      add1 = [
        /* :: */0,
        [
          /* tuple */0,
          "-help",
          [
            /* Unit */0,
            help_action
          ],
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn;
    }
  }
  var add2;
  try {
    assoc3("--help", speclist);
    add2 = /* [] */0;
  }
  catch (exn$1){
    if (exn$1 === Caml_exceptions.Not_found) {
      add2 = [
        /* :: */0,
        [
          /* tuple */0,
          "--help",
          [
            /* Unit */0,
            help_action
          ],
          " Display this list of options"
        ],
        /* [] */0
      ];
    }
    else {
      throw exn$1;
    }
  }
  return Pervasives.$at(speclist, Pervasives.$at(add1, add2));
}

function usage_b(buf, speclist, errmsg) {
  Printf.bprintf(buf, [
          /* Format */0,
          [
            /* String */2,
            /* No_padding */0,
            [
              /* Char_literal */12,
              /* "\n" */10,
              /* End_of_format */0
            ]
          ],
          "%s\n"
        ])(errmsg);
  return List.iter(function (param) {
              var buf$1 = buf;
              var param$1 = param;
              var doc = param$1[3];
              var spec = param$1[2];
              var key = param$1[1];
              if (doc.length) {
                if (spec[0] === 11) {
                  return Printf.bprintf(buf$1, [
                                /* Format */0,
                                [
                                  /* String_literal */11,
                                  "  ",
                                  [
                                    /* String */2,
                                    /* No_padding */0,
                                    [
                                      /* Char_literal */12,
                                      /* " " */32,
                                      [
                                        /* String */2,
                                        /* No_padding */0,
                                        [
                                          /* String */2,
                                          /* No_padding */0,
                                          [
                                            /* Char_literal */12,
                                            /* "\n" */10,
                                            /* End_of_format */0
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                "  %s %s%s\n"
                              ])(key, make_symlist("{", "|", "}", spec[1]), doc);
                }
                else {
                  return Printf.bprintf(buf$1, [
                                /* Format */0,
                                [
                                  /* String_literal */11,
                                  "  ",
                                  [
                                    /* String */2,
                                    /* No_padding */0,
                                    [
                                      /* Char_literal */12,
                                      /* " " */32,
                                      [
                                        /* String */2,
                                        /* No_padding */0,
                                        [
                                          /* Char_literal */12,
                                          /* "\n" */10,
                                          /* End_of_format */0
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                "  %s %s\n"
                              ])(key, doc);
                }
              }
              else {
                return 0;
              }
            }, add_help(speclist));
}

function usage_string(speclist, errmsg) {
  var b = Buffer.create(200);
  usage_b(b, speclist, errmsg);
  return Buffer.contents(b);
}

function usage(speclist, errmsg) {
  return Printf.eprintf([
                /* Format */0,
                [
                  /* String */2,
                  /* No_padding */0,
                  /* End_of_format */0
                ],
                "%s"
              ])(usage_string(speclist, errmsg));
}

var current = [
  0,
  0
];

function parse_argv_dynamic($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star ? $staropt$star[1] : current;
  var l = argv.length;
  var b = Buffer.create(200);
  var initpos = current$1[1];
  var stop = function (error) {
    var progname = initpos < l ? argv[initpos] : "(?)";
    switch (error[0]) {
      case 0 : 
          var s = error[1];
          switch (s) {
            case "--help" : 
            case "-help" : 
                break;
            default:
              Printf.bprintf(b, [
                      /* Format */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          ": unknown option '",
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* String_literal */11,
                              "'.\n",
                              /* End_of_format */0
                            ]
                          ]
                        ]
                      ],
                      "%s: unknown option '%s'.\n"
                    ])(progname, s);
          }
          break;
      case 1 : 
          Printf.bprintf(b, [
                  /* Format */0,
                  [
                    /* String */2,
                    /* No_padding */0,
                    [
                      /* String_literal */11,
                      ": wrong argument '",
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          "'; option '",
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* String_literal */11,
                              "' expects ",
                              [
                                /* String */2,
                                /* No_padding */0,
                                [
                                  /* String_literal */11,
                                  ".\n",
                                  /* End_of_format */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ],
                  "%s: wrong argument '%s'; option '%s' expects %s.\n"
                ])(progname, error[2], error[1], error[3]);
          break;
      case 2 : 
          Printf.bprintf(b, [
                  /* Format */0,
                  [
                    /* String */2,
                    /* No_padding */0,
                    [
                      /* String_literal */11,
                      ": option '",
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          "' needs an argument.\n",
                          /* End_of_format */0
                        ]
                      ]
                    ]
                  ],
                  "%s: option '%s' needs an argument.\n"
                ])(progname, error[1]);
          break;
      case 3 : 
          Printf.bprintf(b, [
                  /* Format */0,
                  [
                    /* String */2,
                    /* No_padding */0,
                    [
                      /* String_literal */11,
                      ": ",
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          ".\n",
                          /* End_of_format */0
                        ]
                      ]
                    ]
                  ],
                  "%s: %s.\n"
                ])(progname, error[1]);
          break;
      
    }
    usage_b(b, speclist[1], errmsg);
    if (Caml_primitive.caml_equal(error, [
            /* Unknown */0,
            "-help"
          ]) || Caml_primitive.caml_equal(error, [
            /* Unknown */0,
            "--help"
          ])) {
      throw [
            0,
            Help,
            Buffer.contents(b)
          ];
    }
    else {
      throw [
            0,
            Bad,
            Buffer.contents(b)
          ];
    }
  };
  ++ current$1[1];
  while(current$1[1] < l) {
    var s = argv[current$1[1]];
    if (s.length >= 1 && s[0] === "-") {
      var action;
      try {
        action = assoc3(s, speclist[1]);
      }
      catch (exn){
        if (exn === Caml_exceptions.Not_found) {
          action = stop([
                /* Unknown */0,
                s
              ]);
        }
        else {
          throw exn;
        }
      }
      try {
        var treat_action = (function(s){
        return function (param) {
          switch (param[0]) {
            case 0 : 
                return param[1](/* () */0);
            case 1 : 
                if (current$1[1] + 1 < l) {
                  var arg = argv[current$1[1] + 1];
                  try {
                    param[1](Pervasives.bool_of_string(arg));
                  }
                  catch (exn){
                    if (exn[1] === Caml_exceptions.Invalid_argument) {
                      if (exn[2] === "bool_of_string") {
                        throw [
                              0,
                              Stop,
                              [
                                /* Wrong */1,
                                s,
                                arg,
                                "a boolean"
                              ]
                            ];
                      }
                      else {
                        throw exn;
                      }
                    }
                    else {
                      throw exn;
                    }
                  }
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 2 : 
                param[1][1] = /* true */1;
                return /* () */0;
            case 3 : 
                param[1][1] = /* false */0;
                return /* () */0;
            case 4 : 
                if (current$1[1] + 1 < l) {
                  param[1](argv[current$1[1] + 1]);
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 5 : 
                if (current$1[1] + 1 < l) {
                  param[1][1] = argv[current$1[1] + 1];
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 6 : 
                if (current$1[1] + 1 < l) {
                  var arg$1 = argv[current$1[1] + 1];
                  try {
                    param[1](Caml_format.caml_int_of_string(arg$1));
                  }
                  catch (exn$1){
                    if (exn$1[1] === Caml_exceptions.Failure) {
                      if (exn$1[2] === "int_of_string") {
                        throw [
                              0,
                              Stop,
                              [
                                /* Wrong */1,
                                s,
                                arg$1,
                                "an integer"
                              ]
                            ];
                      }
                      else {
                        throw exn$1;
                      }
                    }
                    else {
                      throw exn$1;
                    }
                  }
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 7 : 
                if (current$1[1] + 1 < l) {
                  var arg$2 = argv[current$1[1] + 1];
                  try {
                    param[1][1] = Caml_format.caml_int_of_string(arg$2);
                  }
                  catch (exn$2){
                    if (exn$2[1] === Caml_exceptions.Failure) {
                      if (exn$2[2] === "int_of_string") {
                        throw [
                              0,
                              Stop,
                              [
                                /* Wrong */1,
                                s,
                                arg$2,
                                "an integer"
                              ]
                            ];
                      }
                      else {
                        throw exn$2;
                      }
                    }
                    else {
                      throw exn$2;
                    }
                  }
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 8 : 
                if (current$1[1] + 1 < l) {
                  var arg$3 = argv[current$1[1] + 1];
                  try {
                    param[1](Caml_format.caml_float_of_string(arg$3));
                  }
                  catch (exn$3){
                    if (exn$3[1] === Caml_exceptions.Failure) {
                      if (exn$3[2] === "float_of_string") {
                        throw [
                              0,
                              Stop,
                              [
                                /* Wrong */1,
                                s,
                                arg$3,
                                "a float"
                              ]
                            ];
                      }
                      else {
                        throw exn$3;
                      }
                    }
                    else {
                      throw exn$3;
                    }
                  }
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 9 : 
                if (current$1[1] + 1 < l) {
                  var arg$4 = argv[current$1[1] + 1];
                  try {
                    param[1][1] = Caml_format.caml_float_of_string(arg$4);
                  }
                  catch (exn$4){
                    if (exn$4[1] === Caml_exceptions.Failure) {
                      if (exn$4[2] === "float_of_string") {
                        throw [
                              0,
                              Stop,
                              [
                                /* Wrong */1,
                                s,
                                arg$4,
                                "a float"
                              ]
                            ];
                      }
                      else {
                        throw exn$4;
                      }
                    }
                    else {
                      throw exn$4;
                    }
                  }
                  return ++ current$1[1];
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 10 : 
                return List.iter(treat_action, param[1]);
            case 11 : 
                var symb = param[1];
                if (current$1[1] + 1 < l) {
                  var arg$5 = argv[current$1[1] + 1];
                  if (List.mem(arg$5, symb)) {
                    param[2](argv[current$1[1] + 1]);
                    return ++ current$1[1];
                  }
                  else {
                    throw [
                          0,
                          Stop,
                          [
                            /* Wrong */1,
                            s,
                            arg$5,
                            "one of: " + make_symlist("", " ", "", symb)
                          ]
                        ];
                  }
                }
                else {
                  throw [
                        0,
                        Stop,
                        [
                          /* Missing */2,
                          s
                        ]
                      ];
                }
                break;
            case 12 : 
                var f = param[1];
                while(current$1[1] < l - 1) {
                  f(argv[current$1[1] + 1]);
                  ++ current$1[1];
                };
                return /* () */0;
            
          }
        }
        }(s));
        treat_action(action);
      }
      catch (exn$1){
        if (exn$1[1] === Bad) {
          stop([
                /* Message */3,
                exn$1[2]
              ]);
        }
        else if (exn$1[1] === Stop) {
          stop(exn$1[2]);
        }
        else {
          throw exn$1;
        }
      }
      ++ current$1[1];
    }
    else {
      try {
        anonfun(s);
      }
      catch (exn$2){
        if (exn$2[1] === Bad) {
          stop([
                /* Message */3,
                exn$2[2]
              ]);
        }
        else {
          throw exn$2;
        }
      }
      ++ current$1[1];
    }
  };
  return /* () */0;
}

function parse_argv($staropt$star, argv, speclist, anonfun, errmsg) {
  var current$1 = $staropt$star ? $staropt$star[1] : current;
  return parse_argv_dynamic([
              /* Some */0,
              current$1
            ], argv, [
              0,
              speclist
            ], anonfun, errmsg);
}

function parse(l, f, msg) {
  try {
    return parse_argv(/* None */0, Sys.argv, l, f, msg);
  }
  catch (exn){
    if (exn[1] === Bad) {
      Printf.eprintf([
              /* Format */0,
              [
                /* String */2,
                /* No_padding */0,
                /* End_of_format */0
              ],
              "%s"
            ])(exn[2]);
      return Pervasives.exit(2);
    }
    else if (exn[1] === Help) {
      Printf.printf([
              /* Format */0,
              [
                /* String */2,
                /* No_padding */0,
                /* End_of_format */0
              ],
              "%s"
            ])(exn[2]);
      return Pervasives.exit(0);
    }
    else {
      throw exn;
    }
  }
}

function parse_dynamic(l, f, msg) {
  try {
    return parse_argv_dynamic(/* None */0, Sys.argv, l, f, msg);
  }
  catch (exn){
    if (exn[1] === Bad) {
      Printf.eprintf([
              /* Format */0,
              [
                /* String */2,
                /* No_padding */0,
                /* End_of_format */0
              ],
              "%s"
            ])(exn[2]);
      return Pervasives.exit(2);
    }
    else if (exn[1] === Help) {
      Printf.printf([
              /* Format */0,
              [
                /* String */2,
                /* No_padding */0,
                /* End_of_format */0
              ],
              "%s"
            ])(exn[2]);
      return Pervasives.exit(0);
    }
    else {
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
      }
      else if (s[n] === " ") {
        _n = n + 1;
      }
      else {
        return n;
      }
    };
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      return len;
    }
    else {
      throw exn;
    }
  }
}

function max_arg_len(cur, param) {
  var kwd = param[1];
  if (param[2][0] === 11) {
    return Pervasives.max(cur, kwd.length);
  }
  else {
    return Pervasives.max(cur, kwd.length + second_word(param[3]));
  }
}

function align($staropt$star, speclist) {
  var limit = $staropt$star ? $staropt$star[1] : Pervasives.max_int;
  var completed = add_help(speclist);
  var len = List.fold_left(max_arg_len, 0, completed);
  var len$1 = Pervasives.min(len, limit);
  return List.map(function (param) {
              var len$2 = len$1;
              var ksd = param;
              var spec = ksd[2];
              var kwd = ksd[1];
              if (ksd[3] === "") {
                return ksd;
              }
              else if (spec[0] === 11) {
                var msg = ksd[3];
                var cutcol = second_word(msg);
                var spaces = $$String.make(Pervasives.max(0, len$2 - cutcol) + 3, /* " " */32);
                return [
                        /* tuple */0,
                        kwd,
                        spec,
                        "\n" + (spaces + msg)
                      ];
              }
              else {
                var msg$1 = ksd[3];
                var cutcol$1 = second_word(msg$1);
                var kwd_len = kwd.length;
                var diff = len$2 - kwd_len - cutcol$1;
                if (diff <= 0) {
                  return [
                          /* tuple */0,
                          kwd,
                          spec,
                          msg$1
                        ];
                }
                else {
                  var spaces$1 = $$String.make(diff, /* " " */32);
                  var prefix = $$String.sub(msg$1, 0, cutcol$1);
                  var suffix = $$String.sub(msg$1, cutcol$1, msg$1.length - cutcol$1);
                  return [
                          /* tuple */0,
                          kwd,
                          spec,
                          prefix + (spaces$1 + suffix)
                        ];
                }
              }
            }, completed);
}

exports.parse              = parse;
exports.parse_dynamic      = parse_dynamic;
exports.parse_argv         = parse_argv;
exports.parse_argv_dynamic = parse_argv_dynamic;
exports.Help               = Help;
exports.Bad                = Bad;
exports.usage              = usage;
exports.usage_string       = usage_string;
exports.align              = align;
exports.current            = current;
/* No side effect */
