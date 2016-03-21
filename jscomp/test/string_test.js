// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Bytes                   = require("../stdlib/bytes");
var Mt                      = require("./mt");
var Ext_string              = require("./ext_string");
var $$String                = require("../stdlib/string");
var List                    = require("../stdlib/list");
var Caml_string             = require("../runtime/caml_string");

function ff(x) {
  var a;
  switch (x) {
    case "0" : 
    case "1" : 
    case "2" : 
        a = 3;
        break;
    case "3" : 
        a = 4;
        break;
    case "4" : 
        a = 6;
        break;
    case "7" : 
        a = 7;
        break;
    default:
      a = 8;
  }
  return a + 3;
}

function gg(x) {
  var a;
  if (x > 8 || x < 0) {
    a = 8;
  }
  else {
    switch (x) {
      case 0 : 
      case 1 : 
      case 2 : 
          a = 3;
          break;
      case 3 : 
          a = 4;
          break;
      case 4 : 
          a = 6;
          break;
      case 5 : 
      case 6 : 
      case 7 : 
          a = 8;
          break;
      case 8 : 
          a = 7;
          break;
      
    }
  }
  return a + 3;
}

function rev_split_by_char(c, s) {
  var _i = 0;
  var _l = /* [] */0;
  while(true) {
    var l = _l;
    var i = _i;
    try {
      var i$prime = $$String.index_from(s, i, c);
      var s$prime = $$String.sub(s, i, i$prime - i);
      _l = s$prime === "" ? l : /* :: */[
          s$prime,
          l
        ];
      _i = i$prime + 1;
      continue ;
      
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        return /* :: */[
                $$String.sub(s, i, s.length - i),
                l
              ];
      }
      else {
        throw exn;
      }
    }
  };
}

function xsplit(delim, s) {
  var len = s.length;
  if (len !== 0) {
    var _l = /* [] */0;
    var _i = len;
    while(true) {
      var i = _i;
      var l = _l;
      if (i !== 0) {
        var exit = 0;
        var i$prime;
        try {
          i$prime = $$String.rindex_from(s, i - 1, delim);
          exit = 1;
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            return /* :: */[
                    $$String.sub(s, 0, i),
                    l
                  ];
          }
          else {
            throw exn;
          }
        }
        if (exit === 1) {
          var l_000 = $$String.sub(s, i$prime + 1, i - i$prime - 1);
          var l$1 = /* :: */[
            l_000,
            l
          ];
          var l$2 = i$prime ? l$1 : /* :: */[
              "",
              l$1
            ];
          _i = i$prime;
          _l = l$2;
          continue ;
          
        }
        
      }
      else {
        return l;
      }
    };
  }
  else {
    return /* [] */0;
  }
}

function string_of_chars(x) {
  return $$String.concat("", List.map(function (prim) {
                  return Caml_string.string_of_char(prim);
                }, x));
}

Mt.from_pair_suites("string_test.ml", /* :: */[
      /* tuple */[
        "mutliple switch",
        function () {
          return /* Eq */{
                  0: 9,
                  1: ff("4"),
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "int switch",
          function () {
            return /* Eq */{
                    0: 9,
                    1: gg(4),
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "escape_normal",
            function () {
              return /* Eq */{
                      0: "haha",
                      1: $$String.escaped("haha"),
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "escape_bytes",
              function () {
                return /* Eq */{
                        0: Bytes.of_string("haha"),
                        1: Bytes.escaped(Bytes.of_string("haha")),
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "escape_quote",
                function () {
                  return /* Eq */{
                          0: '\\"\\"',
                          1: $$String.escaped('""'),
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "rev_split_by_char",
                  function () {
                    return /* Eq */{
                            0: /* :: */[
                              "",
                              /* :: */[
                                "bbbb",
                                /* :: */[
                                  "bbbb",
                                  /* [] */0
                                ]
                              ]
                            ],
                            1: rev_split_by_char(/* "a" */97, "bbbbabbbba"),
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "xsplit",
                    function () {
                      return /* Eq */{
                              0: /* :: */[
                                "a",
                                /* :: */[
                                  "b",
                                  /* :: */[
                                    "c",
                                    /* [] */0
                                  ]
                                ]
                              ],
                              1: xsplit(/* "." */46, "a.b.c"),
                              length: 2,
                              tag: 0
                            };
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "split_empty",
                      function () {
                        return /* Eq */{
                                0: /* [] */0,
                                1: Ext_string.split(/* None */0, "", /* "_" */95),
                                length: 2,
                                tag: 0
                              };
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "rfind",
                        function () {
                          return /* Eq */{
                                  0: 7,
                                  1: Ext_string.rfind("__", "__index__js"),
                                  length: 2,
                                  tag: 0
                                };
                        }
                      ],
                      /* :: */[
                        /* tuple */[
                          "rfind_2",
                          function () {
                            return /* Eq */{
                                    0: 0,
                                    1: Ext_string.rfind("__", "__index_js"),
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ],
                        /* :: */[
                          /* tuple */[
                            "rfind_3",
                            function () {
                              return /* Eq */{
                                      0: -1,
                                      1: Ext_string.rfind("__", "_index_js"),
                                      length: 2,
                                      tag: 0
                                    };
                            }
                          ],
                          /* :: */[
                            /* tuple */[
                              "find",
                              function () {
                                return /* Eq */{
                                        0: 0,
                                        1: Ext_string.find(/* None */0, "__", "__index__js"),
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ],
                            /* :: */[
                              /* tuple */[
                                "find_2",
                                function () {
                                  return /* Eq */{
                                          0: 6,
                                          1: Ext_string.find(/* None */0, "__", "_index__js"),
                                          length: 2,
                                          tag: 0
                                        };
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  "find_3",
                                  function () {
                                    return /* Eq */{
                                            0: -1,
                                            1: Ext_string.find(/* None */0, "__", "_index_js"),
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "of_char",
                                    function () {
                                      return /* Eq */{
                                              0: "0",
                                              1: $$String.make(1, /* "0" */48),
                                              length: 2,
                                              tag: 0
                                            };
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "of_chars",
                                      function () {
                                        return /* Eq */{
                                                0: string_of_chars(/* :: */[
                                                      /* "0" */48,
                                                      /* :: */[
                                                        /* "1" */49,
                                                        /* :: */[
                                                          /* "2" */50,
                                                          /* [] */0
                                                        ]
                                                      ]
                                                    ]),
                                                1: "012",
                                                length: 2,
                                                tag: 0
                                              };
                                      }
                                    ],
                                    /* [] */0
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

exports.ff                = ff;
exports.gg                = gg;
exports.rev_split_by_char = rev_split_by_char;
exports.xsplit            = xsplit;
exports.string_of_chars   = string_of_chars;
/*  Not a pure module */
