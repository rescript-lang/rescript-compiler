// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Bytes                   = require("../stdlib/bytes");
var Mt                      = require("./mt");
var $$String                = require("../stdlib/string");

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
      _l = s$prime === "" ? l : [
          /* :: */0,
          s$prime,
          l
        ];
      _i = i$prime + 1;
      continue ;
      
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.Not_found) {
        return [
                /* :: */0,
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
          if (exn === Caml_builtin_exceptions.Not_found) {
            return [
                    /* :: */0,
                    $$String.sub(s, 0, i),
                    l
                  ];
          }
          else {
            throw exn;
          }
        }
        if (exit === 1) {
          var l_001 = $$String.sub(s, i$prime + 1, i - i$prime - 1);
          var l$1 = [
            /* :: */0,
            l_001,
            l
          ];
          var l$2 = i$prime ? l$1 : [
              /* :: */0,
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

Mt.from_pair_suites("string_test.ml", [
      /* :: */0,
      [
        /* tuple */0,
        "mutliple switch",
        function () {
          return [
                  /* Eq */0,
                  9,
                  ff("4")
                ];
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "int switch",
          function () {
            return [
                    /* Eq */0,
                    9,
                    gg(4)
                  ];
          }
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "escape_normal",
            function () {
              return [
                      /* Eq */0,
                      "haha",
                      $$String.escaped("haha")
                    ];
            }
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "escape_bytes",
              function () {
                return [
                        /* Eq */0,
                        Bytes.of_string("haha"),
                        Bytes.escaped(Bytes.of_string("haha"))
                      ];
              }
            ],
            [
              /* :: */0,
              [
                /* tuple */0,
                "escape_quote",
                function () {
                  return [
                          /* Eq */0,
                          '\\"\\"',
                          $$String.escaped('""')
                        ];
                }
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "rev_split_by_char",
                  function () {
                    return [
                            /* Eq */0,
                            [
                              /* :: */0,
                              "",
                              [
                                /* :: */0,
                                "bbbb",
                                [
                                  /* :: */0,
                                  "bbbb",
                                  /* [] */0
                                ]
                              ]
                            ],
                            rev_split_by_char(/* "a" */97, "bbbbabbbba")
                          ];
                  }
                ],
                [
                  /* :: */0,
                  [
                    /* tuple */0,
                    "xsplit",
                    function () {
                      return [
                              /* Eq */0,
                              [
                                /* :: */0,
                                "a",
                                [
                                  /* :: */0,
                                  "b",
                                  [
                                    /* :: */0,
                                    "c",
                                    /* [] */0
                                  ]
                                ]
                              ],
                              xsplit(/* "." */46, "a.b.c")
                            ];
                    }
                  ],
                  /* [] */0
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
/*  Not a pure module */
