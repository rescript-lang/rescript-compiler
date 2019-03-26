'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Stream = require("../../lib/js/stream.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function classify(chr) {
  if ((chr & 128) === 0) {
    return /* Single */Block.__(0, [chr]);
  } else if ((chr & 64) === 0) {
    return /* Cont */Block.__(1, [chr & 63]);
  } else if ((chr & 32) === 0) {
    return /* Leading */Block.__(2, [
              1,
              chr & 31
            ]);
  } else if ((chr & 16) === 0) {
    return /* Leading */Block.__(2, [
              2,
              chr & 15
            ]);
  } else if ((chr & 8) === 0) {
    return /* Leading */Block.__(2, [
              3,
              chr & 7
            ]);
  } else if ((chr & 4) === 0) {
    return /* Leading */Block.__(2, [
              4,
              chr & 3
            ]);
  } else if ((chr & 2) === 0) {
    return /* Leading */Block.__(2, [
              5,
              chr & 1
            ]);
  } else {
    return /* Invalid */0;
  }
}

function utf8_decode(strm) {
  return Stream.slazy((function (param) {
                var match = Stream.peek(strm);
                if (match !== undefined) {
                  Stream.junk(strm);
                  var match$1 = classify(match);
                  if (typeof match$1 === "number") {
                    throw [
                          Stream.$$Error,
                          "Invalid byte"
                        ];
                  } else {
                    switch (match$1.tag | 0) {
                      case 0 : 
                          return Stream.icons(match$1[0], utf8_decode(strm));
                      case 1 : 
                          throw [
                                Stream.$$Error,
                                "Unexpected continuation byte"
                              ];
                      case 2 : 
                          var follow = function (strm, _n, _c) {
                            while(true) {
                              var c = _c;
                              var n = _n;
                              if (n === 0) {
                                return c;
                              } else {
                                var match = classify(Stream.next(strm));
                                if (typeof match === "number") {
                                  throw [
                                        Stream.$$Error,
                                        "Continuation byte expected"
                                      ];
                                } else if (match.tag === 1) {
                                  _c = (c << 6) | match[0] & 63;
                                  _n = n - 1 | 0;
                                  continue ;
                                } else {
                                  throw [
                                        Stream.$$Error,
                                        "Continuation byte expected"
                                      ];
                                }
                              }
                            };
                          };
                          return Stream.icons(follow(strm, match$1[0], match$1[1]), utf8_decode(strm));
                      
                    }
                  }
                }
                
              }));
}

function to_list(xs) {
  var v = /* record */[/* contents : [] */0];
  Stream.iter((function (x) {
          v[0] = /* :: */[
            x,
            v[0]
          ];
          return /* () */0;
        }), xs);
  return List.rev(v[0]);
}

function utf8_list(s) {
  return to_list(utf8_decode(Stream.of_string(s)));
}

function decode(bytes, offset) {
  var offset$1 = offset;
  var match = classify(Caml_bytes.get(bytes, offset$1));
  if (typeof match === "number") {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "decode"
        ];
  } else {
    switch (match.tag | 0) {
      case 0 : 
          return /* tuple */[
                  match[0],
                  offset$1 + 1 | 0
                ];
      case 1 : 
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "decode"
              ];
      case 2 : 
          var _n = match[0];
          var _c = match[1];
          var _offset = offset$1 + 1 | 0;
          while(true) {
            var offset$2 = _offset;
            var c = _c;
            var n = _n;
            if (n === 0) {
              return /* tuple */[
                      c,
                      offset$2
                    ];
            } else {
              var match$1 = classify(Caml_bytes.get(bytes, offset$2));
              if (typeof match$1 === "number") {
                throw [
                      Caml_builtin_exceptions.invalid_argument,
                      "decode"
                    ];
              } else if (match$1.tag === 1) {
                _offset = offset$2 + 1 | 0;
                _c = (c << 6) | match$1[0] & 63;
                _n = n - 1 | 0;
                continue ;
              } else {
                throw [
                      Caml_builtin_exceptions.invalid_argument,
                      "decode"
                    ];
              }
            }
          };
      
    }
  }
}

function eq_list(cmp, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys && Curry._2(cmp, xs[0], ys[0])) {
        _ys = ys[1];
        _xs = xs[1];
        continue ;
      } else {
        return false;
      }
    } else if (ys) {
      return false;
    } else {
      return true;
    }
  };
}

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  console.log(/* tuple */[
        x,
        y
      ]);
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

List.iter((function (param) {
        return eq("File \"utf8_decode_test.ml\", line 107, characters 7-14", /* tuple */[
                    true,
                    eq_list((function (prim, prim$1) {
                            return prim === prim$1;
                          }), to_list(utf8_decode(Stream.of_string(param[0]))), param[1])
                  ]);
      }), /* :: */[
      /* tuple */[
        "\xe4\xbd\xa0\xe5\xa5\xbdBuckleScript,\xe6\x9c\x80\xe5\xa5\xbd\xe7\x9a\x84JS\xe8\xaf\xad\xe8\xa8\x80",
        /* :: */[
          20320,
          /* :: */[
            22909,
            /* :: */[
              66,
              /* :: */[
                117,
                /* :: */[
                  99,
                  /* :: */[
                    107,
                    /* :: */[
                      108,
                      /* :: */[
                        101,
                        /* :: */[
                          83,
                          /* :: */[
                            99,
                            /* :: */[
                              114,
                              /* :: */[
                                105,
                                /* :: */[
                                  112,
                                  /* :: */[
                                    116,
                                    /* :: */[
                                      44,
                                      /* :: */[
                                        26368,
                                        /* :: */[
                                          22909,
                                          /* :: */[
                                            30340,
                                            /* :: */[
                                              74,
                                              /* :: */[
                                                83,
                                                /* :: */[
                                                  35821,
                                                  /* :: */[
                                                    35328,
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
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ],
      /* :: */[
        /* tuple */[
          "hello \xe4\xbd\xa0\xe5\xa5\xbd\xef\xbc\x8c\xe4\xb8\xad\xe5\x8d\x8e\xe6\xb0\x91\xe6\x97\x8f hei",
          /* :: */[
            104,
            /* :: */[
              101,
              /* :: */[
                108,
                /* :: */[
                  108,
                  /* :: */[
                    111,
                    /* :: */[
                      32,
                      /* :: */[
                        20320,
                        /* :: */[
                          22909,
                          /* :: */[
                            65292,
                            /* :: */[
                              20013,
                              /* :: */[
                                21326,
                                /* :: */[
                                  27665,
                                  /* :: */[
                                    26063,
                                    /* :: */[
                                      32,
                                      /* :: */[
                                        104,
                                        /* :: */[
                                          101,
                                          /* :: */[
                                            105,
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
            ]
          ]
        ],
        /* [] */0
      ]
    ]);

Mt.from_pair_suites("Utf8_decode_test", suites[0]);

exports.classify = classify;
exports.utf8_decode = utf8_decode;
exports.to_list = to_list;
exports.utf8_list = utf8_list;
exports.decode = decode;
exports.eq_list = eq_list;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
