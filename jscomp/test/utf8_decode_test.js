'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Stream = require("../../lib/js/stream.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

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
                var chr = Stream.peek(strm);
                if (chr === undefined) {
                  return ;
                }
                Stream.junk(strm);
                var c = classify(chr);
                if (typeof c === "number") {
                  throw {
                        ExceptionID: Stream.$$Error.ExceptionID,
                        _1: "Invalid byte",
                        Debug: Stream.$$Error.Debug
                      };
                }
                switch (c.tag | 0) {
                  case /* Single */0 :
                      return Stream.icons(c[0], utf8_decode(strm));
                  case /* Cont */1 :
                      throw {
                            ExceptionID: Stream.$$Error.ExceptionID,
                            _1: "Unexpected continuation byte",
                            Debug: Stream.$$Error.Debug
                          };
                  case /* Leading */2 :
                      var follow = function (strm, _n, _c) {
                        while(true) {
                          var c = _c;
                          var n = _n;
                          if (n === 0) {
                            return c;
                          }
                          var cc = classify(Stream.next(strm));
                          if (typeof cc === "number") {
                            throw {
                                  ExceptionID: Stream.$$Error.ExceptionID,
                                  _1: "Continuation byte expected",
                                  Debug: Stream.$$Error.Debug
                                };
                          }
                          if (cc.tag === /* Cont */1) {
                            _c = (c << 6) | cc[0] & 63;
                            _n = n - 1 | 0;
                            continue ;
                          }
                          throw {
                                ExceptionID: Stream.$$Error.ExceptionID,
                                _1: "Continuation byte expected",
                                Debug: Stream.$$Error.Debug
                              };
                        };
                      };
                      return Stream.icons(follow(strm, c[0], c[1]), utf8_decode(strm));
                  
                }
              }));
}

function to_list(xs) {
  var v = {
    contents: /* [] */0
  };
  Stream.iter((function (x) {
          v.contents = /* :: */[
            x,
            v.contents
          ];
          
        }), xs);
  return List.rev(v.contents);
}

function utf8_list(s) {
  return to_list(utf8_decode(Stream.of_string(s)));
}

function decode(bytes, offset) {
  var c = classify(Caml_bytes.get(bytes, offset));
  if (typeof c === "number") {
    throw {
          ExceptionID: -3,
          _1: "decode",
          Debug: "Invalid_argument"
        };
  }
  switch (c.tag | 0) {
    case /* Single */0 :
        return /* tuple */[
                c[0],
                offset + 1 | 0
              ];
    case /* Cont */1 :
        throw {
              ExceptionID: -3,
              _1: "decode",
              Debug: "Invalid_argument"
            };
    case /* Leading */2 :
        var _n = c[0];
        var _c = c[1];
        var _offset = offset + 1 | 0;
        while(true) {
          var offset$1 = _offset;
          var c$1 = _c;
          var n = _n;
          if (n === 0) {
            return /* tuple */[
                    c$1,
                    offset$1
                  ];
          }
          var cc = classify(Caml_bytes.get(bytes, offset$1));
          if (typeof cc === "number") {
            throw {
                  ExceptionID: -3,
                  _1: "decode",
                  Debug: "Invalid_argument"
                };
          }
          if (cc.tag === /* Cont */1) {
            _offset = offset$1 + 1 | 0;
            _c = (c$1 << 6) | cc[0] & 63;
            _n = n - 1 | 0;
            continue ;
          }
          throw {
                ExceptionID: -3,
                _1: "decode",
                Debug: "Invalid_argument"
              };
        };
    
  }
}

function eq_list(cmp, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (!xs) {
      if (ys) {
        return false;
      } else {
        return true;
      }
    }
    if (!ys) {
      return false;
    }
    if (!Curry._2(cmp, xs[0], ys[0])) {
      return false;
    }
    _ys = ys[1];
    _xs = xs[1];
    continue ;
  };
}

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  console.log(/* tuple */[
        x,
        y
      ]);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
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

Mt.from_pair_suites("Utf8_decode_test", suites.contents);

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
