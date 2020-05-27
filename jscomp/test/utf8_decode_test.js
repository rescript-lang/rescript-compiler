'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Stream = require("../../lib/js/stream.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

function classify(chr) {
  if ((chr & 128) === 0) {
    return {
            TAG: /* Single */0,
            _0: chr
          };
  } else if ((chr & 64) === 0) {
    return {
            TAG: /* Cont */1,
            _0: chr & 63
          };
  } else if ((chr & 32) === 0) {
    return {
            TAG: /* Leading */2,
            _0: 1,
            _1: chr & 31
          };
  } else if ((chr & 16) === 0) {
    return {
            TAG: /* Leading */2,
            _0: 2,
            _1: chr & 15
          };
  } else if ((chr & 8) === 0) {
    return {
            TAG: /* Leading */2,
            _0: 3,
            _1: chr & 7
          };
  } else if ((chr & 4) === 0) {
    return {
            TAG: /* Leading */2,
            _0: 4,
            _1: chr & 3
          };
  } else if ((chr & 2) === 0) {
    return {
            TAG: /* Leading */2,
            _0: 5,
            _1: chr & 1
          };
  } else {
    return /* Invalid */0;
  }
}

function utf8_decode(strm) {
  return Stream.slazy(function (param) {
              var chr = Stream.peek(strm);
              if (chr === undefined) {
                return ;
              }
              Stream.junk(strm);
              var c = classify(chr);
              if (typeof c === "number") {
                throw {
                      RE_EXN_ID: Stream.$$Error,
                      _1: "Invalid byte",
                      Error: new Error()
                    };
              }
              switch (c.TAG | 0) {
                case /* Single */0 :
                    return Stream.icons(c._0, utf8_decode(strm));
                case /* Cont */1 :
                    throw {
                          RE_EXN_ID: Stream.$$Error,
                          _1: "Unexpected continuation byte",
                          Error: new Error()
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
                                RE_EXN_ID: Stream.$$Error,
                                _1: "Continuation byte expected",
                                Error: new Error()
                              };
                        }
                        if (cc.TAG === /* Cont */1) {
                          _c = (c << 6) | cc._0 & 63;
                          _n = n - 1 | 0;
                          continue ;
                        }
                        throw {
                              RE_EXN_ID: Stream.$$Error,
                              _1: "Continuation byte expected",
                              Error: new Error()
                            };
                      };
                    };
                    return Stream.icons(follow(strm, c._0, c._1), utf8_decode(strm));
                
              }
            });
}

function to_list(xs) {
  var v = {
    contents: /* [] */0
  };
  Stream.iter((function (x) {
          v.contents = {
            hd: x,
            tl: v.contents
          };
          
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
          RE_EXN_ID: "Invalid_argument",
          _1: "decode",
          Error: new Error()
        };
  }
  switch (c.TAG | 0) {
    case /* Single */0 :
        return [
                c._0,
                offset + 1 | 0
              ];
    case /* Cont */1 :
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "decode",
              Error: new Error()
            };
    case /* Leading */2 :
        var _n = c._0;
        var _c = c._1;
        var _offset = offset + 1 | 0;
        while(true) {
          var offset$1 = _offset;
          var c$1 = _c;
          var n = _n;
          if (n === 0) {
            return [
                    c$1,
                    offset$1
                  ];
          }
          var cc = classify(Caml_bytes.get(bytes, offset$1));
          if (typeof cc === "number") {
            throw {
                  RE_EXN_ID: "Invalid_argument",
                  _1: "decode",
                  Error: new Error()
                };
          }
          if (cc.TAG === /* Cont */1) {
            _offset = offset$1 + 1 | 0;
            _c = (c$1 << 6) | cc._0 & 63;
            _n = n - 1 | 0;
            continue ;
          }
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "decode",
                Error: new Error()
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
    if (!Curry._2(cmp, xs.hd, ys.hd)) {
      return false;
    }
    _ys = ys.tl;
    _xs = xs.tl;
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
  console.log([
        x,
        y
      ]);
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

List.iter((function (param) {
        return eq("File \"utf8_decode_test.ml\", line 107, characters 7-14", [
                    true,
                    eq_list((function (prim, prim$1) {
                            return prim === prim$1;
                          }), to_list(utf8_decode(Stream.of_string(param[0]))), param[1])
                  ]);
      }), {
      hd: [
        "\xe4\xbd\xa0\xe5\xa5\xbdBuckleScript,\xe6\x9c\x80\xe5\xa5\xbd\xe7\x9a\x84JS\xe8\xaf\xad\xe8\xa8\x80",
        {
          hd: 20320,
          tl: {
            hd: 22909,
            tl: {
              hd: 66,
              tl: {
                hd: 117,
                tl: {
                  hd: 99,
                  tl: {
                    hd: 107,
                    tl: {
                      hd: 108,
                      tl: {
                        hd: 101,
                        tl: {
                          hd: 83,
                          tl: {
                            hd: 99,
                            tl: {
                              hd: 114,
                              tl: {
                                hd: 105,
                                tl: {
                                  hd: 112,
                                  tl: {
                                    hd: 116,
                                    tl: {
                                      hd: 44,
                                      tl: {
                                        hd: 26368,
                                        tl: {
                                          hd: 22909,
                                          tl: {
                                            hd: 30340,
                                            tl: {
                                              hd: 74,
                                              tl: {
                                                hd: 83,
                                                tl: {
                                                  hd: 35821,
                                                  tl: {
                                                    hd: 35328,
                                                    tl: /* [] */0
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
      ],
      tl: {
        hd: [
          "hello \xe4\xbd\xa0\xe5\xa5\xbd\xef\xbc\x8c\xe4\xb8\xad\xe5\x8d\x8e\xe6\xb0\x91\xe6\x97\x8f hei",
          {
            hd: 104,
            tl: {
              hd: 101,
              tl: {
                hd: 108,
                tl: {
                  hd: 108,
                  tl: {
                    hd: 111,
                    tl: {
                      hd: 32,
                      tl: {
                        hd: 20320,
                        tl: {
                          hd: 22909,
                          tl: {
                            hd: 65292,
                            tl: {
                              hd: 20013,
                              tl: {
                                hd: 21326,
                                tl: {
                                  hd: 27665,
                                  tl: {
                                    hd: 26063,
                                    tl: {
                                      hd: 32,
                                      tl: {
                                        hd: 104,
                                        tl: {
                                          hd: 101,
                                          tl: {
                                            hd: 105,
                                            tl: /* [] */0
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
                    }
                  }
                }
              }
            }
          }
        ],
        tl: /* [] */0
      }
    });

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
