'use strict';

var Mt = require("./mt.js");
var Js_array = require("../../lib/js/js_array.js");
var Belt_Option = require("../../lib/js/belt_Option.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
  "make",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: "null",
              _1: String(null).concat("")
            };
    })
];

var suites_1 = {
  hd: [
    "fromCharCode",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: "a",
                _1: String.fromCharCode(97)
              };
      })
  ],
  tl: {
    hd: [
      "fromCharCodeMany",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: "az",
                  _1: String.fromCharCode(97, 122)
                };
        })
    ],
    tl: {
      hd: [
        "fromCodePoint",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: "a",
                    _1: String.fromCodePoint(97)
                  };
          })
      ],
      tl: {
        hd: [
          "fromCodePointMany",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: "az",
                      _1: String.fromCodePoint(97, 122)
                    };
            })
        ],
        tl: {
          hd: [
            "length",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: 3,
                        _1: "foo".length
                      };
              })
          ],
          tl: {
            hd: [
              "get",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: "a",
                          _1: "foobar"[4]
                        };
                })
            ],
            tl: {
              hd: [
                "charAt",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: "a",
                            _1: "foobar".charAt(4)
                          };
                  })
              ],
              tl: {
                hd: [
                  "charCodeAt",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: 97,
                              _1: "foobar".charCodeAt(4)
                            };
                    })
                ],
                tl: {
                  hd: [
                    "codePointAt",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: 97,
                                _1: "foobar".codePointAt(4)
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "codePointAt - out of bounds",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: undefined,
                                  _1: "foobar".codePointAt(98)
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "concat",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: "foobar",
                                    _1: "foo".concat("bar")
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "concatMany",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: "foobarbaz",
                                      _1: "foo".concat("bar", "baz")
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "endsWith",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: true,
                                        _1: "foobar".endsWith("bar")
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "endsWithFrom",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: false,
                                          _1: "foobar".endsWith("bar", 1)
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "includes",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: true,
                                            _1: "foobarbaz".includes("bar")
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "includesFrom",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: false,
                                              _1: "foobarbaz".includes("bar", 4)
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "indexOf",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: 3,
                                                _1: "foobarbaz".indexOf("bar")
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "indexOfFrom",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: -1,
                                                  _1: "foobarbaz".indexOf("bar", 4)
                                                };
                                        })
                                    ],
                                    tl: {
                                      hd: [
                                        "lastIndexOf",
                                        (function (param) {
                                            return {
                                                    TAG: /* Eq */0,
                                                    _0: 3,
                                                    _1: "foobarbaz".lastIndexOf("bar")
                                                  };
                                          })
                                      ],
                                      tl: {
                                        hd: [
                                          "lastIndexOfFrom",
                                          (function (param) {
                                              return {
                                                      TAG: /* Eq */0,
                                                      _0: 3,
                                                      _1: "foobarbaz".lastIndexOf("bar", 4)
                                                    };
                                            })
                                        ],
                                        tl: {
                                          hd: [
                                            "localeCompare",
                                            (function (param) {
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: 0,
                                                        _1: "foo".localeCompare("foo")
                                                      };
                                              })
                                          ],
                                          tl: {
                                            hd: [
                                              "match",
                                              (function (param) {
                                                  return {
                                                          TAG: /* Eq */0,
                                                          _0: [
                                                            "na",
                                                            "na"
                                                          ],
                                                          _1: Caml_option.null_to_opt("banana".match(/na+/g))
                                                        };
                                                })
                                            ],
                                            tl: {
                                              hd: [
                                                "match - no match",
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: undefined,
                                                            _1: Caml_option.null_to_opt("banana".match(/nanana+/g))
                                                          };
                                                  })
                                              ],
                                              tl: {
                                                hd: [
                                                  "match - not found capture groups",
                                                  (function (param) {
                                                      return {
                                                              TAG: /* Eq */0,
                                                              _0: [
                                                                "hello ",
                                                                undefined
                                                              ],
                                                              _1: Belt_Option.map(Caml_option.null_to_opt("hello word".match(/hello (world)?/)), Js_array.copy)
                                                            };
                                                    })
                                                ],
                                                tl: {
                                                  hd: [
                                                    "normalize",
                                                    (function (param) {
                                                        return {
                                                                TAG: /* Eq */0,
                                                                _0: "foo",
                                                                _1: "foo".normalize()
                                                              };
                                                      })
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "normalizeByForm",
                                                      (function (param) {
                                                          return {
                                                                  TAG: /* Eq */0,
                                                                  _0: "foo",
                                                                  _1: "foo".normalize("NFKD")
                                                                };
                                                        })
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "repeat",
                                                        (function (param) {
                                                            return {
                                                                    TAG: /* Eq */0,
                                                                    _0: "foofoofoo",
                                                                    _1: "foo".repeat(3)
                                                                  };
                                                          })
                                                      ],
                                                      tl: {
                                                        hd: [
                                                          "replace",
                                                          (function (param) {
                                                              return {
                                                                      TAG: /* Eq */0,
                                                                      _0: "fooBORKbaz",
                                                                      _1: "foobarbaz".replace("bar", "BORK")
                                                                    };
                                                            })
                                                        ],
                                                        tl: {
                                                          hd: [
                                                            "replaceByRe",
                                                            (function (param) {
                                                                return {
                                                                        TAG: /* Eq */0,
                                                                        _0: "fooBORKBORK",
                                                                        _1: "foobarbaz".replace(/ba./g, "BORK")
                                                                      };
                                                              })
                                                          ],
                                                          tl: {
                                                            hd: [
                                                              "unsafeReplaceBy0",
                                                              (function (param) {
                                                                  var replace = function (whole, offset, s) {
                                                                    if (whole === "bar") {
                                                                      return "BORK";
                                                                    } else {
                                                                      return "DORK";
                                                                    }
                                                                  };
                                                                  return {
                                                                          TAG: /* Eq */0,
                                                                          _0: "fooBORKDORK",
                                                                          _1: "foobarbaz".replace(/ba./g, replace)
                                                                        };
                                                                })
                                                            ],
                                                            tl: {
                                                              hd: [
                                                                "unsafeReplaceBy1",
                                                                (function (param) {
                                                                    var replace = function (whole, p1, offset, s) {
                                                                      if (whole === "bar") {
                                                                        return "BORK";
                                                                      } else {
                                                                        return "DORK";
                                                                      }
                                                                    };
                                                                    return {
                                                                            TAG: /* Eq */0,
                                                                            _0: "fooBORKDORK",
                                                                            _1: "foobarbaz".replace(/ba./g, replace)
                                                                          };
                                                                  })
                                                              ],
                                                              tl: {
                                                                hd: [
                                                                  "unsafeReplaceBy2",
                                                                  (function (param) {
                                                                      var replace = function (whole, p1, p2, offset, s) {
                                                                        if (whole === "bar") {
                                                                          return "BORK";
                                                                        } else {
                                                                          return "DORK";
                                                                        }
                                                                      };
                                                                      return {
                                                                              TAG: /* Eq */0,
                                                                              _0: "fooBORKDORK",
                                                                              _1: "foobarbaz".replace(/ba./g, replace)
                                                                            };
                                                                    })
                                                                ],
                                                                tl: {
                                                                  hd: [
                                                                    "unsafeReplaceBy3",
                                                                    (function (param) {
                                                                        var replace = function (whole, p1, p2, p3, offset, s) {
                                                                          if (whole === "bar") {
                                                                            return "BORK";
                                                                          } else {
                                                                            return "DORK";
                                                                          }
                                                                        };
                                                                        return {
                                                                                TAG: /* Eq */0,
                                                                                _0: "fooBORKDORK",
                                                                                _1: "foobarbaz".replace(/ba./g, replace)
                                                                              };
                                                                      })
                                                                  ],
                                                                  tl: {
                                                                    hd: [
                                                                      "search",
                                                                      (function (param) {
                                                                          return {
                                                                                  TAG: /* Eq */0,
                                                                                  _0: 3,
                                                                                  _1: "foobarbaz".search(/ba./g)
                                                                                };
                                                                        })
                                                                    ],
                                                                    tl: {
                                                                      hd: [
                                                                        "slice",
                                                                        (function (param) {
                                                                            return {
                                                                                    TAG: /* Eq */0,
                                                                                    _0: "bar",
                                                                                    _1: "foobarbaz".slice(3, 6)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      tl: {
                                                                        hd: [
                                                                          "sliceToEnd",
                                                                          (function (param) {
                                                                              return {
                                                                                      TAG: /* Eq */0,
                                                                                      _0: "barbaz",
                                                                                      _1: "foobarbaz".slice(3)
                                                                                    };
                                                                            })
                                                                        ],
                                                                        tl: {
                                                                          hd: [
                                                                            "split",
                                                                            (function (param) {
                                                                                return {
                                                                                        TAG: /* Eq */0,
                                                                                        _0: [
                                                                                          "foo",
                                                                                          "bar",
                                                                                          "baz"
                                                                                        ],
                                                                                        _1: "foo bar baz".split(" ")
                                                                                      };
                                                                              })
                                                                          ],
                                                                          tl: {
                                                                            hd: [
                                                                              "splitAtMost",
                                                                              (function (param) {
                                                                                  return {
                                                                                          TAG: /* Eq */0,
                                                                                          _0: [
                                                                                            "foo",
                                                                                            "bar"
                                                                                          ],
                                                                                          _1: "foo bar baz".split(" ", 2)
                                                                                        };
                                                                                })
                                                                            ],
                                                                            tl: {
                                                                              hd: [
                                                                                "splitByRe",
                                                                                (function (param) {
                                                                                    return {
                                                                                            TAG: /* Eq */0,
                                                                                            _0: [
                                                                                              "a",
                                                                                              "#",
                                                                                              undefined,
                                                                                              "b",
                                                                                              "#",
                                                                                              ":",
                                                                                              "c"
                                                                                            ],
                                                                                            _1: "a#b#:c".split(/(#)(:)?/)
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              tl: {
                                                                                hd: [
                                                                                  "splitByReAtMost",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              TAG: /* Eq */0,
                                                                                              _0: [
                                                                                                "a",
                                                                                                "#",
                                                                                                undefined
                                                                                              ],
                                                                                              _1: "a#b#:c".split(/(#)(:)?/, 3)
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                tl: {
                                                                                  hd: [
                                                                                    "startsWith",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                TAG: /* Eq */0,
                                                                                                _0: true,
                                                                                                _1: "foobarbaz".startsWith("foo")
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  tl: {
                                                                                    hd: [
                                                                                      "startsWithFrom",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  TAG: /* Eq */0,
                                                                                                  _0: false,
                                                                                                  _1: "foobarbaz".startsWith("foo", 1)
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    tl: {
                                                                                      hd: [
                                                                                        "substr",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    TAG: /* Eq */0,
                                                                                                    _0: "barbaz",
                                                                                                    _1: "foobarbaz".substr(3)
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      tl: {
                                                                                        hd: [
                                                                                          "substrAtMost",
                                                                                          (function (param) {
                                                                                              return {
                                                                                                      TAG: /* Eq */0,
                                                                                                      _0: "bar",
                                                                                                      _1: "foobarbaz".substr(3, 3)
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        tl: {
                                                                                          hd: [
                                                                                            "substring",
                                                                                            (function (param) {
                                                                                                return {
                                                                                                        TAG: /* Eq */0,
                                                                                                        _0: "bar",
                                                                                                        _1: "foobarbaz".substring(3, 6)
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          tl: {
                                                                                            hd: [
                                                                                              "substringToEnd",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          TAG: /* Eq */0,
                                                                                                          _0: "barbaz",
                                                                                                          _1: "foobarbaz".substring(3)
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            tl: {
                                                                                              hd: [
                                                                                                "toLowerCase",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            TAG: /* Eq */0,
                                                                                                            _0: "bork",
                                                                                                            _1: "BORK".toLowerCase()
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              tl: {
                                                                                                hd: [
                                                                                                  "toLocaleLowerCase",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              TAG: /* Eq */0,
                                                                                                              _0: "bork",
                                                                                                              _1: "BORK".toLocaleLowerCase()
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                tl: {
                                                                                                  hd: [
                                                                                                    "toUpperCase",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                TAG: /* Eq */0,
                                                                                                                _0: "FUBAR",
                                                                                                                _1: "fubar".toUpperCase()
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  tl: {
                                                                                                    hd: [
                                                                                                      "toLocaleUpperCase",
                                                                                                      (function (param) {
                                                                                                          return {
                                                                                                                  TAG: /* Eq */0,
                                                                                                                  _0: "FUBAR",
                                                                                                                  _1: "fubar".toLocaleUpperCase()
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    tl: {
                                                                                                      hd: [
                                                                                                        "trim",
                                                                                                        (function (param) {
                                                                                                            return {
                                                                                                                    TAG: /* Eq */0,
                                                                                                                    _0: "foo",
                                                                                                                    _1: "  foo  ".trim()
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      tl: {
                                                                                                        hd: [
                                                                                                          "anchor",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      TAG: /* Eq */0,
                                                                                                                      _0: "<a name=\"bar\">foo</a>",
                                                                                                                      _1: "foo".anchor("bar")
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        tl: {
                                                                                                          hd: [
                                                                                                            "link",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        TAG: /* Eq */0,
                                                                                                                        _0: "<a href=\"https://reason.ml\">foo</a>",
                                                                                                                        _1: "foo".link("https://reason.ml")
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          tl: {
                                                                                                            hd: [
                                                                                                              "File \"js_string_test.ml\", line 214, characters 4-11",
                                                                                                              (function (param) {
                                                                                                                  return {
                                                                                                                          TAG: /* Ok */4,
                                                                                                                          _0: "ab".includes("a")
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
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
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_string_test", suites);

exports.suites = suites;
/*  Not a pure module */
