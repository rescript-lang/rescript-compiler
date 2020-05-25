'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = /* tuple */[
  "make",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: "null",
              _1: String(null).concat("")
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "fromCharCode",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: "a",
                _1: String.fromCharCode(97)
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "fromCharCodeMany",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: "az",
                  _1: String.fromCharCode(97, 122)
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "fromCodePoint",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: "a",
                    _1: String.fromCodePoint(97)
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "fromCodePointMany",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: "az",
                      _1: String.fromCodePoint(97, 122)
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "length",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: 3,
                        _1: "foo".length
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "get",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: "a",
                          _1: "foobar"[4]
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "charAt",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: "a",
                            _1: "foobar".charAt(4)
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "charCodeAt",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: 97,
                              _1: "foobar".charCodeAt(4)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "codePointAt",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: 97,
                                _1: "foobar".codePointAt(4)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "codePointAt - out of bounds",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: undefined,
                                  _1: "foobar".codePointAt(98)
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "concat",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: "foobar",
                                    _1: "foo".concat("bar")
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "concatMany",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: "foobarbaz",
                                      _1: "foo".concat("bar", "baz")
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "endsWith",
                            (function (param) {
                                return {
                                        tag: /* Eq */0,
                                        _0: true,
                                        _1: "foobar".endsWith("bar")
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: /* tuple */[
                              "endsWithFrom",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
                                          _0: false,
                                          _1: "foobar".endsWith("bar", 1)
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: /* tuple */[
                                "includes",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
                                            _0: true,
                                            _1: "foobarbaz".includes("bar")
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: /* tuple */[
                                  "includesFrom",
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
                                              _0: false,
                                              _1: "foobarbaz".includes("bar", 4)
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: /* tuple */[
                                    "indexOf",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: 3,
                                                _1: "foobarbaz".indexOf("bar")
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "indexOfFrom",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: -1,
                                                  _1: "foobarbaz".indexOf("bar", 4)
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "lastIndexOf",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
                                                    _0: 3,
                                                    _1: "foobarbaz".lastIndexOf("bar")
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "lastIndexOfFrom",
                                          (function (param) {
                                              return {
                                                      tag: /* Eq */0,
                                                      _0: 3,
                                                      _1: "foobarbaz".lastIndexOf("bar", 4)
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "localeCompare",
                                            (function (param) {
                                                return {
                                                        tag: /* Eq */0,
                                                        _0: 0,
                                                        _1: "foo".localeCompare("foo")
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "match",
                                              (function (param) {
                                                  return {
                                                          tag: /* Eq */0,
                                                          _0: [
                                                            "na",
                                                            "na"
                                                          ],
                                                          _1: Caml_option.null_to_opt("banana".match(/na+/g))
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: /* tuple */[
                                                "match - no match",
                                                (function (param) {
                                                    return {
                                                            tag: /* Eq */0,
                                                            _0: undefined,
                                                            _1: Caml_option.null_to_opt("banana".match(/nanana+/g))
                                                          };
                                                  })
                                              ],
                                              _1: /* :: */{
                                                _0: /* tuple */[
                                                  "normalize",
                                                  (function (param) {
                                                      return {
                                                              tag: /* Eq */0,
                                                              _0: "foo",
                                                              _1: "foo".normalize()
                                                            };
                                                    })
                                                ],
                                                _1: /* :: */{
                                                  _0: /* tuple */[
                                                    "normalizeByForm",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
                                                                _0: "foo",
                                                                _1: "foo".normalize("NFKD")
                                                              };
                                                      })
                                                  ],
                                                  _1: /* :: */{
                                                    _0: /* tuple */[
                                                      "repeat",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* Eq */0,
                                                                  _0: "foofoofoo",
                                                                  _1: "foo".repeat(3)
                                                                };
                                                        })
                                                    ],
                                                    _1: /* :: */{
                                                      _0: /* tuple */[
                                                        "replace",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* Eq */0,
                                                                    _0: "fooBORKbaz",
                                                                    _1: "foobarbaz".replace("bar", "BORK")
                                                                  };
                                                          })
                                                      ],
                                                      _1: /* :: */{
                                                        _0: /* tuple */[
                                                          "replaceByRe",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Eq */0,
                                                                      _0: "fooBORKBORK",
                                                                      _1: "foobarbaz".replace(/ba./g, "BORK")
                                                                    };
                                                            })
                                                        ],
                                                        _1: /* :: */{
                                                          _0: /* tuple */[
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
                                                                        tag: /* Eq */0,
                                                                        _0: "fooBORKDORK",
                                                                        _1: "foobarbaz".replace(/ba./g, replace)
                                                                      };
                                                              })
                                                          ],
                                                          _1: /* :: */{
                                                            _0: /* tuple */[
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
                                                                          tag: /* Eq */0,
                                                                          _0: "fooBORKDORK",
                                                                          _1: "foobarbaz".replace(/ba./g, replace)
                                                                        };
                                                                })
                                                            ],
                                                            _1: /* :: */{
                                                              _0: /* tuple */[
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
                                                                            tag: /* Eq */0,
                                                                            _0: "fooBORKDORK",
                                                                            _1: "foobarbaz".replace(/ba./g, replace)
                                                                          };
                                                                  })
                                                              ],
                                                              _1: /* :: */{
                                                                _0: /* tuple */[
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
                                                                              tag: /* Eq */0,
                                                                              _0: "fooBORKDORK",
                                                                              _1: "foobarbaz".replace(/ba./g, replace)
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: /* tuple */[
                                                                    "search",
                                                                    (function (param) {
                                                                        return {
                                                                                tag: /* Eq */0,
                                                                                _0: 3,
                                                                                _1: "foobarbaz".search(/ba./g)
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: /* tuple */[
                                                                      "slice",
                                                                      (function (param) {
                                                                          return {
                                                                                  tag: /* Eq */0,
                                                                                  _0: "bar",
                                                                                  _1: "foobarbaz".slice(3, 6)
                                                                                };
                                                                        })
                                                                    ],
                                                                    _1: /* :: */{
                                                                      _0: /* tuple */[
                                                                        "sliceToEnd",
                                                                        (function (param) {
                                                                            return {
                                                                                    tag: /* Eq */0,
                                                                                    _0: "barbaz",
                                                                                    _1: "foobarbaz".slice(3)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: /* tuple */[
                                                                          "split",
                                                                          (function (param) {
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: [
                                                                                        "foo",
                                                                                        "bar",
                                                                                        "baz"
                                                                                      ],
                                                                                      _1: "foo bar baz".split(" ")
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: /* tuple */[
                                                                            "splitAtMost",
                                                                            (function (param) {
                                                                                return {
                                                                                        tag: /* Eq */0,
                                                                                        _0: [
                                                                                          "foo",
                                                                                          "bar"
                                                                                        ],
                                                                                        _1: "foo bar baz".split(" ", 2)
                                                                                      };
                                                                              })
                                                                          ],
                                                                          _1: /* :: */{
                                                                            _0: /* tuple */[
                                                                              "splitByRe",
                                                                              (function (param) {
                                                                                  return {
                                                                                          tag: /* Eq */0,
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
                                                                            _1: /* :: */{
                                                                              _0: /* tuple */[
                                                                                "splitByReAtMost",
                                                                                (function (param) {
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: [
                                                                                              "a",
                                                                                              "#",
                                                                                              undefined
                                                                                            ],
                                                                                            _1: "a#b#:c".split(/(#)(:)?/, 3)
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: /* tuple */[
                                                                                  "startsWith",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: true,
                                                                                              _1: "foobarbaz".startsWith("foo")
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: /* tuple */[
                                                                                    "startsWithFrom",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                tag: /* Eq */0,
                                                                                                _0: false,
                                                                                                _1: "foobarbaz".startsWith("foo", 1)
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  _1: /* :: */{
                                                                                    _0: /* tuple */[
                                                                                      "substr",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: "barbaz",
                                                                                                  _1: "foobarbaz".substr(3)
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    _1: /* :: */{
                                                                                      _0: /* tuple */[
                                                                                        "substrAtMost",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    tag: /* Eq */0,
                                                                                                    _0: "bar",
                                                                                                    _1: "foobarbaz".substr(3, 3)
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      _1: /* :: */{
                                                                                        _0: /* tuple */[
                                                                                          "substring",
                                                                                          (function (param) {
                                                                                              return {
                                                                                                      tag: /* Eq */0,
                                                                                                      _0: "bar",
                                                                                                      _1: "foobarbaz".substring(3, 6)
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        _1: /* :: */{
                                                                                          _0: /* tuple */[
                                                                                            "substringToEnd",
                                                                                            (function (param) {
                                                                                                return {
                                                                                                        tag: /* Eq */0,
                                                                                                        _0: "barbaz",
                                                                                                        _1: "foobarbaz".substring(3)
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          _1: /* :: */{
                                                                                            _0: /* tuple */[
                                                                                              "toLowerCase",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          tag: /* Eq */0,
                                                                                                          _0: "bork",
                                                                                                          _1: "BORK".toLowerCase()
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            _1: /* :: */{
                                                                                              _0: /* tuple */[
                                                                                                "toLocaleLowerCase",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
                                                                                                            _0: "bork",
                                                                                                            _1: "BORK".toLocaleLowerCase()
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              _1: /* :: */{
                                                                                                _0: /* tuple */[
                                                                                                  "toUpperCase",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
                                                                                                              _0: "FUBAR",
                                                                                                              _1: "fubar".toUpperCase()
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                _1: /* :: */{
                                                                                                  _0: /* tuple */[
                                                                                                    "toLocaleUpperCase",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                tag: /* Eq */0,
                                                                                                                _0: "FUBAR",
                                                                                                                _1: "fubar".toLocaleUpperCase()
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  _1: /* :: */{
                                                                                                    _0: /* tuple */[
                                                                                                      "trim",
                                                                                                      (function (param) {
                                                                                                          return {
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: "foo",
                                                                                                                  _1: "  foo  ".trim()
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: /* tuple */[
                                                                                                        "anchor",
                                                                                                        (function (param) {
                                                                                                            return {
                                                                                                                    tag: /* Eq */0,
                                                                                                                    _0: "<a name=\"bar\">foo</a>",
                                                                                                                    _1: "foo".anchor("bar")
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: /* tuple */[
                                                                                                          "link",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      tag: /* Eq */0,
                                                                                                                      _0: "<a href=\"https://reason.ml\">foo</a>",
                                                                                                                      _1: "foo".link("https://reason.ml")
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        _1: /* :: */{
                                                                                                          _0: /* tuple */[
                                                                                                            "File \"js_string_test.ml\", line 211, characters 4-11",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        tag: /* Ok */4,
                                                                                                                        _0: "ab".includes("a")
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          _1: /* [] */0
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

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_string_test", suites);

exports.suites = suites;
/*  Not a pure module */
