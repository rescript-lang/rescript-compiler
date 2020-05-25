'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
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
  _0: [
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
    _0: [
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
      _0: [
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
        _0: [
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
          _0: [
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
            _0: [
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
              _0: [
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
                _0: [
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
                  _0: [
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
                    _0: [
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
                      _0: [
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
                        _0: [
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
                          _0: [
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
                            _0: [
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
                              _0: [
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
                                _0: [
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
                                  _0: [
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
                                    _0: [
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
                                      _0: [
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
                                        _0: [
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
                                          _0: [
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
                                            _0: [
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
                                              _0: [
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
                                                _0: [
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
                                                  _0: [
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
                                                    _0: [
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
                                                      _0: [
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
                                                        _0: [
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
                                                          _0: [
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
                                                            _0: [
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
                                                              _0: [
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
                                                                _0: [
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
                                                                  _0: [
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
                                                                    _0: [
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
                                                                      _0: [
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
                                                                        _0: [
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
                                                                          _0: [
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
                                                                            _0: [
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
                                                                              _0: [
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
                                                                                _0: [
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
                                                                                  _0: [
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
                                                                                    _0: [
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
                                                                                      _0: [
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
                                                                                        _0: [
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
                                                                                          _0: [
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
                                                                                            _0: [
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
                                                                                              _0: [
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
                                                                                                _0: [
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
                                                                                                  _0: [
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
                                                                                                    _0: [
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
                                                                                                      _0: [
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
                                                                                                        _0: [
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
                                                                                                          _0: [
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
