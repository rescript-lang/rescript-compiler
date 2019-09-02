'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "make",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "null",
                Arg1: String(null).concat("")
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "fromCharCode",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: "a",
                  Arg1: String.fromCharCode(97)
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "fromCharCodeMany",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: "az",
                    Arg1: String.fromCharCode(97, 122)
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "fromCodePoint",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: "a",
                      Arg1: String.fromCodePoint(97)
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "fromCodePointMany",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: "az",
                        Arg1: String.fromCodePoint(97, 122)
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "length",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: 3,
                          Arg1: "foo".length
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "get",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: "a",
                            Arg1: "foobar"[4]
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "charAt",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: "a",
                              Arg1: "foobar".charAt(4)
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "charCodeAt",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: 97,
                                Arg1: "foobar".charCodeAt(4)
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "codePointAt",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: 97,
                                  Arg1: Caml_option.undefined_to_opt("foobar".codePointAt(4))
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "codePointAt - out of bounds",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: undefined,
                                    Arg1: Caml_option.undefined_to_opt("foobar".codePointAt(98))
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "concat",
                          (function (param) {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: "foobar",
                                      Arg1: "foo".concat("bar")
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "concatMany",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: "foobarbaz",
                                        Arg1: "foo".concat("bar", "baz")
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "endsWith",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: true,
                                          Arg1: "foobar".endsWith("bar")
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "endsWithFrom",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: false,
                                            Arg1: "foobar".endsWith("bar", 1)
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "includes",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: true,
                                              Arg1: "foobarbaz".includes("bar")
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "includesFrom",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: false,
                                                Arg1: "foobarbaz".includes("bar", 4)
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "indexOf",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: 3,
                                                  Arg1: "foobarbaz".indexOf("bar")
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "indexOfFrom",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: -1,
                                                    Arg1: "foobarbaz".indexOf("bar", 4)
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "lastIndexOf",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "Eq",
                                                      Arg0: 3,
                                                      Arg1: "foobarbaz".lastIndexOf("bar")
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "lastIndexOfFrom",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "Eq",
                                                        Arg0: 3,
                                                        Arg1: "foobarbaz".lastIndexOf("bar", 4)
                                                      };
                                              })
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "localeCompare",
                                              (function (param) {
                                                  return /* constructor */{
                                                          tag: "Eq",
                                                          Arg0: 0,
                                                          Arg1: "foo".localeCompare("foo")
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "match",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "Eq",
                                                            Arg0: /* array */[
                                                              "na",
                                                              "na"
                                                            ],
                                                            Arg1: Caml_option.null_to_opt("banana".match((/na+/g)))
                                                          };
                                                  })
                                              ],
                                              Arg1: /* constructor */{
                                                tag: "::",
                                                Arg0: /* tuple */[
                                                  "match - no match",
                                                  (function (param) {
                                                      return /* constructor */{
                                                              tag: "Eq",
                                                              Arg0: undefined,
                                                              Arg1: Caml_option.null_to_opt("banana".match((/nanana+/g)))
                                                            };
                                                    })
                                                ],
                                                Arg1: /* constructor */{
                                                  tag: "::",
                                                  Arg0: /* tuple */[
                                                    "normalize",
                                                    (function (param) {
                                                        return /* constructor */{
                                                                tag: "Eq",
                                                                Arg0: "foo",
                                                                Arg1: "foo".normalize()
                                                              };
                                                      })
                                                  ],
                                                  Arg1: /* constructor */{
                                                    tag: "::",
                                                    Arg0: /* tuple */[
                                                      "normalizeByForm",
                                                      (function (param) {
                                                          return /* constructor */{
                                                                  tag: "Eq",
                                                                  Arg0: "foo",
                                                                  Arg1: "foo".normalize("NFKD")
                                                                };
                                                        })
                                                    ],
                                                    Arg1: /* constructor */{
                                                      tag: "::",
                                                      Arg0: /* tuple */[
                                                        "repeat",
                                                        (function (param) {
                                                            return /* constructor */{
                                                                    tag: "Eq",
                                                                    Arg0: "foofoofoo",
                                                                    Arg1: "foo".repeat(3)
                                                                  };
                                                          })
                                                      ],
                                                      Arg1: /* constructor */{
                                                        tag: "::",
                                                        Arg0: /* tuple */[
                                                          "replace",
                                                          (function (param) {
                                                              return /* constructor */{
                                                                      tag: "Eq",
                                                                      Arg0: "fooBORKbaz",
                                                                      Arg1: "foobarbaz".replace("bar", "BORK")
                                                                    };
                                                            })
                                                        ],
                                                        Arg1: /* constructor */{
                                                          tag: "::",
                                                          Arg0: /* tuple */[
                                                            "replaceByRe",
                                                            (function (param) {
                                                                return /* constructor */{
                                                                        tag: "Eq",
                                                                        Arg0: "fooBORKBORK",
                                                                        Arg1: "foobarbaz".replace((/ba./g), "BORK")
                                                                      };
                                                              })
                                                          ],
                                                          Arg1: /* constructor */{
                                                            tag: "::",
                                                            Arg0: /* tuple */[
                                                              "unsafeReplaceBy0",
                                                              (function (param) {
                                                                  var replace = function (whole, offset, s) {
                                                                    if (whole === "bar") {
                                                                      return "BORK";
                                                                    } else {
                                                                      return "DORK";
                                                                    }
                                                                  };
                                                                  return /* constructor */{
                                                                          tag: "Eq",
                                                                          Arg0: "fooBORKDORK",
                                                                          Arg1: "foobarbaz".replace((/ba./g), replace)
                                                                        };
                                                                })
                                                            ],
                                                            Arg1: /* constructor */{
                                                              tag: "::",
                                                              Arg0: /* tuple */[
                                                                "unsafeReplaceBy1",
                                                                (function (param) {
                                                                    var replace = function (whole, p1, offset, s) {
                                                                      if (whole === "bar") {
                                                                        return "BORK";
                                                                      } else {
                                                                        return "DORK";
                                                                      }
                                                                    };
                                                                    return /* constructor */{
                                                                            tag: "Eq",
                                                                            Arg0: "fooBORKDORK",
                                                                            Arg1: "foobarbaz".replace((/ba./g), replace)
                                                                          };
                                                                  })
                                                              ],
                                                              Arg1: /* constructor */{
                                                                tag: "::",
                                                                Arg0: /* tuple */[
                                                                  "unsafeReplaceBy2",
                                                                  (function (param) {
                                                                      var replace = function (whole, p1, p2, offset, s) {
                                                                        if (whole === "bar") {
                                                                          return "BORK";
                                                                        } else {
                                                                          return "DORK";
                                                                        }
                                                                      };
                                                                      return /* constructor */{
                                                                              tag: "Eq",
                                                                              Arg0: "fooBORKDORK",
                                                                              Arg1: "foobarbaz".replace((/ba./g), replace)
                                                                            };
                                                                    })
                                                                ],
                                                                Arg1: /* constructor */{
                                                                  tag: "::",
                                                                  Arg0: /* tuple */[
                                                                    "unsafeReplaceBy3",
                                                                    (function (param) {
                                                                        var replace = function (whole, p1, p2, p3, offset, s) {
                                                                          if (whole === "bar") {
                                                                            return "BORK";
                                                                          } else {
                                                                            return "DORK";
                                                                          }
                                                                        };
                                                                        return /* constructor */{
                                                                                tag: "Eq",
                                                                                Arg0: "fooBORKDORK",
                                                                                Arg1: "foobarbaz".replace((/ba./g), replace)
                                                                              };
                                                                      })
                                                                  ],
                                                                  Arg1: /* constructor */{
                                                                    tag: "::",
                                                                    Arg0: /* tuple */[
                                                                      "search",
                                                                      (function (param) {
                                                                          return /* constructor */{
                                                                                  tag: "Eq",
                                                                                  Arg0: 3,
                                                                                  Arg1: "foobarbaz".search((/ba./g))
                                                                                };
                                                                        })
                                                                    ],
                                                                    Arg1: /* constructor */{
                                                                      tag: "::",
                                                                      Arg0: /* tuple */[
                                                                        "slice",
                                                                        (function (param) {
                                                                            return /* constructor */{
                                                                                    tag: "Eq",
                                                                                    Arg0: "bar",
                                                                                    Arg1: "foobarbaz".slice(3, 6)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      Arg1: /* constructor */{
                                                                        tag: "::",
                                                                        Arg0: /* tuple */[
                                                                          "sliceToEnd",
                                                                          (function (param) {
                                                                              return /* constructor */{
                                                                                      tag: "Eq",
                                                                                      Arg0: "barbaz",
                                                                                      Arg1: "foobarbaz".slice(3)
                                                                                    };
                                                                            })
                                                                        ],
                                                                        Arg1: /* constructor */{
                                                                          tag: "::",
                                                                          Arg0: /* tuple */[
                                                                            "split",
                                                                            (function (param) {
                                                                                return /* constructor */{
                                                                                        tag: "Eq",
                                                                                        Arg0: /* array */[
                                                                                          "foo",
                                                                                          "bar",
                                                                                          "baz"
                                                                                        ],
                                                                                        Arg1: "foo bar baz".split(" ")
                                                                                      };
                                                                              })
                                                                          ],
                                                                          Arg1: /* constructor */{
                                                                            tag: "::",
                                                                            Arg0: /* tuple */[
                                                                              "splitAtMost",
                                                                              (function (param) {
                                                                                  return /* constructor */{
                                                                                          tag: "Eq",
                                                                                          Arg0: /* array */[
                                                                                            "foo",
                                                                                            "bar"
                                                                                          ],
                                                                                          Arg1: "foo bar baz".split(" ", 2)
                                                                                        };
                                                                                })
                                                                            ],
                                                                            Arg1: /* constructor */{
                                                                              tag: "::",
                                                                              Arg0: /* tuple */[
                                                                                "splitByRe",
                                                                                (function (param) {
                                                                                    return /* constructor */{
                                                                                            tag: "Eq",
                                                                                            Arg0: /* array */[
                                                                                              "a",
                                                                                              "#",
                                                                                              undefined,
                                                                                              "b",
                                                                                              "#",
                                                                                              ":",
                                                                                              "c"
                                                                                            ],
                                                                                            Arg1: "a#b#:c".split((/(#)(:)?/))
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              Arg1: /* constructor */{
                                                                                tag: "::",
                                                                                Arg0: /* tuple */[
                                                                                  "splitByReAtMost",
                                                                                  (function (param) {
                                                                                      return /* constructor */{
                                                                                              tag: "Eq",
                                                                                              Arg0: /* array */[
                                                                                                "a",
                                                                                                "#",
                                                                                                undefined
                                                                                              ],
                                                                                              Arg1: "a#b#:c".split((/(#)(:)?/), 3)
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                Arg1: /* constructor */{
                                                                                  tag: "::",
                                                                                  Arg0: /* tuple */[
                                                                                    "startsWith",
                                                                                    (function (param) {
                                                                                        return /* constructor */{
                                                                                                tag: "Eq",
                                                                                                Arg0: true,
                                                                                                Arg1: "foobarbaz".startsWith("foo")
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  Arg1: /* constructor */{
                                                                                    tag: "::",
                                                                                    Arg0: /* tuple */[
                                                                                      "startsWithFrom",
                                                                                      (function (param) {
                                                                                          return /* constructor */{
                                                                                                  tag: "Eq",
                                                                                                  Arg0: false,
                                                                                                  Arg1: "foobarbaz".startsWith("foo", 1)
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    Arg1: /* constructor */{
                                                                                      tag: "::",
                                                                                      Arg0: /* tuple */[
                                                                                        "substr",
                                                                                        (function (param) {
                                                                                            return /* constructor */{
                                                                                                    tag: "Eq",
                                                                                                    Arg0: "barbaz",
                                                                                                    Arg1: "foobarbaz".substr(3)
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      Arg1: /* constructor */{
                                                                                        tag: "::",
                                                                                        Arg0: /* tuple */[
                                                                                          "substrAtMost",
                                                                                          (function (param) {
                                                                                              return /* constructor */{
                                                                                                      tag: "Eq",
                                                                                                      Arg0: "bar",
                                                                                                      Arg1: "foobarbaz".substr(3, 3)
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        Arg1: /* constructor */{
                                                                                          tag: "::",
                                                                                          Arg0: /* tuple */[
                                                                                            "substring",
                                                                                            (function (param) {
                                                                                                return /* constructor */{
                                                                                                        tag: "Eq",
                                                                                                        Arg0: "bar",
                                                                                                        Arg1: "foobarbaz".substring(3, 6)
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          Arg1: /* constructor */{
                                                                                            tag: "::",
                                                                                            Arg0: /* tuple */[
                                                                                              "substringToEnd",
                                                                                              (function (param) {
                                                                                                  return /* constructor */{
                                                                                                          tag: "Eq",
                                                                                                          Arg0: "barbaz",
                                                                                                          Arg1: "foobarbaz".substring(3)
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            Arg1: /* constructor */{
                                                                                              tag: "::",
                                                                                              Arg0: /* tuple */[
                                                                                                "toLowerCase",
                                                                                                (function (param) {
                                                                                                    return /* constructor */{
                                                                                                            tag: "Eq",
                                                                                                            Arg0: "bork",
                                                                                                            Arg1: "BORK".toLowerCase()
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              Arg1: /* constructor */{
                                                                                                tag: "::",
                                                                                                Arg0: /* tuple */[
                                                                                                  "toLocaleLowerCase",
                                                                                                  (function (param) {
                                                                                                      return /* constructor */{
                                                                                                              tag: "Eq",
                                                                                                              Arg0: "bork",
                                                                                                              Arg1: "BORK".toLocaleLowerCase()
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                Arg1: /* constructor */{
                                                                                                  tag: "::",
                                                                                                  Arg0: /* tuple */[
                                                                                                    "toUpperCase",
                                                                                                    (function (param) {
                                                                                                        return /* constructor */{
                                                                                                                tag: "Eq",
                                                                                                                Arg0: "FUBAR",
                                                                                                                Arg1: "fubar".toUpperCase()
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  Arg1: /* constructor */{
                                                                                                    tag: "::",
                                                                                                    Arg0: /* tuple */[
                                                                                                      "toLocaleUpperCase",
                                                                                                      (function (param) {
                                                                                                          return /* constructor */{
                                                                                                                  tag: "Eq",
                                                                                                                  Arg0: "FUBAR",
                                                                                                                  Arg1: "fubar".toLocaleUpperCase()
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    Arg1: /* constructor */{
                                                                                                      tag: "::",
                                                                                                      Arg0: /* tuple */[
                                                                                                        "trim",
                                                                                                        (function (param) {
                                                                                                            return /* constructor */{
                                                                                                                    tag: "Eq",
                                                                                                                    Arg0: "foo",
                                                                                                                    Arg1: "  foo  ".trim()
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      Arg1: /* constructor */{
                                                                                                        tag: "::",
                                                                                                        Arg0: /* tuple */[
                                                                                                          "anchor",
                                                                                                          (function (param) {
                                                                                                              return /* constructor */{
                                                                                                                      tag: "Eq",
                                                                                                                      Arg0: "<a name=\"bar\">foo</a>",
                                                                                                                      Arg1: "foo".anchor("bar")
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        Arg1: /* constructor */{
                                                                                                          tag: "::",
                                                                                                          Arg0: /* tuple */[
                                                                                                            "link",
                                                                                                            (function (param) {
                                                                                                                return /* constructor */{
                                                                                                                        tag: "Eq",
                                                                                                                        Arg0: "<a href=\"https://reason.ml\">foo</a>",
                                                                                                                        Arg1: "foo".link("https://reason.ml")
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          Arg1: /* constructor */{
                                                                                                            tag: "::",
                                                                                                            Arg0: /* tuple */[
                                                                                                              "File \"js_string_test.ml\", line 211, characters 4-11",
                                                                                                              (function (param) {
                                                                                                                  return /* constructor */{
                                                                                                                          tag: "Ok",
                                                                                                                          Arg0: "ab".includes("a")
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            Arg1: "[]"
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

Mt.from_pair_suites("Js_string_test", suites);

exports.suites = suites;
/*  Not a pure module */
