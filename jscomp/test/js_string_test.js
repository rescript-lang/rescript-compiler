'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_000 = /* tuple */[
  "make",
  (function (param) {
      return /* Eq */Block.__(0, [
                "null",
                String(null).concat("")
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "fromCharCode",
    (function (param) {
        return /* Eq */Block.__(0, [
                  "a",
                  String.fromCharCode(97)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "fromCharCodeMany",
      (function (param) {
          return /* Eq */Block.__(0, [
                    "az",
                    String.fromCharCode(97, 122)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "fromCodePoint",
        (function (param) {
            return /* Eq */Block.__(0, [
                      "a",
                      String.fromCodePoint(97)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "fromCodePointMany",
          (function (param) {
              return /* Eq */Block.__(0, [
                        "az",
                        String.fromCodePoint(97, 122)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "length",
            (function (param) {
                return /* Eq */Block.__(0, [
                          3,
                          "foo".length
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "get",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            "a",
                            "foobar"[4]
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "charAt",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              "a",
                              "foobar".charAt(4)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "charCodeAt",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                97,
                                "foobar".charCodeAt(4)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "codePointAt",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  97,
                                  Caml_option.undefined_to_opt("foobar".codePointAt(4))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "codePointAt - out of bounds",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    undefined,
                                    Caml_option.undefined_to_opt("foobar".codePointAt(98))
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "concat",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      "foobar",
                                      "foo".concat("bar")
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "concatMany",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        "foobarbaz",
                                        "foo".concat("bar", "baz")
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "endsWith",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          true,
                                          "foobar".endsWith("bar")
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "endsWithFrom",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            false,
                                            "foobar".endsWith("bar", 1)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "includes",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              true,
                                              "foobarbaz".includes("bar")
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "includesFrom",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                false,
                                                "foobarbaz".includes("bar", 4)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "indexOf",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  3,
                                                  "foobarbaz".indexOf("bar")
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "indexOfFrom",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    -1,
                                                    "foobarbaz".indexOf("bar", 4)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "lastIndexOf",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      3,
                                                      "foobarbaz".lastIndexOf("bar")
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "lastIndexOfFrom",
                                          (function (param) {
                                              return /* Eq */Block.__(0, [
                                                        3,
                                                        "foobarbaz".lastIndexOf("bar", 4)
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "localeCompare",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          0,
                                                          "foo".localeCompare("foo")
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "match",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            /* array */[
                                                              "na",
                                                              "na"
                                                            ],
                                                            Caml_option.null_to_opt("banana".match((/na+/g)))
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "match - no match",
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              undefined,
                                                              Caml_option.null_to_opt("banana".match((/nanana+/g)))
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "normalize",
                                                  (function (param) {
                                                      return /* Eq */Block.__(0, [
                                                                "foo",
                                                                "foo".normalize()
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "normalizeByForm",
                                                    (function (param) {
                                                        return /* Eq */Block.__(0, [
                                                                  "foo",
                                                                  "foo".normalize("NFKD")
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "repeat",
                                                      (function (param) {
                                                          return /* Eq */Block.__(0, [
                                                                    "foofoofoo",
                                                                    "foo".repeat(3)
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "replace",
                                                        (function (param) {
                                                            return /* Eq */Block.__(0, [
                                                                      "fooBORKbaz",
                                                                      "foobarbaz".replace("bar", "BORK")
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "replaceByRe",
                                                          (function (param) {
                                                              return /* Eq */Block.__(0, [
                                                                        "fooBORKBORK",
                                                                        "foobarbaz".replace((/ba./g), "BORK")
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "unsafeReplaceBy0",
                                                            (function (param) {
                                                                var replace = function (whole, offset, s) {
                                                                  if (whole === "bar") {
                                                                    return "BORK";
                                                                  } else {
                                                                    return "DORK";
                                                                  }
                                                                };
                                                                return /* Eq */Block.__(0, [
                                                                          "fooBORKDORK",
                                                                          "foobarbaz".replace((/ba./g), replace)
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "unsafeReplaceBy1",
                                                              (function (param) {
                                                                  var replace = function (whole, p1, offset, s) {
                                                                    if (whole === "bar") {
                                                                      return "BORK";
                                                                    } else {
                                                                      return "DORK";
                                                                    }
                                                                  };
                                                                  return /* Eq */Block.__(0, [
                                                                            "fooBORKDORK",
                                                                            "foobarbaz".replace((/ba./g), replace)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "unsafeReplaceBy2",
                                                                (function (param) {
                                                                    var replace = function (whole, p1, p2, offset, s) {
                                                                      if (whole === "bar") {
                                                                        return "BORK";
                                                                      } else {
                                                                        return "DORK";
                                                                      }
                                                                    };
                                                                    return /* Eq */Block.__(0, [
                                                                              "fooBORKDORK",
                                                                              "foobarbaz".replace((/ba./g), replace)
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "unsafeReplaceBy3",
                                                                  (function (param) {
                                                                      var replace = function (whole, p1, p2, p3, offset, s) {
                                                                        if (whole === "bar") {
                                                                          return "BORK";
                                                                        } else {
                                                                          return "DORK";
                                                                        }
                                                                      };
                                                                      return /* Eq */Block.__(0, [
                                                                                "fooBORKDORK",
                                                                                "foobarbaz".replace((/ba./g), replace)
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "search",
                                                                    (function (param) {
                                                                        return /* Eq */Block.__(0, [
                                                                                  3,
                                                                                  "foobarbaz".search((/ba./g))
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "slice",
                                                                      (function (param) {
                                                                          return /* Eq */Block.__(0, [
                                                                                    "bar",
                                                                                    "foobarbaz".slice(3, 6)
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "sliceToEnd",
                                                                        (function (param) {
                                                                            return /* Eq */Block.__(0, [
                                                                                      "barbaz",
                                                                                      "foobarbaz".slice(3)
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "split",
                                                                          (function (param) {
                                                                              return /* Eq */Block.__(0, [
                                                                                        /* array */[
                                                                                          "foo",
                                                                                          "bar",
                                                                                          "baz"
                                                                                        ],
                                                                                        "foo bar baz".split(" ")
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "splitAtMost",
                                                                            (function (param) {
                                                                                return /* Eq */Block.__(0, [
                                                                                          /* array */[
                                                                                            "foo",
                                                                                            "bar"
                                                                                          ],
                                                                                          "foo bar baz".split(" ", 2)
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "splitByRe",
                                                                              (function (param) {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            /* array */[
                                                                                              "foo",
                                                                                              "bar",
                                                                                              "baz"
                                                                                            ],
                                                                                            "foo bar baz".split((/\s/))
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "splitByReAtMost",
                                                                                (function (param) {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              /* array */[
                                                                                                "foo",
                                                                                                "bar"
                                                                                              ],
                                                                                              "foo bar baz".split((/\s/), 2)
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "startsWith",
                                                                                  (function (param) {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                true,
                                                                                                "foobarbaz".startsWith("foo")
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "startsWithFrom",
                                                                                    (function (param) {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  false,
                                                                                                  "foobarbaz".startsWith("foo", 1)
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "substr",
                                                                                      (function (param) {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    "barbaz",
                                                                                                    "foobarbaz".substr(3)
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "substrAtMost",
                                                                                        (function (param) {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      "bar",
                                                                                                      "foobarbaz".substr(3, 3)
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "substring",
                                                                                          (function (param) {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        "bar",
                                                                                                        "foobarbaz".substring(3, 6)
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "substringToEnd",
                                                                                            (function (param) {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          "barbaz",
                                                                                                          "foobarbaz".substring(3)
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "toLowerCase",
                                                                                              (function (param) {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            "bork",
                                                                                                            "BORK".toLowerCase()
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "toLocaleLowerCase",
                                                                                                (function (param) {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              "bork",
                                                                                                              "BORK".toLocaleLowerCase()
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "toUpperCase",
                                                                                                  (function (param) {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                "FUBAR",
                                                                                                                "fubar".toUpperCase()
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "toLocaleUpperCase",
                                                                                                    (function (param) {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  "FUBAR",
                                                                                                                  "fubar".toLocaleUpperCase()
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "trim",
                                                                                                      (function (param) {
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    "foo",
                                                                                                                    "  foo  ".trim()
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "anchor",
                                                                                                        (function (param) {
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      "<a name=\"bar\">foo</a>",
                                                                                                                      "foo".anchor("bar")
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "link",
                                                                                                          (function (param) {
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        "<a href=\"https://reason.ml\">foo</a>",
                                                                                                                        "foo".link("https://reason.ml")
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "File \"js_string_test.ml\", line 211, characters 4-11",
                                                                                                            (function (param) {
                                                                                                                return /* Ok */Block.__(4, ["ab".includes("a")]);
                                                                                                              })
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Js_string_test", suites);

exports.suites = suites;
/*  Not a pure module */
