'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites_000 = /* tuple */[
  "make",
  (function () {
      return /* Eq */Block.__(0, [
                "null",
                String(null).concat("")
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "fromCharCode",
    (function () {
        return /* Eq */Block.__(0, [
                  "a",
                  String.fromCharCode(97)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "fromCharCodeMany",
      (function () {
          return /* Eq */Block.__(0, [
                    "az",
                    String.fromCharCode(97, 122)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "fromCodePoint",
        (function () {
            return /* Eq */Block.__(0, [
                      "a",
                      String.fromCodePoint(97)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "fromCodePointMany",
          (function () {
              return /* Eq */Block.__(0, [
                        "az",
                        String.fromCodePoint(97, 122)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "length",
            (function () {
                return /* Eq */Block.__(0, [
                          3,
                          "foo".length
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "get",
              (function () {
                  return /* Eq */Block.__(0, [
                            "a",
                            "foobar"[4]
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "charAt",
                (function () {
                    return /* Eq */Block.__(0, [
                              "a",
                              "foobar".charAt(4)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "charCodeAt",
                  (function () {
                      return /* Eq */Block.__(0, [
                                97,
                                "foobar".charCodeAt(4)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "codePointAt",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  /* Some */[97],
                                  Js_primitive.undefined_to_opt("foobar".codePointAt(4))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "codePointAt - out of bounds",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    /* None */0,
                                    Js_primitive.undefined_to_opt("foobar".codePointAt(98))
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "concat",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      "foobar",
                                      "foo".concat("bar")
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "concatMany",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        "foobarbaz",
                                        "foo".concat("bar", "baz")
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "endsWith",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          /* true */1,
                                          +"foobar".endsWith("bar")
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "endsWithFrom",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            /* false */0,
                                            +"foobar".endsWith("bar", 1)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "includes",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              /* true */1,
                                              +"foobarbaz".includes("bar")
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "includesFrom",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                /* false */0,
                                                +"foobarbaz".includes("bar", 4)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "indexOf",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  3,
                                                  "foobarbaz".indexOf("bar")
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "indexOfFrom",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    -1,
                                                    "foobarbaz".indexOf("bar", 4)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "lastIndexOf",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      3,
                                                      "foobarbaz".lastIndexOf("bar")
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "lastIndexOfFrom",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        3,
                                                        "foobarbaz".lastIndexOf("bar", 4)
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "localeCompare",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          0,
                                                          "foo".localeCompare("foo")
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "match",
                                              (function () {
                                                  return /* Eq */Block.__(0, [
                                                            /* Some */[/* array */[
                                                                "na",
                                                                "na"
                                                              ]],
                                                            Js_primitive.null_to_opt("banana".match((/na+/g)))
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "match - no match",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              /* None */0,
                                                              Js_primitive.null_to_opt("banana".match((/nanana+/g)))
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "normalize",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                "foo",
                                                                "foo".normalize()
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "normalizeByForm",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  "foo",
                                                                  "foo".normalize("NFKD")
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "repeat",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    "foofoofoo",
                                                                    "foo".repeat(3)
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "replace",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      "fooBORKbaz",
                                                                      "foobarbaz".replace("bar", "BORK")
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "replaceByRe",
                                                          (function () {
                                                              return /* Eq */Block.__(0, [
                                                                        "fooBORKBORK",
                                                                        "foobarbaz".replace((/ba./g), "BORK")
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "unsafeReplaceBy0",
                                                            (function () {
                                                                var replace = function (whole, _, _$1) {
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
                                                              (function () {
                                                                  var replace = function (whole, _, _$1, _$2) {
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
                                                                (function () {
                                                                    var replace = function (whole, _, _$1, _$2, _$3) {
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
                                                                  (function () {
                                                                      var replace = function (whole, _, _$1, _$2, _$3, _$4) {
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
                                                                    (function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  3,
                                                                                  "foobarbaz".search((/ba./g))
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "slice",
                                                                      (function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    "bar",
                                                                                    "foobarbaz".slice(3, 6)
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "sliceToEnd",
                                                                        (function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      "barbaz",
                                                                                      "foobarbaz".slice(3)
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "split",
                                                                          (function () {
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
                                                                            (function () {
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
                                                                              (function () {
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
                                                                                (function () {
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
                                                                                  (function () {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                /* true */1,
                                                                                                +"foobarbaz".startsWith("foo")
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "startsWithFrom",
                                                                                    (function () {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  /* false */0,
                                                                                                  +"foobarbaz".startsWith("foo", 1)
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "substr",
                                                                                      (function () {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    "barbaz",
                                                                                                    "foobarbaz".substr(3)
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "substrAtMost",
                                                                                        (function () {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      "bar",
                                                                                                      "foobarbaz".substr(3, 3)
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "substring",
                                                                                          (function () {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        "bar",
                                                                                                        "foobarbaz".substring(3, 6)
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "substringToEnd",
                                                                                            (function () {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          "barbaz",
                                                                                                          "foobarbaz".substring(3)
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "toLowerCase",
                                                                                              (function () {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            "bork",
                                                                                                            "BORK".toLowerCase()
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "toLocaleLowerCase",
                                                                                                (function () {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              "bork",
                                                                                                              "BORK".toLocaleLowerCase()
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "toUpperCase",
                                                                                                  (function () {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                "FUBAR",
                                                                                                                "fubar".toUpperCase()
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "toLocaleUpperCase",
                                                                                                    (function () {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  "FUBAR",
                                                                                                                  "fubar".toLocaleUpperCase()
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "trim",
                                                                                                      (function () {
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    "foo",
                                                                                                                    "  foo  ".trim()
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "anchor",
                                                                                                        (function () {
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      "<a name=\"bar\">foo</a>",
                                                                                                                      "foo".anchor("bar")
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "link",
                                                                                                          (function () {
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        "<a href=\"https://reason.ml\">foo</a>",
                                                                                                                        "foo".link("https://reason.ml")
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "File \"js_string_test.ml\", line 211, characters 4-11",
                                                                                                            (function () {
                                                                                                                return /* Ok */Block.__(4, [+"ab".includes("a")]);
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

Mt.from_pair_suites("js_string_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
