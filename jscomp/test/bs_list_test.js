'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function $$throw(loc, x) {
  return Mt.throw_suites(test_id, suites, loc, x);
}

function sum(xs) {
  var v = /* record */[/* contents */0];
  Belt_List.forEach(xs, (function (x) {
          v[0] = v[0] + x | 0;
          return /* () */0;
        }));
  return v[0];
}

function sum2(xs, ys) {
  var v = /* record */[/* contents */0];
  Belt_List.forEach2(xs, ys, (function (x, y) {
          v[0] = (v[0] + x | 0) + y | 0;
          return /* () */0;
        }));
  return v[0];
}

var u = Belt_List.makeBy(5, (function (i) {
        return Caml_int32.imul(i, i);
      }));

function f(i) {
  return eq("File \"bs_list_test.ml\", line 26, characters 7-14", Belt_List.getExn(u, i), Caml_int32.imul(i, i));
}

for(var i = 0; i <= 4; ++i){
  f(i);
}

eq("File \"bs_list_test.ml\", line 30, characters 5-12", Belt_List.map(u, (function (i) {
            return i + 1 | 0;
          })), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 5,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 10,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 17,
              Arg1: "[]"
            }
          }
        }
      }
    });

eq("File \"bs_list_test.ml\", line 31, characters 5-12", Belt_List.getBy(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: "[]"
              }
            }
          }
        }, (function (x) {
            return x % 2 === 0;
          })), 4);

eq("File \"bs_list_test.ml\", line 32, characters 5-12", Belt_List.getBy(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: "[]"
              }
            }
          }
        }, (function (x) {
            return x % 5 === 0;
          })), undefined);

eq("FLATTEN", Belt_List.flatten(/* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: "[]"
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: "[]"
            },
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "::",
                Arg0: 3,
                Arg1: "[]"
              },
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "[]",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: Belt_List.makeBy(4, (function (i) {
                          return i;
                        })),
                  Arg1: "[]"
                }
              }
            }
          }
        }), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 0,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 1,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: "[]"
                }
              }
            }
          }
        }
      }
    });

eq("FLATTEN", Belt_List.flatten("[]"), "[]");

eq("FLATTEN", Belt_List.flatten(/* constructor */{
          tag: "::",
          Arg0: "[]",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "[]",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: "[]"
              },
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* constructor */{
                  tag: "::",
                  Arg0: 1,
                  Arg1: "[]"
                },
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "::",
                    Arg0: 2,
                    Arg1: "[]"
                  },
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "[]",
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: "[]"
        }
      }
    });

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: "[]"
          },
          /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          },
          /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          },
          "[]",
          Belt_List.makeBy(4, (function (i) {
                  return i;
                }))
        ]), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 0,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 1,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: "[]"
                }
              }
            }
          }
        }
      }
    });

eq("CONCATMANY", Belt_List.concatMany(/* array */[]), "[]");

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          "[]",
          "[]",
          /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          },
          /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: "[]"
          },
          /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          },
          "[]"
        ]), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: "[]"
        }
      }
    });

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          "[]",
          "[]",
          /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          },
          /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: "[]"
          },
          /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          },
          "[]"
        ]), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }
      }
    });

eq("CONCATMANY", Belt_List.concatMany(/* array */[/* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 3,
                Arg1: "[]"
              }
            }
          }]), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: "[]"
        }
      }
    });

eq("File \"bs_list_test.ml\", line 57, characters 5-12", Belt_List.toArray(Belt_List.concat(Belt_List.makeBy(100, (function (i) {
                    return i;
                  })), Belt_List.makeBy(100, (function (i) {
                    return i;
                  })))), Belt_Array.concat(Belt_Array.makeBy(100, (function (i) {
                return i;
              })), Belt_Array.makeBy(100, (function (i) {
                return i;
              }))));

eq("APPEND", Belt_List.concat(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, "[]"), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: "[]"
    });

eq("APPEND", Belt_List.concat("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: "[]"
    });

eq("ZIP", Belt_List.zip(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        3
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          4
        ],
        Arg1: "[]"
      }
    });

eq("ZIP", Belt_List.zip("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }), "[]");

eq("ZIP", Belt_List.zip("[]", "[]"), "[]");

eq("ZIP", Belt_List.zip(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, "[]"), "[]");

eq("ZIP", Belt_List.zip(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        2
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          3
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            3,
            4
          ],
          Arg1: "[]"
        }
      }
    });

function mod2(x) {
  return x % 2 === 0;
}

function evenIndex(_x, i) {
  return i % 2 === 0;
}

eq("PARTITION", Belt_List.partition(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4,
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }, mod2), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }
      },
      /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }
      }
    ]);

eq("PARTITION", Belt_List.partition(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, mod2), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }
      },
      "[]"
    ]);

eq("PARTITION", Belt_List.partition(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, (function (x) {
            return !mod2(x);
          })), /* tuple */[
      "[]",
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }
      }
    ]);

eq("PARTITION", Belt_List.partition("[]", mod2), /* tuple */[
      "[]",
      "[]"
    ]);

eq("UNZIP", Belt_List.unzip("[]"), /* tuple */[
      "[]",
      "[]"
    ]);

eq("UNZIP", Belt_List.unzip(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            2
          ],
          Arg1: "[]"
        }), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: "[]"
      },
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: "[]"
      }
    ]);

eq("UNZIP", Belt_List.unzip(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            2
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              3,
              4
            ],
            Arg1: "[]"
          }
        }), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: "[]"
        }
      },
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: "[]"
        }
      }
    ]);

eq("FILTER", Belt_List.keep(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, mod2), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 4,
        Arg1: "[]"
      }
    });

eq("FILTER", Belt_List.keep(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 41,
              Arg1: "[]"
            }
          }
        }, mod2), "[]");

eq("FILTER", Belt_List.keep("[]", mod2), "[]");

eq("FILTER", Belt_List.keep(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 6,
                  Arg1: "[]"
                }
              }
            }
          }
        }, mod2), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 6,
              Arg1: "[]"
            }
          }
        }
      }
    });

eq("FILTER2", Belt_List.keepWithIndex("[]", evenIndex), "[]");

eq("FILTER2", Belt_List.keepWithIndex(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, evenIndex), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: "[]"
      }
    });

eq("FILTER2", Belt_List.keepWithIndex(/* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 3,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 4,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 5,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 6,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 7,
                        Arg1: "[]"
                      }
                    }
                  }
                }
              }
            }
          }
        }, evenIndex), /* constructor */{
      tag: "::",
      Arg0: 0,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 6,
            Arg1: "[]"
          }
        }
      }
    });

function id(x) {
  return x;
}

eq("MAP", Belt_List.map(Belt_List.makeBy(5, id), (function (x) {
            return (x << 1);
          })), /* constructor */{
      tag: "::",
      Arg0: 0,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 6,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 8,
              Arg1: "[]"
            }
          }
        }
      }
    });

eq("MAP", Belt_List.map("[]", id), "[]");

eq("MAP", Belt_List.map(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x) {
            return -x | 0;
          })), /* constructor */{
      tag: "::",
      Arg0: -1,
      Arg1: "[]"
    });

function add(a, b) {
  return a + b | 0;
}

var length_10_id = Belt_List.makeBy(10, id);

var length_8_id = Belt_List.makeBy(8, id);

var d = Belt_List.makeBy(10, (function (x) {
        return (x << 1);
      }));

eq("MAP2", Belt_List.zipBy(length_10_id, length_10_id, add), d);

eq("MAP2", Belt_List.zipBy("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, add), "[]");

eq("MAP2", Belt_List.zipBy(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, "[]", add), "[]");

eq("MAP2", Belt_List.zipBy("[]", "[]", add), "[]");

eq("MAP2", Belt_List.zipBy(length_10_id, length_10_id, add), Belt_List.concat(Belt_List.map(length_8_id, (function (x) {
                return (x << 1);
              })), /* constructor */{
          tag: "::",
          Arg0: 16,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 18,
            Arg1: "[]"
          }
        }));

eq("MAP2", Belt_List.zipBy(length_10_id, length_8_id, add), Belt_List.mapWithIndex(length_8_id, (function (i, x) {
            return i + x | 0;
          })));

eq("MAP2", Belt_List.reverse(Belt_List.mapReverse2(length_10_id, length_10_id, add)), Belt_List.map(length_10_id, (function (x) {
            return (x << 1);
          })));

var xs = Belt_List.reverse(Belt_List.mapReverse2(length_8_id, length_10_id, add));

eq("File \"bs_list_test.ml\", line 144, characters 5-12", Belt_List.length(xs), 8);

eq("MAP2", xs, Belt_List.zipBy(length_10_id, length_8_id, add));

eq("MAP2", Belt_List.mapReverse2(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x + y | 0;
          })), /* constructor */{
      tag: "::",
      Arg0: 4,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: "[]"
      }
    });

eq("TAKE", Belt_List.take(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, 2), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: "[]"
      }
    });

eq("TAKE", Belt_List.take("[]", 1), undefined);

eq("TAKE", Belt_List.take(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, 3), undefined);

eq("TAKE", Belt_List.take(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, 2), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: "[]"
      }
    });

eq("TAKE", Belt_List.take(length_10_id, 8), length_8_id);

eq("TAKE", Belt_List.take(length_10_id, 0), "[]");

eq("TAKE", Belt_List.take(length_8_id, -2), undefined);

eq("DROP", Belt_List.drop(length_10_id, 10), "[]");

eq("DROP", Belt_List.drop(length_10_id, 8), /* constructor */{
      tag: "::",
      Arg0: 8,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 9,
        Arg1: "[]"
      }
    });

eq("DROP", Belt_List.drop(length_10_id, 0), length_10_id);

eq("DROP", Belt_List.drop(length_8_id, -1), undefined);

var a = Belt_List.makeBy(5, id);

eq("SPLIT", Belt_List.splitAt("[]", 1), undefined);

eq("SPLIT", Belt_List.splitAt(a, 6), undefined);

eq("SPLIT", Belt_List.splitAt(a, 5), /* tuple */[
      a,
      "[]"
    ]);

eq("SPLIT", Belt_List.splitAt(a, 4), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 0,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }
      },
      /* constructor */{
        tag: "::",
        Arg0: 4,
        Arg1: "[]"
      }
    ]);

eq("SPLIT", Belt_List.splitAt(a, 3), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 0,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }
      },
      /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: "[]"
        }
      }
    ]);

eq("SPLIT", Belt_List.splitAt(a, 2), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 0,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }
      },
      /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }
      }
    ]);

eq("SPLIT", Belt_List.splitAt(a, 1), /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: 0,
        Arg1: "[]"
      },
      /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }
      }
    ]);

eq("SPLIT", Belt_List.splitAt(a, 0), /* tuple */[
      "[]",
      a
    ]);

eq("SPLIT", Belt_List.splitAt(a, -1), undefined);

function succx(x) {
  return x + 1 | 0;
}

function eqx(x, y) {
  return x === y;
}

b("File \"bs_list_test.ml\", line 182, characters 4-11", Belt_List.hasAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 2, Caml_obj.caml_equal));

b("File \"bs_list_test.ml\", line 183, characters 4-11", !Belt_List.hasAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 4, Caml_obj.caml_equal));

b("File \"bs_list_test.ml\", line 184, characters 4-11", Belt_List.hasAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 4, (function (x, y) {
            return (x + 1 | 0) === y;
          })));

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 3, Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          "2"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 1, Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        2,
        "2"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          3,
          "3"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 2, Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          3,
          "3"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 0, Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          "2"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            3,
            "3"
          ],
          Arg1: "[]"
        }
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 3, eqx), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          "2"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 1, eqx), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        2,
        "2"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          3,
          "3"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "1"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "2"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "3"
              ],
              Arg1: "[]"
            }
          }
        }, 2, eqx), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          3,
          "3"
        ],
        Arg1: "[]"
      }
    });

eq("REMOVEASSOQ", Belt_List.removeAssoc("[]", 2, eqx), "[]");

var ll = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    1,
    "1"
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      2,
      "2"
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        3,
        "3"
      ],
      Arg1: "[]"
    }
  }
};

var ll0 = Belt_List.removeAssoc(ll, 0, eqx);

b("File \"bs_list_test.ml\", line 196, characters 5-12", ll === ll0);

var ll1 = Belt_List.setAssoc(ll, 2, "22", Caml_obj.caml_equal);

eq("File \"bs_list_test.ml\", line 198, characters 5-12", ll1, /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          2,
          "22"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            3,
            "3"
          ],
          Arg1: "[]"
        }
      }
    });

var ll2 = Belt_List.setAssoc(ll1, 22, "2", Caml_obj.caml_equal);

b("File \"bs_list_test.ml\", line 200, characters 4-11", Caml_obj.caml_equal(ll2, /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            22,
            "2"
          ],
          Arg1: ll1
        }));

b("File \"bs_list_test.ml\", line 201, characters 4-11", Belt_List.tailExn(ll2) === ll1);

b("File \"bs_list_test.ml\", line 202, characters 4-11", Caml_obj.caml_equal(Belt_List.setAssoc(/* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                1,
                "a"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  2,
                  "b"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    3,
                    "c"
                  ],
                  Arg1: "[]"
                }
              }
            }, 2, "x", Caml_obj.caml_equal), /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "a"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "x"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "c"
              ],
              Arg1: "[]"
            }
          }
        }));

b("File \"bs_list_test.ml\", line 204, characters 4-11", Caml_obj.caml_equal(Belt_List.setAssoc(/* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                1,
                "a"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  3,
                  "c"
                ],
                Arg1: "[]"
              }
            }, 2, "2", Caml_obj.caml_equal), /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            2,
            "2"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              1,
              "a"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "c"
              ],
              Arg1: "[]"
            }
          }
        }));

eq("File \"bs_list_test.ml\", line 206, characters 5-12", Belt_List.setAssoc("[]", 1, "1", Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: "[]"
    });

debugger;

eq("File \"bs_list_test.ml\", line 208, characters 5-12", Belt_List.setAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "2"
          ],
          Arg1: "[]"
        }, 1, "1", Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        1,
        "1"
      ],
      Arg1: "[]"
    });

eq("File \"bs_list_test.ml\", line 210, characters 5-12", Belt_List.setAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            0,
            "0"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              1,
              "2"
            ],
            Arg1: "[]"
          }
        }, 1, "1", Caml_obj.caml_equal), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        0,
        "0"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          1,
          "1"
        ],
        Arg1: "[]"
      }
    });

b("File \"bs_list_test.ml\", line 211, characters 4-11", Caml_obj.caml_equal(Belt_List.getAssoc(/* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                1,
                "a"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  2,
                  "b"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    3,
                    "c"
                  ],
                  Arg1: "[]"
                }
              }
            }, 2, Caml_obj.caml_equal), "b"));

b("File \"bs_list_test.ml\", line 212, characters 4-11", Belt_List.getAssoc(/* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            1,
            "a"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              2,
              "b"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                3,
                "c"
              ],
              Arg1: "[]"
            }
          }
        }, 4, Caml_obj.caml_equal) === undefined);

eq("File \"bs_list_test.ml\", line 216, characters 5-12", /* tuple */[
      Belt_List.head(length_10_id),
      Belt_List.tail(length_10_id)
    ], /* tuple */[
      0,
      Belt_List.drop(length_10_id, 1)
    ]);

eq("File \"bs_list_test.ml\", line 219, characters 5-12", Belt_List.head("[]"), undefined);

$$throw("File \"bs_list_test.ml\", line 220, characters 8-15", (function (param) {
        return Belt_List.headExn("[]");
      }));

$$throw("File \"bs_list_test.ml\", line 221, characters 8-15", (function (param) {
        Belt_List.tailExn("[]");
        return /* () */0;
      }));

$$throw("File \"bs_list_test.ml\", line 222, characters 8-15", (function (param) {
        Belt_List.getExn(/* constructor */{
              tag: "::",
              Arg0: 0,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 1,
                Arg1: "[]"
              }
            }, -1);
        return /* () */0;
      }));

$$throw("File \"bs_list_test.ml\", line 223, characters 8-15", (function (param) {
        Belt_List.getExn(/* constructor */{
              tag: "::",
              Arg0: 0,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 1,
                Arg1: "[]"
              }
            }, 2);
        return /* () */0;
      }));

eq("File \"bs_list_test.ml\", line 224, characters 5-12", Belt_List.map(/* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: "[]"
          }
        }, (function (i) {
            return Belt_List.getExn(/* constructor */{
                        tag: "::",
                        Arg0: 0,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 1,
                          Arg1: "[]"
                        }
                      }, i);
          })), /* constructor */{
      tag: "::",
      Arg0: 0,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: "[]"
      }
    });

eq("File \"bs_list_test.ml\", line 225, characters 5-12", Belt_List.headExn(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }), 1);

eq("File \"bs_list_test.ml\", line 226, characters 5-12", Belt_List.tailExn(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }), "[]");

Belt_List.forEachWithIndex(length_10_id, (function (i, x) {
        return eq("File \"bs_list_test.ml\", line 228, characters 9-16", Belt_List.get(length_10_id, i), x);
      }));

eq("File \"bs_list_test.ml\", line 229, characters 5-12", Belt_List.tail("[]"), undefined);

eq("File \"bs_list_test.ml\", line 230, characters 5-12", Belt_List.drop("[]", 3), undefined);

eq("File \"bs_list_test.ml\", line 231, characters 5-12", Belt_List.mapWithIndex("[]", (function (i, x) {
            return i + x | 0;
          })), "[]");

eq("File \"bs_list_test.ml\", line 232, characters 5-12", Belt_List.get(length_10_id, -1), undefined);

eq("File \"bs_list_test.ml\", line 233, characters 5-12", Belt_List.get(length_10_id, 12), undefined);

eq("File \"bs_list_test.ml\", line 234, characters 5-12", sum("[]"), 0);

eq("File \"bs_list_test.ml\", line 235, characters 5-12", sum(length_10_id), 45);

eq("File \"bs_list_test.ml\", line 236, characters 5-12", Belt_List.makeBy(0, id), "[]");

eq("File \"bs_list_test.ml\", line 237, characters 5-12", Belt_List.reverse(Belt_List.reverse(length_10_id)), length_10_id);

eq("File \"bs_list_test.ml\", line 238, characters 5-12", Belt_List.reverse(Belt_List.reverse(length_8_id)), length_8_id);

eq("File \"bs_list_test.ml\", line 239, characters 5-12", Belt_List.reverse("[]"), "[]");

eq("File \"bs_list_test.ml\", line 240, characters 5-12", Belt_List.reverse(Belt_List.mapReverse(length_10_id, succx)), Belt_List.map(length_10_id, succx));

eq("File \"bs_list_test.ml\", line 243, characters 5-12", Belt_List.reduce(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 245, characters 5-12", Belt_List.reduceReverse(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 247, characters 5-12", Belt_List.reduceReverse(Belt_List.makeBy(10000, (function (i) {
                return i;
              })), 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })), 49995000);

eq("File \"bs_list_test.ml\", line 252, characters 5-12", sum2(length_10_id, length_10_id), 90);

eq("File \"bs_list_test.ml\", line 253, characters 5-12", sum2(length_8_id, length_10_id), 56);

eq("File \"bs_list_test.ml\", line 254, characters 5-12", sum2(length_10_id, length_8_id), 56);

eq("File \"bs_list_test.ml\", line 255, characters 5-12", Belt_List.reduce2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 257, characters 5-12", Belt_List.reduce2(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 6,
              Arg1: "[]"
            }
          }
        }, 0, (function (a, b, c) {
            return (a + b | 0) + c | 0;
          })), 18);

eq("File \"bs_list_test.ml\", line 258, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 260, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_10_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 90);

eq("File \"bs_list_test.ml\", line 262, characters 5-12", Belt_List.reduceReverse2(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 6);

eq("File \"bs_list_test.ml\", line 263, characters 5-12", Belt_List.every(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 6,
              Arg1: "[]"
            }
          }
        }, mod2), true);

eq("File \"bs_list_test.ml\", line 264, characters 5-12", Belt_List.every(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, mod2), false);

eq("File \"bs_list_test.ml\", line 265, characters 5-12", Belt_List.every("[]", mod2), true);

eq("File \"bs_list_test.ml\", line 266, characters 5-12", Belt_List.some(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 5,
              Arg1: "[]"
            }
          }
        }, mod2), true);

eq("File \"bs_list_test.ml\", line 267, characters 5-12", Belt_List.some(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 5,
              Arg1: "[]"
            }
          }
        }, mod2), false);

eq("File \"bs_list_test.ml\", line 268, characters 5-12", Belt_List.some("[]", mod2), false);

eq("File \"bs_list_test.ml\", line 269, characters 5-12", Belt_List.has(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, "2", (function (x, s) {
            return String(x) === s;
          })), true);

eq("File \"bs_list_test.ml\", line 270, characters 5-12", Belt_List.has(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, "0", (function (x, s) {
            return String(x) === s;
          })), false);

b("File \"bs_list_test.ml\", line 272, characters 4-11", Belt_List.reduceReverse(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })) === 10);

b("File \"bs_list_test.ml\", line 273, characters 4-11", Belt_List.reduceReverse(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, 10, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })) === 0);

b("File \"bs_list_test.ml\", line 274, characters 4-11", Caml_obj.caml_equal(Belt_List.reduceReverse(/* constructor */{
              tag: "::",
              Arg0: 1,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4,
                    Arg1: "[]"
                  }
                }
              }
            }, "[]", Belt_List.add), /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }));

b("File \"bs_list_test.ml\", line 275, characters 4-11", Belt_List.reduce(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })) === 10);

b("File \"bs_list_test.ml\", line 276, characters 4-11", Belt_List.reduce(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, 10, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })) === 0);

b("File \"bs_list_test.ml\", line 277, characters 4-11", Caml_obj.caml_equal(Belt_List.reduce(/* constructor */{
              tag: "::",
              Arg0: 1,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4,
                    Arg1: "[]"
                  }
                }
              }
            }, "[]", Belt_List.add), /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 1,
                Arg1: "[]"
              }
            }
          }
        }));

b("File \"bs_list_test.ml\", line 278, characters 4-11", Belt_List.reduceWithIndex(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, 0, (function (acc, x, i) {
            return (acc + x | 0) + i | 0;
          })) === 16);

b("File \"bs_list_test.ml\", line 279, characters 4-11", Belt_List.reduceReverse2(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })) === 6);

var a$1 = Belt_List.makeBy(10000, (function (i) {
        return i;
      }));

b("File \"bs_list_test.ml\", line 282, characters 4-11", Belt_List.reduceReverse2(a$1, /* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: a$1
        }, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })) === 99980001);

eq("File \"bs_list_test.ml\", line 288, characters 5-12", Belt_List.every2("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 289, characters 5-12", Belt_List.every2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 290, characters 5-12", Belt_List.every2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: "[]"
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 291, characters 5-12", Belt_List.every2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 292, characters 5-12", Belt_List.every2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 0,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 293, characters 5-12", Belt_List.some2("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x, y) {
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 294, characters 5-12", Belt_List.some2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 295, characters 5-12", Belt_List.some2(/* constructor */{
          tag: "::",
          Arg0: 2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 296, characters 5-12", Belt_List.some2(/* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 297, characters 5-12", Belt_List.some2(/* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 298, characters 5-12", Belt_List.some2(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: -1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: -2,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x === y;
          })), false);

function makeTest(n) {
  return eq("File \"bs_list_test.ml\", line 301, characters 5-12", Belt_List.make(n, 3), Belt_List.makeBy(n, (function (param) {
                    return 3;
                  })));
}

eq("File \"bs_list_test.ml\", line 304, characters 5-12", /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: "[]"
      }
    }, /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: "[]"
      }
    });

b("File \"bs_list_test.ml\", line 310, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 3,
                Arg1: "[]"
              }
            }
          }
        }, Caml_obj.caml_compare) > 0);

b("File \"bs_list_test.ml\", line 311, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, Caml_obj.caml_compare) > 0);

b("File \"bs_list_test.ml\", line 312, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, Caml_obj.caml_compare) < 0);

b("File \"bs_list_test.ml\", line 313, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 0,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: "[]"
            }
          }
        }, Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 314, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, Caml_primitive.caml_int_compare) === 0);

b("File \"bs_list_test.ml\", line 315, characters 4-11", Belt_List.cmp(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 316, characters 4-11", Belt_List.cmpByLength("[]", "[]") === 0);

b("File \"bs_list_test.ml\", line 317, characters 4-11", Belt_List.cmpByLength(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, "[]") > 0);

b("File \"bs_list_test.ml\", line 318, characters 4-11", Belt_List.cmpByLength("[]", /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }) < 0);

b("File \"bs_list_test.ml\", line 319, characters 4-11", Belt_List.cmpByLength(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }) > 0);

b("File \"bs_list_test.ml\", line 320, characters 4-11", Belt_List.cmpByLength(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: "[]"
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }) < 0);

b("File \"bs_list_test.ml\", line 321, characters 4-11", Belt_List.cmpByLength(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 3,
            Arg1: "[]"
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }) === 0);

makeTest(0);

makeTest(1);

makeTest(2);

makeTest(3);

function cmp(a, b) {
  return a - b | 0;
}

eq("SORT", Belt_List.sort(/* constructor */{
          tag: "::",
          Arg0: 5,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: "[]"
              }
            }
          }
        }, cmp), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 4,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 5,
            Arg1: "[]"
          }
        }
      }
    });

eq("SORT", Belt_List.sort(/* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 9,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 37,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 3,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 1,
                  Arg1: "[]"
                }
              }
            }
          }
        }, cmp), /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 3,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 3,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 9,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 37,
              Arg1: "[]"
            }
          }
        }
      }
    });

b("File \"bs_list_test.ml\", line 337, characters 4-11", !Belt_List.eq(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: "[]"
          }
        }, (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 338, characters 4-11", Belt_List.eq(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 339, characters 4-11", !Belt_List.eq(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4,
              Arg1: "[]"
            }
          }
        }, (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 340, characters 4-11", !Belt_List.eq(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }, /* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, Caml_obj.caml_equal));

var u0 = Belt_List.makeBy(20, (function (x) {
        return x;
      }));

var u1 = Belt_List.keepMap(u0, (function (x) {
        if (x % 7 === 0) {
          return x + 1 | 0;
        }
        
      }));

eq("File \"bs_list_test.ml\", line 344, characters 5-12", u1, /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 8,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 15,
          Arg1: "[]"
        }
      }
    });

b("File \"bs_list_test.ml\", line 345, characters 4-11", Caml_obj.caml_equal(Belt_List.keepMap(/* constructor */{
              tag: "::",
              Arg0: 1,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 2,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 3,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4,
                    Arg1: "[]"
                  }
                }
              }
            }, (function (x) {
                if (x % 2 === 0) {
                  return -x | 0;
                }
                
              })), /* constructor */{
          tag: "::",
          Arg0: -2,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: -4,
            Arg1: "[]"
          }
        }));

b("File \"bs_list_test.ml\", line 349, characters 4-11", Belt_List.keepMap(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4,
                Arg1: "[]"
              }
            }
          }
        }, (function (x) {
            if (x % 5 === 0) {
              return x;
            }
            
          })) === "[]");

Mt.from_pair_suites("Bs_list_test", suites[0]);

var N = 0;

var A = 0;

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.$$throw = $$throw;
exports.N = N;
exports.A = A;
exports.J = J;
exports.sum = sum;
exports.sum2 = sum2;
exports.mod2 = mod2;
exports.evenIndex = evenIndex;
exports.id = id;
exports.add = add;
exports.length_10_id = length_10_id;
exports.length_8_id = length_8_id;
exports.succx = succx;
exports.makeTest = makeTest;
/* u Not a pure module */
