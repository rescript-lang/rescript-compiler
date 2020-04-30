

import * as Curry from "./curry.js";
import * as Caml_obj from "./caml_obj.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_option from "./caml_option.js";

function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (!param) {
      return len;
    }
    _param = param[1];
    _len = len + 1 | 0;
    continue ;
  };
}

function cons(a, l) {
  return /* :: */[
          a,
          l
        ];
}

function hd(param) {
  if (param) {
    return param[0];
  }
  throw {
        ExceptionID: -2,
        _1: "hd",
        Debug: "Failure"
      };
}

function tl(param) {
  if (param) {
    return param[1];
  }
  throw {
        ExceptionID: -2,
        _1: "tl",
        Debug: "Failure"
      };
}

function nth(l, n) {
  if (n < 0) {
    throw {
          ExceptionID: -3,
          _1: "List.nth",
          Debug: "Invalid_argument"
        };
  }
  var _l = l;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var l$1 = _l;
    if (l$1) {
      if (n$1 === 0) {
        return l$1[0];
      }
      _n = n$1 - 1 | 0;
      _l = l$1[1];
      continue ;
    }
    throw {
          ExceptionID: -2,
          _1: "nth",
          Debug: "Failure"
        };
  };
}

function nth_opt(l, n) {
  if (n < 0) {
    throw {
          ExceptionID: -3,
          _1: "List.nth",
          Debug: "Invalid_argument"
        };
  }
  var _l = l;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var l$1 = _l;
    if (!l$1) {
      return ;
    }
    if (n$1 === 0) {
      return Caml_option.some(l$1[0]);
    }
    _n = n$1 - 1 | 0;
    _l = l$1[1];
    continue ;
  };
}

function rev_append(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      return l2;
    }
    _l2 = /* :: */[
      l1[0],
      l2
    ];
    _l1 = l1[1];
    continue ;
  };
}

function rev(l) {
  return rev_append(l, /* [] */0);
}

function init_tailrec_aux(_acc, _i, n, f) {
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i >= n) {
      return acc;
    }
    _i = i + 1 | 0;
    _acc = /* :: */[
      Curry._1(f, i),
      acc
    ];
    continue ;
  };
}

function init_aux(i, n, f) {
  if (i >= n) {
    return /* [] */0;
  }
  var r = Curry._1(f, i);
  return /* :: */[
          r,
          init_aux(i + 1 | 0, n, f)
        ];
}

function init(len, f) {
  if (len < 0) {
    throw {
          ExceptionID: -3,
          _1: "List.init",
          Debug: "Invalid_argument"
        };
  }
  if (len > 10000) {
    return rev_append(init_tailrec_aux(/* [] */0, 0, len, f), /* [] */0);
  } else {
    return init_aux(0, len, f);
  }
}

function flatten(param) {
  if (param) {
    return Pervasives.$at(param[0], flatten(param[1]));
  } else {
    return /* [] */0;
  }
}

function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = Curry._1(f, param[0]);
  return /* :: */[
          r,
          map(f, param[1])
        ];
}

function mapi(i, f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = Curry._2(f, i, param[0]);
  return /* :: */[
          r,
          mapi(i + 1 | 0, f, param[1])
        ];
}

function mapi$1(f, l) {
  return mapi(0, f, l);
}

function rev_map(f, l) {
  var _accu = /* [] */0;
  var _param = l;
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param[1];
    _accu = /* :: */[
      Curry._1(f, param[0]),
      accu
    ];
    continue ;
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    Curry._1(f, param[0]);
    _param = param[1];
    continue ;
  };
}

function iteri(f, l) {
  var _i = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var i = _i;
    if (!param) {
      return ;
    }
    Curry._2(f, i, param[0]);
    _param = param[1];
    _i = i + 1 | 0;
    continue ;
  };
}

function fold_left(f, _accu, _l) {
  while(true) {
    var l = _l;
    var accu = _accu;
    if (!l) {
      return accu;
    }
    _l = l[1];
    _accu = Curry._2(f, accu, l[0]);
    continue ;
  };
}

function fold_right(f, l, accu) {
  if (l) {
    return Curry._2(f, l[0], fold_right(f, l[1], accu));
  } else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1) {
    if (l2) {
      var r = Curry._2(f, l1[0], l2[0]);
      return /* :: */[
              r,
              map2(f, l1[1], l2[1])
            ];
    }
    throw {
          ExceptionID: -3,
          _1: "List.map2",
          Debug: "Invalid_argument"
        };
  }
  if (!l2) {
    return /* [] */0;
  }
  throw {
        ExceptionID: -3,
        _1: "List.map2",
        Debug: "Invalid_argument"
      };
}

function rev_map2(f, l1, l2) {
  var _accu = /* [] */0;
  var _l1 = l1;
  var _l2 = l2;
  while(true) {
    var l2$1 = _l2;
    var l1$1 = _l1;
    var accu = _accu;
    if (l1$1) {
      if (l2$1) {
        _l2 = l2$1[1];
        _l1 = l1$1[1];
        _accu = /* :: */[
          Curry._2(f, l1$1[0], l2$1[0]),
          accu
        ];
        continue ;
      }
      throw {
            ExceptionID: -3,
            _1: "List.rev_map2",
            Debug: "Invalid_argument"
          };
    }
    if (l2$1) {
      throw {
            ExceptionID: -3,
            _1: "List.rev_map2",
            Debug: "Invalid_argument"
          };
    }
    return accu;
  };
}

function iter2(f, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        Curry._2(f, l1[0], l2[0]);
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      }
      throw {
            ExceptionID: -3,
            _1: "List.iter2",
            Debug: "Invalid_argument"
          };
    }
    if (!l2) {
      return ;
    }
    throw {
          ExceptionID: -3,
          _1: "List.iter2",
          Debug: "Invalid_argument"
        };
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    var accu = _accu;
    if (l1) {
      if (l2) {
        _l2 = l2[1];
        _l1 = l1[1];
        _accu = Curry._3(f, accu, l1[0], l2[0]);
        continue ;
      }
      throw {
            ExceptionID: -3,
            _1: "List.fold_left2",
            Debug: "Invalid_argument"
          };
    }
    if (l2) {
      throw {
            ExceptionID: -3,
            _1: "List.fold_left2",
            Debug: "Invalid_argument"
          };
    }
    return accu;
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return Curry._3(f, l1[0], l2[0], fold_right2(f, l1[1], l2[1], accu));
    }
    throw {
          ExceptionID: -3,
          _1: "List.fold_right2",
          Debug: "Invalid_argument"
        };
  }
  if (l2) {
    throw {
          ExceptionID: -3,
          _1: "List.fold_right2",
          Debug: "Invalid_argument"
        };
  }
  return accu;
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._1(p, param[0])) {
      return false;
    }
    _param = param[1];
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._1(p, param[0])) {
      return true;
    }
    _param = param[1];
    continue ;
  };
}

function for_all2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (!Curry._2(p, l1[0], l2[0])) {
          return false;
        }
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      }
      throw {
            ExceptionID: -3,
            _1: "List.for_all2",
            Debug: "Invalid_argument"
          };
    }
    if (!l2) {
      return true;
    }
    throw {
          ExceptionID: -3,
          _1: "List.for_all2",
          Debug: "Invalid_argument"
        };
  };
}

function exists2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (Curry._2(p, l1[0], l2[0])) {
          return true;
        }
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      }
      throw {
            ExceptionID: -3,
            _1: "List.exists2",
            Debug: "Invalid_argument"
          };
    }
    if (!l2) {
      return false;
    }
    throw {
          ExceptionID: -3,
          _1: "List.exists2",
          Debug: "Invalid_argument"
        };
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Caml_obj.caml_equal(param[0], x)) {
      return true;
    }
    _param = param[1];
    continue ;
  };
}

function memq(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (param[0] === x) {
      return true;
    }
    _param = param[1];
    continue ;
  };
}

function assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param[0];
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      }
      _param = param[1];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function assoc_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var match = param[0];
    if (Caml_obj.caml_equal(match[0], x)) {
      return Caml_option.some(match[1]);
    }
    _param = param[1];
    continue ;
  };
}

function assq(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param[0];
      if (match[0] === x) {
        return match[1];
      }
      _param = param[1];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function assq_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var match = param[0];
    if (match[0] === x) {
      return Caml_option.some(match[1]);
    }
    _param = param[1];
    continue ;
  };
}

function mem_assoc(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Caml_obj.caml_equal(param[0][0], x)) {
      return true;
    }
    _param = param[1];
    continue ;
  };
}

function mem_assq(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (param[0][0] === x) {
      return true;
    }
    _param = param[1];
    continue ;
  };
}

function remove_assoc(x, param) {
  if (!param) {
    return /* [] */0;
  }
  var l = param[1];
  var pair = param[0];
  if (Caml_obj.caml_equal(pair[0], x)) {
    return l;
  } else {
    return /* :: */[
            pair,
            remove_assoc(x, l)
          ];
  }
}

function remove_assq(x, param) {
  if (!param) {
    return /* [] */0;
  }
  var l = param[1];
  var pair = param[0];
  if (pair[0] === x) {
    return l;
  } else {
    return /* :: */[
            pair,
            remove_assq(x, l)
          ];
  }
}

function find(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param[0];
      if (Curry._1(p, x)) {
        return x;
      }
      _param = param[1];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_opt(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var x = param[0];
    if (Curry._1(p, x)) {
      return Caml_option.some(x);
    }
    _param = param[1];
    continue ;
  };
}

function find_all(p) {
  return (function (param) {
      var _accu = /* [] */0;
      var _param = param;
      while(true) {
        var param$1 = _param;
        var accu = _accu;
        if (!param$1) {
          return rev_append(accu, /* [] */0);
        }
        var l = param$1[1];
        var x = param$1[0];
        if (Curry._1(p, x)) {
          _param = l;
          _accu = /* :: */[
            x,
            accu
          ];
          continue ;
        }
        _param = l;
        continue ;
      };
    });
}

function partition(p, l) {
  var _yes = /* [] */0;
  var _no = /* [] */0;
  var _param = l;
  while(true) {
    var param = _param;
    var no = _no;
    var yes = _yes;
    if (!param) {
      return /* tuple */[
              rev_append(yes, /* [] */0),
              rev_append(no, /* [] */0)
            ];
    }
    var l$1 = param[1];
    var x = param[0];
    if (Curry._1(p, x)) {
      _param = l$1;
      _yes = /* :: */[
        x,
        yes
      ];
      continue ;
    }
    _param = l$1;
    _no = /* :: */[
      x,
      no
    ];
    continue ;
  };
}

function split(param) {
  if (!param) {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
  var match = param[0];
  var match$1 = split(param[1]);
  return /* tuple */[
          /* :: */[
            match[0],
            match$1[0]
          ],
          /* :: */[
            match[1],
            match$1[1]
          ]
        ];
}

function combine(l1, l2) {
  if (l1) {
    if (l2) {
      return /* :: */[
              /* tuple */[
                l1[0],
                l2[0]
              ],
              combine(l1[1], l2[1])
            ];
    }
    throw {
          ExceptionID: -3,
          _1: "List.combine",
          Debug: "Invalid_argument"
        };
  }
  if (!l2) {
    return /* [] */0;
  }
  throw {
        ExceptionID: -3,
        _1: "List.combine",
        Debug: "Invalid_argument"
      };
}

function merge(cmp, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  var h2 = l2[0];
  var h1 = l1[0];
  if (Curry._2(cmp, h1, h2) <= 0) {
    return /* :: */[
            h1,
            merge(cmp, l1[1], l2)
          ];
  } else {
    return /* :: */[
            h2,
            merge(cmp, l1, l2[1])
          ];
  }
}

function chop(_k, _l) {
  while(true) {
    var l = _l;
    var k = _k;
    if (k === 0) {
      return l;
    }
    if (l) {
      _l = l[1];
      _k = k - 1 | 0;
      continue ;
    }
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "list.ml",
            262,
            11
          ],
          Debug: "Assert_failure"
        };
  };
}

function stable_sort(cmp, l) {
  var sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            if (Curry._2(cmp, x1, x2) <= 0) {
              if (Curry._2(cmp, x2, x3) <= 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              } else if (Curry._2(cmp, x1, x3) <= 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            } else if (Curry._2(cmp, x1, x3) <= 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            } else if (Curry._2(cmp, x2, x3) <= 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            } else {
              return /* :: */[
                      x3,
                      /* :: */[
                        x2,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        if (Curry._2(cmp, x1$1, x2$1) <= 0) {
          return /* :: */[
                  x1$1,
                  /* :: */[
                    x2$1,
                    /* [] */0
                  ]
                ];
        } else {
          return /* :: */[
                  x2$1,
                  /* :: */[
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      
    }
    var n1 = (n >> 1);
    var n2 = n - n1 | 0;
    var l2 = chop(n1, l);
    var s1 = rev_sort(n1, l);
    var s2 = rev_sort(n2, l2);
    var _l1 = s1;
    var _l2 = s2;
    var _accu = /* [] */0;
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      var h2 = l2$1[0];
      var h1 = l1[0];
      if (Curry._2(cmp, h1, h2) > 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l1 = l1[1];
        continue ;
      }
      _accu = /* :: */[
        h2,
        accu
      ];
      _l2 = l2$1[1];
      continue ;
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            if (Curry._2(cmp, x1, x2) > 0) {
              if (Curry._2(cmp, x2, x3) > 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              } else if (Curry._2(cmp, x1, x3) > 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            } else if (Curry._2(cmp, x1, x3) > 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            } else if (Curry._2(cmp, x2, x3) > 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            } else {
              return /* :: */[
                      x3,
                      /* :: */[
                        x2,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        if (Curry._2(cmp, x1$1, x2$1) > 0) {
          return /* :: */[
                  x1$1,
                  /* :: */[
                    x2$1,
                    /* [] */0
                  ]
                ];
        } else {
          return /* :: */[
                  x2$1,
                  /* :: */[
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      
    }
    var n1 = (n >> 1);
    var n2 = n - n1 | 0;
    var l2 = chop(n1, l);
    var s1 = sort(n1, l);
    var s2 = sort(n2, l2);
    var _l1 = s1;
    var _l2 = s2;
    var _accu = /* [] */0;
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      var h2 = l2$1[0];
      var h1 = l1[0];
      if (Curry._2(cmp, h1, h2) <= 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l1 = l1[1];
        continue ;
      }
      _accu = /* :: */[
        h2,
        accu
      ];
      _l2 = l2$1[1];
      continue ;
    };
  };
  var len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

function sort_uniq(cmp, l) {
  var sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return /* :: */[
                        x2,
                        /* [] */0
                      ];
              } else if (c$1 < 0) {
                return /* :: */[
                        x2,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              }
            }
            if (c < 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              }
              if (c$2 < 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              var c$3 = Curry._2(cmp, x1, x3);
              if (c$3 === 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              } else if (c$3 < 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            }
            var c$4 = Curry._2(cmp, x1, x3);
            if (c$4 === 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* [] */0
                      ]
                    ];
            }
            if (c$4 < 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            var c$5 = Curry._2(cmp, x2, x3);
            if (c$5 === 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* [] */0
                      ]
                    ];
            } else if (c$5 < 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            } else {
              return /* :: */[
                      x3,
                      /* :: */[
                        x2,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return /* :: */[
                  x1$1,
                  /* [] */0
                ];
        } else if (c$6 < 0) {
          return /* :: */[
                  x1$1,
                  /* :: */[
                    x2$1,
                    /* [] */0
                  ]
                ];
        } else {
          return /* :: */[
                  x2$1,
                  /* :: */[
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      
    }
    var n1 = (n >> 1);
    var n2 = n - n1 | 0;
    var l2 = chop(n1, l);
    var s1 = rev_sort(n1, l);
    var s2 = rev_sort(n2, l2);
    var _l1 = s1;
    var _l2 = s2;
    var _accu = /* [] */0;
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      var t2 = l2$1[1];
      var h2 = l2$1[0];
      var t1 = l1[1];
      var h1 = l1[0];
      var c$7 = Curry._2(cmp, h1, h2);
      if (c$7 === 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l2 = t2;
        _l1 = t1;
        continue ;
      }
      if (c$7 > 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l1 = t1;
        continue ;
      }
      _accu = /* :: */[
        h2,
        accu
      ];
      _l2 = t2;
      continue ;
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return /* :: */[
                        x2,
                        /* [] */0
                      ];
              } else if (c$1 > 0) {
                return /* :: */[
                        x2,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              }
            }
            if (c > 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              }
              if (c$2 > 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              var c$3 = Curry._2(cmp, x1, x3);
              if (c$3 === 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x2,
                          /* [] */0
                        ]
                      ];
              } else if (c$3 > 0) {
                return /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              } else {
                return /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            }
            var c$4 = Curry._2(cmp, x1, x3);
            if (c$4 === 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* [] */0
                      ]
                    ];
            }
            if (c$4 > 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* :: */[
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            var c$5 = Curry._2(cmp, x2, x3);
            if (c$5 === 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x1,
                        /* [] */0
                      ]
                    ];
            } else if (c$5 > 0) {
              return /* :: */[
                      x2,
                      /* :: */[
                        x3,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            } else {
              return /* :: */[
                      x3,
                      /* :: */[
                        x2,
                        /* :: */[
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return /* :: */[
                  x1$1,
                  /* [] */0
                ];
        } else if (c$6 > 0) {
          return /* :: */[
                  x1$1,
                  /* :: */[
                    x2$1,
                    /* [] */0
                  ]
                ];
        } else {
          return /* :: */[
                  x2$1,
                  /* :: */[
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      
    }
    var n1 = (n >> 1);
    var n2 = n - n1 | 0;
    var l2 = chop(n1, l);
    var s1 = sort(n1, l);
    var s2 = sort(n2, l2);
    var _l1 = s1;
    var _l2 = s2;
    var _accu = /* [] */0;
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      var t2 = l2$1[1];
      var h2 = l2$1[0];
      var t1 = l1[1];
      var h1 = l1[0];
      var c$7 = Curry._2(cmp, h1, h2);
      if (c$7 === 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l2 = t2;
        _l1 = t1;
        continue ;
      }
      if (c$7 < 0) {
        _accu = /* :: */[
          h1,
          accu
        ];
        _l1 = t1;
        continue ;
      }
      _accu = /* :: */[
        h2,
        accu
      ];
      _l2 = t2;
      continue ;
    };
  };
  var len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

function compare_lengths(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!l2) {
      return 1;
    }
    _l2 = l2[1];
    _l1 = l1[1];
    continue ;
  };
}

function compare_length_with(_l, _n) {
  while(true) {
    var n = _n;
    var l = _l;
    if (!l) {
      if (n === 0) {
        return 0;
      } else if (n > 0) {
        return -1;
      } else {
        return 1;
      }
    }
    if (n <= 0) {
      return 1;
    }
    _n = n - 1 | 0;
    _l = l[1];
    continue ;
  };
}

var append = Pervasives.$at;

var concat = flatten;

var filter = find_all;

var sort = stable_sort;

var fast_sort = stable_sort;

export {
  length ,
  compare_lengths ,
  compare_length_with ,
  cons ,
  hd ,
  tl ,
  nth ,
  nth_opt ,
  rev ,
  init ,
  append ,
  rev_append ,
  concat ,
  flatten ,
  iter ,
  iteri ,
  map ,
  mapi$1 as mapi,
  rev_map ,
  fold_left ,
  fold_right ,
  iter2 ,
  map2 ,
  rev_map2 ,
  fold_left2 ,
  fold_right2 ,
  for_all ,
  exists ,
  for_all2 ,
  exists2 ,
  mem ,
  memq ,
  find ,
  find_opt ,
  filter ,
  find_all ,
  partition ,
  assoc ,
  assoc_opt ,
  assq ,
  assq_opt ,
  mem_assoc ,
  mem_assq ,
  remove_assoc ,
  remove_assq ,
  split ,
  combine ,
  sort ,
  stable_sort ,
  fast_sort ,
  sort_uniq ,
  merge ,
  
}
/* No side effect */
