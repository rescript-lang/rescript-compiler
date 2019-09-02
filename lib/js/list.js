'use strict';

var Curry = require("./curry.js");
var Caml_obj = require("./caml_obj.js");
var Pervasives = require("./pervasives.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (param !== "[]") {
      _param = param.Arg1;
      _len = len + 1 | 0;
      continue ;
    } else {
      return len;
    }
  };
}

function hd(param) {
  if (param !== "[]") {
    return param.Arg0;
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "hd"
        ];
  }
}

function tl(param) {
  if (param !== "[]") {
    return param.Arg1;
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "tl"
        ];
  }
}

function nth(l, n) {
  if (n < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.nth"
        ];
  }
  var _l = l;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var l$1 = _l;
    if (l$1 !== "[]") {
      if (n$1 === 0) {
        return l$1.Arg0;
      } else {
        _n = n$1 - 1 | 0;
        _l = l$1.Arg1;
        continue ;
      }
    } else {
      throw [
            Caml_builtin_exceptions.failure,
            "nth"
          ];
    }
  };
}

function rev_append(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      _l2 = /* constructor */{
        tag: "::",
        Arg0: l1.Arg0,
        Arg1: l2
      };
      _l1 = l1.Arg1;
      continue ;
    } else {
      return l2;
    }
  };
}

function rev(l) {
  return rev_append(l, "[]");
}

function flatten(param) {
  if (param !== "[]") {
    return Pervasives.$at(param.Arg0, flatten(param.Arg1));
  } else {
    return "[]";
  }
}

function map(f, param) {
  if (param !== "[]") {
    var r = Curry._1(f, param.Arg0);
    return /* constructor */{
            tag: "::",
            Arg0: r,
            Arg1: map(f, param.Arg1)
          };
  } else {
    return "[]";
  }
}

function mapi(i, f, param) {
  if (param !== "[]") {
    var r = Curry._2(f, i, param.Arg0);
    return /* constructor */{
            tag: "::",
            Arg0: r,
            Arg1: mapi(i + 1 | 0, f, param.Arg1)
          };
  } else {
    return "[]";
  }
}

function mapi$1(f, l) {
  return mapi(0, f, l);
}

function rev_map(f, l) {
  var _accu = "[]";
  var _param = l;
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "[]") {
      _param = param.Arg1;
      _accu = /* constructor */{
        tag: "::",
        Arg0: Curry._1(f, param.Arg0),
        Arg1: accu
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      Curry._1(f, param.Arg0);
      _param = param.Arg1;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function iteri(f, l) {
  var _i = 0;
  var f$1 = f;
  var _param = l;
  while(true) {
    var param = _param;
    var i = _i;
    if (param !== "[]") {
      Curry._2(f$1, i, param.Arg0);
      _param = param.Arg1;
      _i = i + 1 | 0;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fold_left(f, _accu, _l) {
  while(true) {
    var l = _l;
    var accu = _accu;
    if (l !== "[]") {
      _l = l.Arg1;
      _accu = Curry._2(f, accu, l.Arg0);
      continue ;
    } else {
      return accu;
    }
  };
}

function fold_right(f, l, accu) {
  if (l !== "[]") {
    return Curry._2(f, l.Arg0, fold_right(f, l.Arg1, accu));
  } else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1 !== "[]") {
    if (l2 !== "[]") {
      var r = Curry._2(f, l1.Arg0, l2.Arg0);
      return /* constructor */{
              tag: "::",
              Arg0: r,
              Arg1: map2(f, l1.Arg1, l2.Arg1)
            };
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.map2"
          ];
    }
  } else if (l2 !== "[]") {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.map2"
        ];
  } else {
    return "[]";
  }
}

function rev_map2(f, l1, l2) {
  var _accu = "[]";
  var _l1 = l1;
  var _l2 = l2;
  while(true) {
    var l2$1 = _l2;
    var l1$1 = _l1;
    var accu = _accu;
    if (l1$1 !== "[]") {
      if (l2$1 !== "[]") {
        _l2 = l2$1.Arg1;
        _l1 = l1$1.Arg1;
        _accu = /* constructor */{
          tag: "::",
          Arg0: Curry._2(f, l1$1.Arg0, l2$1.Arg0),
          Arg1: accu
        };
        continue ;
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.rev_map2"
            ];
      }
    } else {
      if (l2$1 !== "[]") {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.rev_map2"
            ];
      }
      return accu;
    }
  };
}

function iter2(f, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      if (l2 !== "[]") {
        Curry._2(f, l1.Arg0, l2.Arg0);
        _l2 = l2.Arg1;
        _l1 = l1.Arg1;
        continue ;
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.iter2"
            ];
      }
    } else if (l2 !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.iter2"
          ];
    } else {
      return /* () */0;
    }
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    var accu = _accu;
    if (l1 !== "[]") {
      if (l2 !== "[]") {
        _l2 = l2.Arg1;
        _l1 = l1.Arg1;
        _accu = Curry._3(f, accu, l1.Arg0, l2.Arg0);
        continue ;
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.fold_left2"
            ];
      }
    } else {
      if (l2 !== "[]") {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.fold_left2"
            ];
      }
      return accu;
    }
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1 !== "[]") {
    if (l2 !== "[]") {
      return Curry._3(f, l1.Arg0, l2.Arg0, fold_right2(f, l1.Arg1, l2.Arg1, accu));
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.fold_right2"
          ];
    }
  } else {
    if (l2 !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.fold_right2"
          ];
    }
    return accu;
  }
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (Curry._1(p, param.Arg0)) {
        _param = param.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (Curry._1(p, param.Arg0)) {
        return true;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function for_all2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      if (l2 !== "[]") {
        if (Curry._2(p, l1.Arg0, l2.Arg0)) {
          _l2 = l2.Arg1;
          _l1 = l1.Arg1;
          continue ;
        } else {
          return false;
        }
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.for_all2"
            ];
      }
    } else if (l2 !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.for_all2"
          ];
    } else {
      return true;
    }
  };
}

function exists2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      if (l2 !== "[]") {
        if (Curry._2(p, l1.Arg0, l2.Arg0)) {
          return true;
        } else {
          _l2 = l2.Arg1;
          _l1 = l1.Arg1;
          continue ;
        }
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.exists2"
            ];
      }
    } else if (l2 !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.exists2"
          ];
    } else {
      return false;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (Caml_obj.caml_equal(param.Arg0, x)) {
        return true;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function memq(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (param.Arg0 === x) {
        return true;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var match = param.Arg0;
      if (Caml_obj.caml_equal(match[0], x)) {
        return match[1];
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function assq(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var match = param.Arg0;
      if (match[0] === x) {
        return match[1];
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function mem_assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (Caml_obj.caml_equal(param.Arg0[0], x)) {
        return true;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function mem_assq(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      if (param.Arg0[0] === x) {
        return true;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function remove_assoc(x, param) {
  if (param !== "[]") {
    var l = param.Arg1;
    var pair = param.Arg0;
    if (Caml_obj.caml_equal(pair[0], x)) {
      return l;
    } else {
      return /* constructor */{
              tag: "::",
              Arg0: pair,
              Arg1: remove_assoc(x, l)
            };
    }
  } else {
    return "[]";
  }
}

function remove_assq(x, param) {
  if (param !== "[]") {
    var l = param.Arg1;
    var pair = param.Arg0;
    if (pair[0] === x) {
      return l;
    } else {
      return /* constructor */{
              tag: "::",
              Arg0: pair,
              Arg1: remove_assq(x, l)
            };
    }
  } else {
    return "[]";
  }
}

function find(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var x = param.Arg0;
      if (Curry._1(p, x)) {
        return x;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function find_all(p) {
  return (function (param) {
      var _accu = "[]";
      var _param = param;
      while(true) {
        var param$1 = _param;
        var accu = _accu;
        if (param$1 !== "[]") {
          var l = param$1.Arg1;
          var x = param$1.Arg0;
          if (Curry._1(p, x)) {
            _param = l;
            _accu = /* constructor */{
              tag: "::",
              Arg0: x,
              Arg1: accu
            };
            continue ;
          } else {
            _param = l;
            continue ;
          }
        } else {
          return rev_append(accu, "[]");
        }
      };
    });
}

function partition(p, l) {
  var _yes = "[]";
  var _no = "[]";
  var _param = l;
  while(true) {
    var param = _param;
    var no = _no;
    var yes = _yes;
    if (param !== "[]") {
      var l$1 = param.Arg1;
      var x = param.Arg0;
      if (Curry._1(p, x)) {
        _param = l$1;
        _yes = /* constructor */{
          tag: "::",
          Arg0: x,
          Arg1: yes
        };
        continue ;
      } else {
        _param = l$1;
        _no = /* constructor */{
          tag: "::",
          Arg0: x,
          Arg1: no
        };
        continue ;
      }
    } else {
      return /* tuple */[
              rev_append(yes, "[]"),
              rev_append(no, "[]")
            ];
    }
  };
}

function split(param) {
  if (param !== "[]") {
    var match = param.Arg0;
    var match$1 = split(param.Arg1);
    return /* tuple */[
            /* constructor */{
              tag: "::",
              Arg0: match[0],
              Arg1: match$1[0]
            },
            /* constructor */{
              tag: "::",
              Arg0: match[1],
              Arg1: match$1[1]
            }
          ];
  } else {
    return /* tuple */[
            "[]",
            "[]"
          ];
  }
}

function combine(l1, l2) {
  if (l1 !== "[]") {
    if (l2 !== "[]") {
      return /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                l1.Arg0,
                l2.Arg0
              ],
              Arg1: combine(l1.Arg1, l2.Arg1)
            };
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.combine"
          ];
    }
  } else if (l2 !== "[]") {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.combine"
        ];
  } else {
    return "[]";
  }
}

function merge(cmp, l1, l2) {
  if (l1 !== "[]") {
    if (l2 !== "[]") {
      var h2 = l2.Arg0;
      var h1 = l1.Arg0;
      if (Curry._2(cmp, h1, h2) <= 0) {
        return /* constructor */{
                tag: "::",
                Arg0: h1,
                Arg1: merge(cmp, l1.Arg1, l2)
              };
      } else {
        return /* constructor */{
                tag: "::",
                Arg0: h2,
                Arg1: merge(cmp, l1, l2.Arg1)
              };
      }
    } else {
      return l1;
    }
  } else {
    return l2;
  }
}

function chop(_k, _l) {
  while(true) {
    var l = _l;
    var k = _k;
    if (k === 0) {
      return l;
    } else if (l !== "[]") {
      _l = l.Arg1;
      _k = k - 1 | 0;
      continue ;
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "list.ml",
              223,
              11
            ]
          ];
    }
  };
}

function stable_sort(cmp, l) {
  var sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l !== "[]") {
        var match = l.Arg1;
        if (match !== "[]") {
          var match$1 = match.Arg1;
          if (match$1 !== "[]") {
            var x3 = match$1.Arg0;
            var x2 = match.Arg0;
            var x1 = l.Arg0;
            if (Curry._2(cmp, x1, x2) <= 0) {
              if (Curry._2(cmp, x2, x3) <= 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else if (Curry._2(cmp, x1, x3) <= 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        }
                      };
              }
            } else if (Curry._2(cmp, x1, x3) <= 0) {
              return /* constructor */{
                      tag: "::",
                      Arg0: x2,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: "[]"
                        }
                      }
                    };
            } else if (Curry._2(cmp, x2, x3) <= 0) {
              return /* constructor */{
                      tag: "::",
                      Arg0: x2,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      }
                    };
            } else {
              return /* constructor */{
                      tag: "::",
                      Arg0: x3,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l !== "[]") {
      var match$2 = l.Arg1;
      if (match$2 !== "[]") {
        var x2$1 = match$2.Arg0;
        var x1$1 = l.Arg0;
        if (Curry._2(cmp, x1$1, x2$1) <= 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x2$1,
                    Arg1: "[]"
                  }
                };
        } else {
          return /* constructor */{
                  tag: "::",
                  Arg0: x2$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x1$1,
                    Arg1: "[]"
                  }
                };
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
    var _accu = "[]";
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (l1 !== "[]") {
        if (l2$1 !== "[]") {
          var h2 = l2$1.Arg0;
          var h1 = l1.Arg0;
          if (Curry._2(cmp, h1, h2) > 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l1 = l1.Arg1;
            continue ;
          } else {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h2,
              Arg1: accu
            };
            _l2 = l2$1.Arg1;
            continue ;
          }
        } else {
          return rev_append(l1, accu);
        }
      } else {
        return rev_append(l2$1, accu);
      }
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l !== "[]") {
        var match = l.Arg1;
        if (match !== "[]") {
          var match$1 = match.Arg1;
          if (match$1 !== "[]") {
            var x3 = match$1.Arg0;
            var x2 = match.Arg0;
            var x1 = l.Arg0;
            if (Curry._2(cmp, x1, x2) > 0) {
              if (Curry._2(cmp, x2, x3) > 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else if (Curry._2(cmp, x1, x3) > 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        }
                      };
              }
            } else if (Curry._2(cmp, x1, x3) > 0) {
              return /* constructor */{
                      tag: "::",
                      Arg0: x2,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: "[]"
                        }
                      }
                    };
            } else if (Curry._2(cmp, x2, x3) > 0) {
              return /* constructor */{
                      tag: "::",
                      Arg0: x2,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      }
                    };
            } else {
              return /* constructor */{
                      tag: "::",
                      Arg0: x3,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l !== "[]") {
      var match$2 = l.Arg1;
      if (match$2 !== "[]") {
        var x2$1 = match$2.Arg0;
        var x1$1 = l.Arg0;
        if (Curry._2(cmp, x1$1, x2$1) > 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x2$1,
                    Arg1: "[]"
                  }
                };
        } else {
          return /* constructor */{
                  tag: "::",
                  Arg0: x2$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x1$1,
                    Arg1: "[]"
                  }
                };
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
    var _accu = "[]";
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (l1 !== "[]") {
        if (l2$1 !== "[]") {
          var h2 = l2$1.Arg0;
          var h1 = l1.Arg0;
          if (Curry._2(cmp, h1, h2) <= 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l1 = l1.Arg1;
            continue ;
          } else {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h2,
              Arg1: accu
            };
            _l2 = l2$1.Arg1;
            continue ;
          }
        } else {
          return rev_append(l1, accu);
        }
      } else {
        return rev_append(l2$1, accu);
      }
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
      if (n === 3 && l !== "[]") {
        var match = l.Arg1;
        if (match !== "[]") {
          var match$1 = match.Arg1;
          if (match$1 !== "[]") {
            var x3 = match$1.Arg0;
            var x2 = match.Arg0;
            var x1 = l.Arg0;
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: "[]"
                      };
              } else if (c$1 < 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: "[]"
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: "[]"
                        }
                      };
              }
            } else if (c < 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: "[]"
                        }
                      };
              } else if (c$2 < 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                var c$3 = Curry._2(cmp, x1, x3);
                if (c$3 === 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        };
                } else if (c$3 < 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x2,
                              Arg1: "[]"
                            }
                          }
                        };
                } else {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x1,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x2,
                              Arg1: "[]"
                            }
                          }
                        };
                }
              }
            } else {
              var c$4 = Curry._2(cmp, x1, x3);
              if (c$4 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      };
              } else if (c$4 < 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                var c$5 = Curry._2(cmp, x2, x3);
                if (c$5 === 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x1,
                            Arg1: "[]"
                          }
                        };
                } else if (c$5 < 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x1,
                              Arg1: "[]"
                            }
                          }
                        };
                } else {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x1,
                              Arg1: "[]"
                            }
                          }
                        };
                }
              }
            }
          }
          
        }
        
      }
      
    } else if (l !== "[]") {
      var match$2 = l.Arg1;
      if (match$2 !== "[]") {
        var x2$1 = match$2.Arg0;
        var x1$1 = l.Arg0;
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: "[]"
                };
        } else if (c$6 < 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x2$1,
                    Arg1: "[]"
                  }
                };
        } else {
          return /* constructor */{
                  tag: "::",
                  Arg0: x2$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x1$1,
                    Arg1: "[]"
                  }
                };
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
    var _accu = "[]";
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (l1 !== "[]") {
        if (l2$1 !== "[]") {
          var t2 = l2$1.Arg1;
          var h2 = l2$1.Arg0;
          var t1 = l1.Arg1;
          var h1 = l1.Arg0;
          var c$7 = Curry._2(cmp, h1, h2);
          if (c$7 === 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l2 = t2;
            _l1 = t1;
            continue ;
          } else if (c$7 > 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l1 = t1;
            continue ;
          } else {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h2,
              Arg1: accu
            };
            _l2 = t2;
            continue ;
          }
        } else {
          return rev_append(l1, accu);
        }
      } else {
        return rev_append(l2$1, accu);
      }
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l !== "[]") {
        var match = l.Arg1;
        if (match !== "[]") {
          var match$1 = match.Arg1;
          if (match$1 !== "[]") {
            var x3 = match$1.Arg0;
            var x2 = match.Arg0;
            var x1 = l.Arg0;
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: "[]"
                      };
              } else if (c$1 > 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: "[]"
                        }
                      };
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: x3,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: "[]"
                        }
                      };
              }
            } else if (c > 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: "[]"
                        }
                      };
              } else if (c$2 > 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                var c$3 = Curry._2(cmp, x1, x3);
                if (c$3 === 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: "[]"
                          }
                        };
                } else if (c$3 > 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x2,
                              Arg1: "[]"
                            }
                          }
                        };
                } else {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x1,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x2,
                              Arg1: "[]"
                            }
                          }
                        };
                }
              }
            } else {
              var c$4 = Curry._2(cmp, x1, x3);
              if (c$4 === 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: "[]"
                        }
                      };
              } else if (c$4 > 0) {
                return /* constructor */{
                        tag: "::",
                        Arg0: x2,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: x1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: "[]"
                          }
                        }
                      };
              } else {
                var c$5 = Curry._2(cmp, x2, x3);
                if (c$5 === 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x1,
                            Arg1: "[]"
                          }
                        };
                } else if (c$5 > 0) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x3,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x1,
                              Arg1: "[]"
                            }
                          }
                        };
                } else {
                  return /* constructor */{
                          tag: "::",
                          Arg0: x3,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: x2,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: x1,
                              Arg1: "[]"
                            }
                          }
                        };
                }
              }
            }
          }
          
        }
        
      }
      
    } else if (l !== "[]") {
      var match$2 = l.Arg1;
      if (match$2 !== "[]") {
        var x2$1 = match$2.Arg0;
        var x1$1 = l.Arg0;
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: "[]"
                };
        } else if (c$6 > 0) {
          return /* constructor */{
                  tag: "::",
                  Arg0: x1$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x2$1,
                    Arg1: "[]"
                  }
                };
        } else {
          return /* constructor */{
                  tag: "::",
                  Arg0: x2$1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: x1$1,
                    Arg1: "[]"
                  }
                };
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
    var _accu = "[]";
    while(true) {
      var accu = _accu;
      var l2$1 = _l2;
      var l1 = _l1;
      if (l1 !== "[]") {
        if (l2$1 !== "[]") {
          var t2 = l2$1.Arg1;
          var h2 = l2$1.Arg0;
          var t1 = l1.Arg1;
          var h1 = l1.Arg0;
          var c$7 = Curry._2(cmp, h1, h2);
          if (c$7 === 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l2 = t2;
            _l1 = t1;
            continue ;
          } else if (c$7 < 0) {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h1,
              Arg1: accu
            };
            _l1 = t1;
            continue ;
          } else {
            _accu = /* constructor */{
              tag: "::",
              Arg0: h2,
              Arg1: accu
            };
            _l2 = t2;
            continue ;
          }
        } else {
          return rev_append(l1, accu);
        }
      } else {
        return rev_append(l2$1, accu);
      }
    };
  };
  var len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

var append = Pervasives.$at;

var concat = flatten;

var filter = find_all;

var sort = stable_sort;

var fast_sort = stable_sort;

exports.length = length;
exports.hd = hd;
exports.tl = tl;
exports.nth = nth;
exports.rev = rev;
exports.append = append;
exports.rev_append = rev_append;
exports.concat = concat;
exports.flatten = flatten;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi$1;
exports.rev_map = rev_map;
exports.fold_left = fold_left;
exports.fold_right = fold_right;
exports.iter2 = iter2;
exports.map2 = map2;
exports.rev_map2 = rev_map2;
exports.fold_left2 = fold_left2;
exports.fold_right2 = fold_right2;
exports.for_all = for_all;
exports.exists = exists;
exports.for_all2 = for_all2;
exports.exists2 = exists2;
exports.mem = mem;
exports.memq = memq;
exports.find = find;
exports.filter = filter;
exports.find_all = find_all;
exports.partition = partition;
exports.assoc = assoc;
exports.assq = assq;
exports.mem_assoc = mem_assoc;
exports.mem_assq = mem_assq;
exports.remove_assoc = remove_assoc;
exports.remove_assq = remove_assq;
exports.split = split;
exports.combine = combine;
exports.sort = sort;
exports.stable_sort = stable_sort;
exports.fast_sort = fast_sort;
exports.sort_uniq = sort_uniq;
exports.merge = merge;
/* No side effect */
