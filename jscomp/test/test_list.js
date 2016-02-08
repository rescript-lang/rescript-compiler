// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives      = require("../stdlib/pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Caml_primitive  = require("../runtime/caml_primitive");
var Caml_curry      = require("../runtime/caml_curry");
var List            = require("../stdlib/list");

function length_aux(_len, _param) {
  while(true) {
    var param = _param;
    var len = _len;
    if (param) {
      _param = param[2];
      _len = len + 1;
      continue ;
      
    }
    else {
      return len;
    }
  };
}

function length(l) {
  return length_aux(0, l);
}

function hd(param) {
  if (param) {
    return param[1];
  }
  else {
    return Pervasives.failwith("hd");
  }
}

function tl(param) {
  if (param) {
    return param[2];
  }
  else {
    return Pervasives.failwith("tl");
  }
}

function nth(l, n) {
  if (n < 0) {
    return Pervasives.invalid_arg("List.nth");
  }
  else {
    var _l = l;
    var _n = n;
    while(true) {
      var n$1 = _n;
      var l$1 = _l;
      if (l$1) {
        if (n$1) {
          _n = n$1 - 1;
          _l = l$1[2];
          continue ;
          
        }
        else {
          return l$1[1];
        }
      }
      else {
        return Pervasives.failwith("nth");
      }
    };
  }
}

function rev_append(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      _l2 = [
        /* :: */0,
        l1[1],
        l2
      ];
      _l1 = l1[2];
      continue ;
      
    }
    else {
      return l2;
    }
  };
}

function rev(l) {
  return rev_append(l, /* [] */0);
}

function flatten(param) {
  if (param) {
    return Pervasives.$at(param[1], flatten(param[2]));
  }
  else {
    return /* [] */0;
  }
}

function map(f, param) {
  if (param) {
    var r = Caml_curry.app1(f, param[1]);
    return [
            /* :: */0,
            r,
            map(f, param[2])
          ];
  }
  else {
    return /* [] */0;
  }
}

function mapi(i, f, param) {
  if (param) {
    var r = Caml_curry.app2(f, i, param[1]);
    return [
            /* :: */0,
            r,
            mapi(i + 1, f, param[2])
          ];
  }
  else {
    return /* [] */0;
  }
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
    if (param) {
      _param = param[2];
      _accu = [
        /* :: */0,
        Caml_curry.app1(f, param[1]),
        accu
      ];
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      Caml_curry.app1(f, param[1]);
      _param = param[2];
      continue ;
      
    }
    else {
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
    if (param) {
      Caml_curry.app2(f$1, i, param[1]);
      _param = param[2];
      _i = i + 1;
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

function fold_left(f, _accu, _l) {
  while(true) {
    var l = _l;
    var accu = _accu;
    if (l) {
      _l = l[2];
      _accu = Caml_curry.app2(f, accu, l[1]);
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function fold_right(f, l, accu) {
  if (l) {
    return Caml_curry.app2(f, l[1], fold_right(f, l[2], accu));
  }
  else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1) {
    if (l2) {
      var r = Caml_curry.app2(f, l1[1], l2[1]);
      return [
              /* :: */0,
              r,
              map2(f, l1[2], l2[2])
            ];
    }
    else {
      return Pervasives.invalid_arg("List.map2");
    }
  }
  else if (l2) {
    return Pervasives.invalid_arg("List.map2");
  }
  else {
    return /* [] */0;
  }
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
        _l2 = l2$1[2];
        _l1 = l1$1[2];
        _accu = [
          /* :: */0,
          Caml_curry.app2(f, l1$1[1], l2$1[1]),
          accu
        ];
        continue ;
        
      }
      else {
        return Pervasives.invalid_arg("List.rev_map2");
      }
    }
    else if (l2$1) {
      return Pervasives.invalid_arg("List.rev_map2");
    }
    else {
      return accu;
    }
  };
}

function iter2(f, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        Caml_curry.app2(f, l1[1], l2[1]);
        _l2 = l2[2];
        _l1 = l1[2];
        continue ;
        
      }
      else {
        return Pervasives.invalid_arg("List.iter2");
      }
    }
    else if (l2) {
      return Pervasives.invalid_arg("List.iter2");
    }
    else {
      return /* () */0;
    }
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    var accu = _accu;
    if (l1) {
      if (l2) {
        _l2 = l2[2];
        _l1 = l1[2];
        _accu = Caml_curry.app3(f, accu, l1[1], l2[1]);
        continue ;
        
      }
      else {
        return Pervasives.invalid_arg("List.fold_left2");
      }
    }
    else if (l2) {
      return Pervasives.invalid_arg("List.fold_left2");
    }
    else {
      return accu;
    }
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return Caml_curry.app3(f, l1[1], l2[1], fold_right2(f, l1[2], l2[2], accu));
    }
    else {
      return Pervasives.invalid_arg("List.fold_right2");
    }
  }
  else if (l2) {
    return Pervasives.invalid_arg("List.fold_right2");
  }
  else {
    return accu;
  }
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_curry.app1(p, param[1])) {
        _param = param[2];
        continue ;
        
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* true */1;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_curry.app1(p, param[1])) {
        return /* true */1;
      }
      else {
        _param = param[2];
        continue ;
        
      }
    }
    else {
      return /* false */0;
    }
  };
}

function for_all2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (Caml_curry.app2(p, l1[1], l2[1])) {
          _l2 = l2[2];
          _l1 = l1[2];
          continue ;
          
        }
        else {
          return /* false */0;
        }
      }
      else {
        return Pervasives.invalid_arg("List.for_all2");
      }
    }
    else if (l2) {
      return Pervasives.invalid_arg("List.for_all2");
    }
    else {
      return /* true */1;
    }
  };
}

function exists2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (Caml_curry.app2(p, l1[1], l2[1])) {
          return /* true */1;
        }
        else {
          _l2 = l2[2];
          _l1 = l1[2];
          continue ;
          
        }
      }
      else {
        return Pervasives.invalid_arg("List.exists2");
      }
    }
    else if (l2) {
      return Pervasives.invalid_arg("List.exists2");
    }
    else {
      return /* false */0;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_primitive.caml_compare(param[1], x)) {
        _param = param[2];
        continue ;
        
      }
      else {
        return /* true */1;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function memq(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (param[1] === x) {
        return /* true */1;
      }
      else {
        _param = param[2];
        continue ;
        
      }
    }
    else {
      return /* false */0;
    }
  };
}

function assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param[1];
      if (Caml_primitive.caml_compare(match[1], x)) {
        _param = param[2];
        continue ;
        
      }
      else {
        return match[2];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function assq(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param[1];
      if (match[1] === x) {
        return match[2];
      }
      else {
        _param = param[2];
        continue ;
        
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function mem_assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_primitive.caml_compare(param[1][1], x)) {
        _param = param[2];
        continue ;
        
      }
      else {
        return /* true */1;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function mem_assq(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (param[1][1] === x) {
        return /* true */1;
      }
      else {
        _param = param[2];
        continue ;
        
      }
    }
    else {
      return /* false */0;
    }
  };
}

function remove_assoc(x, param) {
  if (param) {
    var l = param[2];
    var pair = param[1];
    if (Caml_primitive.caml_compare(pair[1], x)) {
      return [
              /* :: */0,
              pair,
              remove_assoc(x, l)
            ];
    }
    else {
      return l;
    }
  }
  else {
    return /* [] */0;
  }
}

function remove_assq(x, param) {
  if (param) {
    var l = param[2];
    var pair = param[1];
    if (pair[1] === x) {
      return l;
    }
    else {
      return [
              /* :: */0,
              pair,
              remove_assq(x, l)
            ];
    }
  }
  else {
    return /* [] */0;
  }
}

function find(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param[1];
      if (Caml_curry.app1(p, x)) {
        return x;
      }
      else {
        _param = param[2];
        continue ;
        
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function find_all(p) {
  return function (param) {
    var _accu = /* [] */0;
    var _param = param;
    while(true) {
      var param$1 = _param;
      var accu = _accu;
      if (param$1) {
        var l = param$1[2];
        var x = param$1[1];
        if (Caml_curry.app1(p, x)) {
          _param = l;
          _accu = [
            /* :: */0,
            x,
            accu
          ];
          continue ;
          
        }
        else {
          _param = l;
          continue ;
          
        }
      }
      else {
        return rev_append(accu, /* [] */0);
      }
    };
  };
}

function partition(p, l) {
  var _yes = /* [] */0;
  var _no = /* [] */0;
  var _param = l;
  while(true) {
    var param = _param;
    var no = _no;
    var yes = _yes;
    if (param) {
      var l$1 = param[2];
      var x = param[1];
      if (Caml_curry.app1(p, x)) {
        _param = l$1;
        _yes = [
          /* :: */0,
          x,
          yes
        ];
        continue ;
        
      }
      else {
        _param = l$1;
        _no = [
          /* :: */0,
          x,
          no
        ];
        continue ;
        
      }
    }
    else {
      return [
              /* tuple */0,
              rev_append(yes, /* [] */0),
              rev_append(no, /* [] */0)
            ];
    }
  };
}

function split(param) {
  if (param) {
    var match = param[1];
    var match$1 = split(param[2]);
    return [
            /* tuple */0,
            [
              /* :: */0,
              match[1],
              match$1[1]
            ],
            [
              /* :: */0,
              match[2],
              match$1[2]
            ]
          ];
  }
  else {
    return [
            /* tuple */0,
            /* [] */0,
            /* [] */0
          ];
  }
}

function combine(l1, l2) {
  if (l1) {
    if (l2) {
      return [
              /* :: */0,
              [
                /* tuple */0,
                l1[1],
                l2[1]
              ],
              combine(l1[2], l2[2])
            ];
    }
    else {
      return Pervasives.invalid_arg("List.combine");
    }
  }
  else if (l2) {
    return Pervasives.invalid_arg("List.combine");
  }
  else {
    return /* [] */0;
  }
}

function merge(cmp, l1, l2) {
  if (l1) {
    if (l2) {
      var h2 = l2[1];
      var h1 = l1[1];
      if (Caml_curry.app2(cmp, h1, h2) <= 0) {
        return [
                /* :: */0,
                h1,
                merge(cmp, l1[2], l2)
              ];
      }
      else {
        return [
                /* :: */0,
                h2,
                merge(cmp, l1, l2[2])
              ];
      }
    }
    else {
      return l1;
    }
  }
  else {
    return l2;
  }
}

function chop(_k, _l) {
  while(true) {
    var l = _l;
    var k = _k;
    if (k) {
      if (l) {
        _l = l[2];
        _k = k - 1;
        continue ;
        
      }
      else {
        throw [
              0,
              Caml_exceptions.Assert_failure,
              [
                0,
                "test_list.ml",
                224,
                11
              ]
            ];
      }
    }
    else {
      return l;
    }
  };
}

function stable_sort(cmp, l) {
  var sort = function (n, l) {
    var exit = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit = 1;
      }
      else if (l) {
        var match = l[2];
        if (match) {
          var match$1 = match[2];
          if (match$1) {
            var x3 = match$1[1];
            var x2 = match[1];
            var x1 = l[1];
            if (Caml_curry.app2(cmp, x1, x2) <= 0) {
              if (Caml_curry.app2(cmp, x2, x3) <= 0) {
                return [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              else if (Caml_curry.app2(cmp, x1, x3) <= 0) {
                return [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x3,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              else {
                return [
                        /* :: */0,
                        x3,
                        [
                          /* :: */0,
                          x1,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            }
            else if (Caml_curry.app2(cmp, x1, x3) <= 0) {
              return [
                      /* :: */0,
                      x2,
                      [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            else if (Caml_curry.app2(cmp, x2, x3) <= 0) {
              return [
                      /* :: */0,
                      x2,
                      [
                        /* :: */0,
                        x3,
                        [
                          /* :: */0,
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            else {
              return [
                      /* :: */0,
                      x3,
                      [
                        /* :: */0,
                        x2,
                        [
                          /* :: */0,
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
    }
    else if (l) {
      var match$2 = l[2];
      if (match$2) {
        var x2$1 = match$2[1];
        var x1$1 = l[1];
        if (Caml_curry.app2(cmp, x1$1, x2$1) <= 0) {
          return [
                  /* :: */0,
                  x1$1,
                  [
                    /* :: */0,
                    x2$1,
                    /* [] */0
                  ]
                ];
        }
        else {
          return [
                  /* :: */0,
                  x2$1,
                  [
                    /* :: */0,
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      else {
        exit = 1;
      }
    }
    else {
      exit = 1;
    }
    if (exit === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1;
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
        if (l1) {
          if (l2$1) {
            var h2 = l2$1[1];
            var h1 = l1[1];
            if (Caml_curry.app2(cmp, h1, h2) > 0) {
              _accu = [
                /* :: */0,
                h1,
                accu
              ];
              _l1 = l1[2];
              continue ;
              
            }
            else {
              _accu = [
                /* :: */0,
                h2,
                accu
              ];
              _l2 = l2$1[2];
              continue ;
              
            }
          }
          else {
            return rev_append(l1, accu);
          }
        }
        else {
          return rev_append(l2$1, accu);
        }
      };
    }
    
  };
  var rev_sort = function (n, l) {
    var exit = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit = 1;
      }
      else if (l) {
        var match = l[2];
        if (match) {
          var match$1 = match[2];
          if (match$1) {
            var x3 = match$1[1];
            var x2 = match[1];
            var x1 = l[1];
            if (Caml_curry.app2(cmp, x1, x2) > 0) {
              if (Caml_curry.app2(cmp, x2, x3) > 0) {
                return [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x3,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              else if (Caml_curry.app2(cmp, x1, x3) > 0) {
                return [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x3,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
              else {
                return [
                        /* :: */0,
                        x3,
                        [
                          /* :: */0,
                          x1,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ]
                      ];
              }
            }
            else if (Caml_curry.app2(cmp, x1, x3) > 0) {
              return [
                      /* :: */0,
                      x2,
                      [
                        /* :: */0,
                        x1,
                        [
                          /* :: */0,
                          x3,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            else if (Caml_curry.app2(cmp, x2, x3) > 0) {
              return [
                      /* :: */0,
                      x2,
                      [
                        /* :: */0,
                        x3,
                        [
                          /* :: */0,
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
            else {
              return [
                      /* :: */0,
                      x3,
                      [
                        /* :: */0,
                        x2,
                        [
                          /* :: */0,
                          x1,
                          /* [] */0
                        ]
                      ]
                    ];
            }
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
    }
    else if (l) {
      var match$2 = l[2];
      if (match$2) {
        var x2$1 = match$2[1];
        var x1$1 = l[1];
        if (Caml_curry.app2(cmp, x1$1, x2$1) > 0) {
          return [
                  /* :: */0,
                  x1$1,
                  [
                    /* :: */0,
                    x2$1,
                    /* [] */0
                  ]
                ];
        }
        else {
          return [
                  /* :: */0,
                  x2$1,
                  [
                    /* :: */0,
                    x1$1,
                    /* [] */0
                  ]
                ];
        }
      }
      else {
        exit = 1;
      }
    }
    else {
      exit = 1;
    }
    if (exit === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1;
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
        if (l1) {
          if (l2$1) {
            var h2 = l2$1[1];
            var h1 = l1[1];
            if (Caml_curry.app2(cmp, h1, h2) <= 0) {
              _accu = [
                /* :: */0,
                h1,
                accu
              ];
              _l1 = l1[2];
              continue ;
              
            }
            else {
              _accu = [
                /* :: */0,
                h2,
                accu
              ];
              _l2 = l2$1[2];
              continue ;
              
            }
          }
          else {
            return rev_append(l1, accu);
          }
        }
        else {
          return rev_append(l2$1, accu);
        }
      };
    }
    
  };
  var len = length_aux(0, l);
  if (len < 2) {
    return l;
  }
  else {
    return sort(len, l);
  }
}

function sort_uniq(cmp, l) {
  var sort = function (n, l) {
    var exit = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit = 1;
      }
      else if (l) {
        var match = l[2];
        if (match) {
          var match$1 = match[2];
          if (match$1) {
            var x3 = match$1[1];
            var x2 = match[1];
            var x1 = l[1];
            var c = Caml_curry.app2(cmp, x1, x2);
            if (c) {
              if (c < 0) {
                var c$1 = Caml_curry.app2(cmp, x2, x3);
                if (c$1) {
                  if (c$1 < 0) {
                    return [
                            /* :: */0,
                            x1,
                            [
                              /* :: */0,
                              x2,
                              [
                                /* :: */0,
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  }
                  else {
                    var c$2 = Caml_curry.app2(cmp, x1, x3);
                    if (c$2) {
                      if (c$2 < 0) {
                        return [
                                /* :: */0,
                                x1,
                                [
                                  /* :: */0,
                                  x3,
                                  [
                                    /* :: */0,
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                      else {
                        return [
                                /* :: */0,
                                x3,
                                [
                                  /* :: */0,
                                  x1,
                                  [
                                    /* :: */0,
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    }
                    else {
                      return [
                              /* :: */0,
                              x1,
                              [
                                /* :: */0,
                                x2,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                }
                else {
                  return [
                          /* :: */0,
                          x1,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              }
              else {
                var c$3 = Caml_curry.app2(cmp, x1, x3);
                if (c$3) {
                  if (c$3 < 0) {
                    return [
                            /* :: */0,
                            x2,
                            [
                              /* :: */0,
                              x1,
                              [
                                /* :: */0,
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  }
                  else {
                    var c$4 = Caml_curry.app2(cmp, x2, x3);
                    if (c$4) {
                      if (c$4 < 0) {
                        return [
                                /* :: */0,
                                x2,
                                [
                                  /* :: */0,
                                  x3,
                                  [
                                    /* :: */0,
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                      else {
                        return [
                                /* :: */0,
                                x3,
                                [
                                  /* :: */0,
                                  x2,
                                  [
                                    /* :: */0,
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    }
                    else {
                      return [
                              /* :: */0,
                              x2,
                              [
                                /* :: */0,
                                x1,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                }
                else {
                  return [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x1,
                            /* [] */0
                          ]
                        ];
                }
              }
            }
            else {
              var c$5 = Caml_curry.app2(cmp, x2, x3);
              if (c$5) {
                if (c$5 < 0) {
                  return [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x3,
                            /* [] */0
                          ]
                        ];
                }
                else {
                  return [
                          /* :: */0,
                          x3,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              }
              else {
                return [
                        /* :: */0,
                        x2,
                        /* [] */0
                      ];
              }
            }
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
    }
    else if (l) {
      var match$2 = l[2];
      if (match$2) {
        var x2$1 = match$2[1];
        var x1$1 = l[1];
        var c$6 = Caml_curry.app2(cmp, x1$1, x2$1);
        if (c$6) {
          if (c$6 < 0) {
            return [
                    /* :: */0,
                    x1$1,
                    [
                      /* :: */0,
                      x2$1,
                      /* [] */0
                    ]
                  ];
          }
          else {
            return [
                    /* :: */0,
                    x2$1,
                    [
                      /* :: */0,
                      x1$1,
                      /* [] */0
                    ]
                  ];
          }
        }
        else {
          return [
                  /* :: */0,
                  x1$1,
                  /* [] */0
                ];
        }
      }
      else {
        exit = 1;
      }
    }
    else {
      exit = 1;
    }
    if (exit === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1;
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
        if (l1) {
          if (l2$1) {
            var t2 = l2$1[2];
            var h2 = l2$1[1];
            var t1 = l1[2];
            var h1 = l1[1];
            var c$7 = Caml_curry.app2(cmp, h1, h2);
            if (c$7) {
              if (c$7 > 0) {
                _accu = [
                  /* :: */0,
                  h1,
                  accu
                ];
                _l1 = t1;
                continue ;
                
              }
              else {
                _accu = [
                  /* :: */0,
                  h2,
                  accu
                ];
                _l2 = t2;
                continue ;
                
              }
            }
            else {
              _accu = [
                /* :: */0,
                h1,
                accu
              ];
              _l2 = t2;
              _l1 = t1;
              continue ;
              
            }
          }
          else {
            return rev_append(l1, accu);
          }
        }
        else {
          return rev_append(l2$1, accu);
        }
      };
    }
    
  };
  var rev_sort = function (n, l) {
    var exit = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit = 1;
      }
      else if (l) {
        var match = l[2];
        if (match) {
          var match$1 = match[2];
          if (match$1) {
            var x3 = match$1[1];
            var x2 = match[1];
            var x1 = l[1];
            var c = Caml_curry.app2(cmp, x1, x2);
            if (c) {
              if (c > 0) {
                var c$1 = Caml_curry.app2(cmp, x2, x3);
                if (c$1) {
                  if (c$1 > 0) {
                    return [
                            /* :: */0,
                            x1,
                            [
                              /* :: */0,
                              x2,
                              [
                                /* :: */0,
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  }
                  else {
                    var c$2 = Caml_curry.app2(cmp, x1, x3);
                    if (c$2) {
                      if (c$2 > 0) {
                        return [
                                /* :: */0,
                                x1,
                                [
                                  /* :: */0,
                                  x3,
                                  [
                                    /* :: */0,
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                      else {
                        return [
                                /* :: */0,
                                x3,
                                [
                                  /* :: */0,
                                  x1,
                                  [
                                    /* :: */0,
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    }
                    else {
                      return [
                              /* :: */0,
                              x1,
                              [
                                /* :: */0,
                                x2,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                }
                else {
                  return [
                          /* :: */0,
                          x1,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              }
              else {
                var c$3 = Caml_curry.app2(cmp, x1, x3);
                if (c$3) {
                  if (c$3 > 0) {
                    return [
                            /* :: */0,
                            x2,
                            [
                              /* :: */0,
                              x1,
                              [
                                /* :: */0,
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  }
                  else {
                    var c$4 = Caml_curry.app2(cmp, x2, x3);
                    if (c$4) {
                      if (c$4 > 0) {
                        return [
                                /* :: */0,
                                x2,
                                [
                                  /* :: */0,
                                  x3,
                                  [
                                    /* :: */0,
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                      else {
                        return [
                                /* :: */0,
                                x3,
                                [
                                  /* :: */0,
                                  x2,
                                  [
                                    /* :: */0,
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    }
                    else {
                      return [
                              /* :: */0,
                              x2,
                              [
                                /* :: */0,
                                x1,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                }
                else {
                  return [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x1,
                            /* [] */0
                          ]
                        ];
                }
              }
            }
            else {
              var c$5 = Caml_curry.app2(cmp, x2, x3);
              if (c$5) {
                if (c$5 > 0) {
                  return [
                          /* :: */0,
                          x2,
                          [
                            /* :: */0,
                            x3,
                            /* [] */0
                          ]
                        ];
                }
                else {
                  return [
                          /* :: */0,
                          x3,
                          [
                            /* :: */0,
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              }
              else {
                return [
                        /* :: */0,
                        x2,
                        /* [] */0
                      ];
              }
            }
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
    }
    else if (l) {
      var match$2 = l[2];
      if (match$2) {
        var x2$1 = match$2[1];
        var x1$1 = l[1];
        var c$6 = Caml_curry.app2(cmp, x1$1, x2$1);
        if (c$6) {
          if (c$6 > 0) {
            return [
                    /* :: */0,
                    x1$1,
                    [
                      /* :: */0,
                      x2$1,
                      /* [] */0
                    ]
                  ];
          }
          else {
            return [
                    /* :: */0,
                    x2$1,
                    [
                      /* :: */0,
                      x1$1,
                      /* [] */0
                    ]
                  ];
          }
        }
        else {
          return [
                  /* :: */0,
                  x1$1,
                  /* [] */0
                ];
        }
      }
      else {
        exit = 1;
      }
    }
    else {
      exit = 1;
    }
    if (exit === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1;
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
        if (l1) {
          if (l2$1) {
            var t2 = l2$1[2];
            var h2 = l2$1[1];
            var t1 = l1[2];
            var h1 = l1[1];
            var c$7 = Caml_curry.app2(cmp, h1, h2);
            if (c$7) {
              if (c$7 < 0) {
                _accu = [
                  /* :: */0,
                  h1,
                  accu
                ];
                _l1 = t1;
                continue ;
                
              }
              else {
                _accu = [
                  /* :: */0,
                  h2,
                  accu
                ];
                _l2 = t2;
                continue ;
                
              }
            }
            else {
              _accu = [
                /* :: */0,
                h1,
                accu
              ];
              _l2 = t2;
              _l1 = t1;
              continue ;
              
            }
          }
          else {
            return rev_append(l1, accu);
          }
        }
        else {
          return rev_append(l2$1, accu);
        }
      };
    }
    
  };
  var len = length_aux(0, l);
  if (len < 2) {
    return l;
  }
  else {
    return sort(len, l);
  }
}

var u = List.length;

var append = Pervasives.$at;

var concat = flatten;

var filter = find_all;

var sort = stable_sort;

var fast_sort = stable_sort;

exports.u            = u;
exports.length_aux   = length_aux;
exports.length       = length;
exports.hd           = hd;
exports.tl           = tl;
exports.nth          = nth;
exports.append       = append;
exports.rev_append   = rev_append;
exports.rev          = rev;
exports.flatten      = flatten;
exports.concat       = concat;
exports.map          = map;
exports.mapi         = mapi$1;
exports.rev_map      = rev_map;
exports.iter         = iter;
exports.iteri        = iteri;
exports.fold_left    = fold_left;
exports.fold_right   = fold_right;
exports.map2         = map2;
exports.rev_map2     = rev_map2;
exports.iter2        = iter2;
exports.fold_left2   = fold_left2;
exports.fold_right2  = fold_right2;
exports.for_all      = for_all;
exports.exists       = exists;
exports.for_all2     = for_all2;
exports.exists2      = exists2;
exports.mem          = mem;
exports.memq         = memq;
exports.assoc        = assoc;
exports.assq         = assq;
exports.mem_assoc    = mem_assoc;
exports.mem_assq     = mem_assq;
exports.remove_assoc = remove_assoc;
exports.remove_assq  = remove_assq;
exports.find         = find;
exports.find_all     = find_all;
exports.filter       = filter;
exports.partition    = partition;
exports.split        = split;
exports.combine      = combine;
exports.merge        = merge;
exports.chop         = chop;
exports.stable_sort  = stable_sort;
exports.sort         = sort;
exports.fast_sort    = fast_sort;
exports.sort_uniq    = sort_uniq;
/* No side effect */
