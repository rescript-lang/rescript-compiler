

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
    _param = param.tl;
    _len = len + 1 | 0;
    continue ;
  };
}

function cons(a, l) {
  return {
          hd: a,
          tl: l
        };
}

function hd(param) {
  if (param) {
    return param.hd;
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "hd",
        Error: new Error()
      };
}

function tl(param) {
  if (param) {
    return param.tl;
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "tl",
        Error: new Error()
      };
}

function nth(l, n) {
  if (n < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.nth",
          Error: new Error()
        };
  }
  var _l = l;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var l$1 = _l;
    if (l$1) {
      if (n$1 === 0) {
        return l$1.hd;
      }
      _n = n$1 - 1 | 0;
      _l = l$1.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Failure",
          _1: "nth",
          Error: new Error()
        };
  };
}

function nth_opt(l, n) {
  if (n < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.nth",
          Error: new Error()
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
      return Caml_option.some(l$1.hd);
    }
    _n = n$1 - 1 | 0;
    _l = l$1.tl;
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
    _l2 = {
      hd: l1.hd,
      tl: l2
    };
    _l1 = l1.tl;
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
    _acc = {
      hd: Curry._1(f, i),
      tl: acc
    };
    continue ;
  };
}

function init_aux(i, n, f) {
  if (i >= n) {
    return /* [] */0;
  }
  var r = Curry._1(f, i);
  return {
          hd: r,
          tl: init_aux(i + 1 | 0, n, f)
        };
}

function init(len, f) {
  if (len < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.init",
          Error: new Error()
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
    return Pervasives.$at(param.hd, flatten(param.tl));
  } else {
    return /* [] */0;
  }
}

function map(f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = Curry._1(f, param.hd);
  return {
          hd: r,
          tl: map(f, param.tl)
        };
}

function mapi(i, f, param) {
  if (!param) {
    return /* [] */0;
  }
  var r = Curry._2(f, i, param.hd);
  return {
          hd: r,
          tl: mapi(i + 1 | 0, f, param.tl)
        };
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
    _param = param.tl;
    _accu = {
      hd: Curry._1(f, param.hd),
      tl: accu
    };
    continue ;
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    Curry._1(f, param.hd);
    _param = param.tl;
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
    Curry._2(f, i, param.hd);
    _param = param.tl;
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
    _l = l.tl;
    _accu = Curry._2(f, accu, l.hd);
    continue ;
  };
}

function fold_right(f, l, accu) {
  if (l) {
    return Curry._2(f, l.hd, fold_right(f, l.tl, accu));
  } else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1) {
    if (l2) {
      var r = Curry._2(f, l1.hd, l2.hd);
      return {
              hd: r,
              tl: map2(f, l1.tl, l2.tl)
            };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.map2",
          Error: new Error()
        };
  }
  if (!l2) {
    return /* [] */0;
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "List.map2",
        Error: new Error()
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
        _l2 = l2$1.tl;
        _l1 = l1$1.tl;
        _accu = {
          hd: Curry._2(f, l1$1.hd, l2$1.hd),
          tl: accu
        };
        continue ;
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.rev_map2",
            Error: new Error()
          };
    }
    if (l2$1) {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.rev_map2",
            Error: new Error()
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
        Curry._2(f, l1.hd, l2.hd);
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue ;
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.iter2",
            Error: new Error()
          };
    }
    if (!l2) {
      return ;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.iter2",
          Error: new Error()
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
        _l2 = l2.tl;
        _l1 = l1.tl;
        _accu = Curry._3(f, accu, l1.hd, l2.hd);
        continue ;
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.fold_left2",
            Error: new Error()
          };
    }
    if (l2) {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.fold_left2",
            Error: new Error()
          };
    }
    return accu;
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return Curry._3(f, l1.hd, l2.hd, fold_right2(f, l1.tl, l2.tl, accu));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.fold_right2",
          Error: new Error()
        };
  }
  if (l2) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.fold_right2",
          Error: new Error()
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
    if (!Curry._1(p, param.hd)) {
      return false;
    }
    _param = param.tl;
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._1(p, param.hd)) {
      return true;
    }
    _param = param.tl;
    continue ;
  };
}

function for_all2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (!Curry._2(p, l1.hd, l2.hd)) {
          return false;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue ;
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.for_all2",
            Error: new Error()
          };
    }
    if (!l2) {
      return true;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.for_all2",
          Error: new Error()
        };
  };
}

function exists2(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (Curry._2(p, l1.hd, l2.hd)) {
          return true;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue ;
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.exists2",
            Error: new Error()
          };
    }
    if (!l2) {
      return false;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.exists2",
          Error: new Error()
        };
  };
}

function mem(x, _set) {
  while(true) {
    var set = _set;
    if (!set) {
      return false;
    }
    if (Caml_obj.equal(set.hd, x)) {
      return true;
    }
    _set = set.tl;
    continue ;
  };
}

function memq(x, _set) {
  while(true) {
    var set = _set;
    if (!set) {
      return false;
    }
    if (set.hd === x) {
      return true;
    }
    _set = set.tl;
    continue ;
  };
}

function assoc(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _param = param.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function assoc_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var match = param.hd;
    if (Caml_obj.equal(match[0], x)) {
      return Caml_option.some(match[1]);
    }
    _param = param.tl;
    continue ;
  };
}

function assq(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var match = param.hd;
      if (match[0] === x) {
        return match[1];
      }
      _param = param.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function assq_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var match = param.hd;
    if (match[0] === x) {
      return Caml_option.some(match[1]);
    }
    _param = param.tl;
    continue ;
  };
}

function mem_assoc(x, _map) {
  while(true) {
    var map = _map;
    if (!map) {
      return false;
    }
    if (Caml_obj.equal(map.hd[0], x)) {
      return true;
    }
    _map = map.tl;
    continue ;
  };
}

function mem_assq(x, _map) {
  while(true) {
    var map = _map;
    if (!map) {
      return false;
    }
    if (map.hd[0] === x) {
      return true;
    }
    _map = map.tl;
    continue ;
  };
}

function remove_assoc(x, param) {
  if (!param) {
    return /* [] */0;
  }
  var l = param.tl;
  var pair = param.hd;
  if (Caml_obj.equal(pair[0], x)) {
    return l;
  } else {
    return {
            hd: pair,
            tl: remove_assoc(x, l)
          };
  }
}

function remove_assq(x, param) {
  if (!param) {
    return /* [] */0;
  }
  var l = param.tl;
  var pair = param.hd;
  if (pair[0] === x) {
    return l;
  } else {
    return {
            hd: pair,
            tl: remove_assq(x, l)
          };
  }
}

function find(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param.hd;
      if (Curry._1(p, x)) {
        return x;
      }
      _param = param.tl;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_opt(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var x = param.hd;
    if (Curry._1(p, x)) {
      return Caml_option.some(x);
    }
    _param = param.tl;
    continue ;
  };
}

function find_all(p) {
  return function (param) {
    var _accu = /* [] */0;
    var _param = param;
    while(true) {
      var param$1 = _param;
      var accu = _accu;
      if (!param$1) {
        return rev_append(accu, /* [] */0);
      }
      var l = param$1.tl;
      var x = param$1.hd;
      if (Curry._1(p, x)) {
        _param = l;
        _accu = {
          hd: x,
          tl: accu
        };
        continue ;
      }
      _param = l;
      continue ;
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
    if (!param) {
      return [
              rev_append(yes, /* [] */0),
              rev_append(no, /* [] */0)
            ];
    }
    var l$1 = param.tl;
    var x = param.hd;
    if (Curry._1(p, x)) {
      _param = l$1;
      _yes = {
        hd: x,
        tl: yes
      };
      continue ;
    }
    _param = l$1;
    _no = {
      hd: x,
      tl: no
    };
    continue ;
  };
}

function split(param) {
  if (!param) {
    return [
            /* [] */0,
            /* [] */0
          ];
  }
  var match = param.hd;
  var match$1 = split(param.tl);
  return [
          {
            hd: match[0],
            tl: match$1[0]
          },
          {
            hd: match[1],
            tl: match$1[1]
          }
        ];
}

function combine(l1, l2) {
  if (l1) {
    if (l2) {
      return {
              hd: [
                l1.hd,
                l2.hd
              ],
              tl: combine(l1.tl, l2.tl)
            };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.combine",
          Error: new Error()
        };
  }
  if (!l2) {
    return /* [] */0;
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "List.combine",
        Error: new Error()
      };
}

function merge(cmp, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  var h2 = l2.hd;
  var h1 = l1.hd;
  if (Curry._2(cmp, h1, h2) <= 0) {
    return {
            hd: h1,
            tl: merge(cmp, l1.tl, l2)
          };
  } else {
    return {
            hd: h2,
            tl: merge(cmp, l1, l2.tl)
          };
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
      _l = l.tl;
      _k = k - 1 | 0;
      continue ;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "listLabels.res",
            420,
            11
          ],
          Error: new Error()
        };
  };
}

function stable_sort(cmp, l) {
  var sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l.tl;
        if (match) {
          var match$1 = match.tl;
          if (match$1) {
            var x3 = match$1.hd;
            var x2 = match.hd;
            var x1 = l.hd;
            if (Curry._2(cmp, x1, x2) <= 0) {
              if (Curry._2(cmp, x2, x3) <= 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: {
                            hd: x3,
                            tl: /* [] */0
                          }
                        }
                      };
              } else if (Curry._2(cmp, x1, x3) <= 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              }
            } else if (Curry._2(cmp, x1, x3) <= 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      }
                    };
            } else if (Curry._2(cmp, x2, x3) <= 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            } else {
              return {
                      hd: x3,
                      tl: {
                        hd: x2,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l.tl;
      if (match$2) {
        var x2$1 = match$2.hd;
        var x1$1 = l.hd;
        if (Curry._2(cmp, x1$1, x2$1) <= 0) {
          return {
                  hd: x1$1,
                  tl: {
                    hd: x2$1,
                    tl: /* [] */0
                  }
                };
        } else {
          return {
                  hd: x2$1,
                  tl: {
                    hd: x1$1,
                    tl: /* [] */0
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
      var h2 = l2$1.hd;
      var h1 = l1.hd;
      if (Curry._2(cmp, h1, h2) > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue ;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
      continue ;
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l.tl;
        if (match) {
          var match$1 = match.tl;
          if (match$1) {
            var x3 = match$1.hd;
            var x2 = match.hd;
            var x1 = l.hd;
            if (Curry._2(cmp, x1, x2) > 0) {
              if (Curry._2(cmp, x2, x3) > 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: {
                            hd: x3,
                            tl: /* [] */0
                          }
                        }
                      };
              } else if (Curry._2(cmp, x1, x3) > 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              }
            } else if (Curry._2(cmp, x1, x3) > 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      }
                    };
            } else if (Curry._2(cmp, x2, x3) > 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            } else {
              return {
                      hd: x3,
                      tl: {
                        hd: x2,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l.tl;
      if (match$2) {
        var x2$1 = match$2.hd;
        var x1$1 = l.hd;
        if (Curry._2(cmp, x1$1, x2$1) > 0) {
          return {
                  hd: x1$1,
                  tl: {
                    hd: x2$1,
                    tl: /* [] */0
                  }
                };
        } else {
          return {
                  hd: x2$1,
                  tl: {
                    hd: x1$1,
                    tl: /* [] */0
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
      var h2 = l2$1.hd;
      var h1 = l1.hd;
      if (Curry._2(cmp, h1, h2) <= 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue ;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
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
        var match = l.tl;
        if (match) {
          var match$1 = match.tl;
          if (match$1) {
            var x3 = match$1.hd;
            var x2 = match.hd;
            var x1 = l.hd;
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return {
                        hd: x2,
                        tl: /* [] */0
                      };
              } else if (c$1 < 0) {
                return {
                        hd: x2,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              }
            }
            if (c < 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              }
              if (c$2 < 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: {
                            hd: x3,
                            tl: /* [] */0
                          }
                        }
                      };
              }
              var c$3 = Curry._2(cmp, x1, x3);
              if (c$3 === 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              } else if (c$3 < 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              }
            }
            var c$4 = Curry._2(cmp, x1, x3);
            if (c$4 === 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: /* [] */0
                      }
                    };
            }
            if (c$4 < 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      }
                    };
            }
            var c$5 = Curry._2(cmp, x2, x3);
            if (c$5 === 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: /* [] */0
                      }
                    };
            } else if (c$5 < 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            } else {
              return {
                      hd: x3,
                      tl: {
                        hd: x2,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l.tl;
      if (match$2) {
        var x2$1 = match$2.hd;
        var x1$1 = l.hd;
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return {
                  hd: x1$1,
                  tl: /* [] */0
                };
        } else if (c$6 < 0) {
          return {
                  hd: x1$1,
                  tl: {
                    hd: x2$1,
                    tl: /* [] */0
                  }
                };
        } else {
          return {
                  hd: x2$1,
                  tl: {
                    hd: x1$1,
                    tl: /* [] */0
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
      var t2 = l2$1.tl;
      var h2 = l2$1.hd;
      var t1 = l1.tl;
      var h1 = l1.hd;
      var c$7 = Curry._2(cmp, h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue ;
      }
      if (c$7 > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue ;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = t2;
      continue ;
    };
  };
  var rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        var match = l.tl;
        if (match) {
          var match$1 = match.tl;
          if (match$1) {
            var x3 = match$1.hd;
            var x2 = match.hd;
            var x1 = l.hd;
            var c = Curry._2(cmp, x1, x2);
            if (c === 0) {
              var c$1 = Curry._2(cmp, x2, x3);
              if (c$1 === 0) {
                return {
                        hd: x2,
                        tl: /* [] */0
                      };
              } else if (c$1 > 0) {
                return {
                        hd: x2,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              }
            }
            if (c > 0) {
              var c$2 = Curry._2(cmp, x2, x3);
              if (c$2 === 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              }
              if (c$2 > 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: {
                            hd: x3,
                            tl: /* [] */0
                          }
                        }
                      };
              }
              var c$3 = Curry._2(cmp, x1, x3);
              if (c$3 === 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x2,
                          tl: /* [] */0
                        }
                      };
              } else if (c$3 > 0) {
                return {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              } else {
                return {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: {
                            hd: x2,
                            tl: /* [] */0
                          }
                        }
                      };
              }
            }
            var c$4 = Curry._2(cmp, x1, x3);
            if (c$4 === 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: /* [] */0
                      }
                    };
            }
            if (c$4 > 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: {
                          hd: x3,
                          tl: /* [] */0
                        }
                      }
                    };
            }
            var c$5 = Curry._2(cmp, x2, x3);
            if (c$5 === 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x1,
                        tl: /* [] */0
                      }
                    };
            } else if (c$5 > 0) {
              return {
                      hd: x2,
                      tl: {
                        hd: x3,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            } else {
              return {
                      hd: x3,
                      tl: {
                        hd: x2,
                        tl: {
                          hd: x1,
                          tl: /* [] */0
                        }
                      }
                    };
            }
          }
          
        }
        
      }
      
    } else if (l) {
      var match$2 = l.tl;
      if (match$2) {
        var x2$1 = match$2.hd;
        var x1$1 = l.hd;
        var c$6 = Curry._2(cmp, x1$1, x2$1);
        if (c$6 === 0) {
          return {
                  hd: x1$1,
                  tl: /* [] */0
                };
        } else if (c$6 > 0) {
          return {
                  hd: x1$1,
                  tl: {
                    hd: x2$1,
                    tl: /* [] */0
                  }
                };
        } else {
          return {
                  hd: x2$1,
                  tl: {
                    hd: x1$1,
                    tl: /* [] */0
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
      var t2 = l2$1.tl;
      var h2 = l2$1.hd;
      var t1 = l1.tl;
      var h1 = l1.hd;
      var c$7 = Curry._2(cmp, h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue ;
      }
      if (c$7 < 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue ;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
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
    _l2 = l2.tl;
    _l1 = l1.tl;
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
    _l = l.tl;
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
  hd ,
  compare_lengths ,
  compare_length_with ,
  cons ,
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
