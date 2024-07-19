// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let List = require("../../lib/js/list.js");
let Caml_obj = require("../../lib/js/caml_obj.js");
let Pervasives = require("../../lib/js/pervasives.js");

function length_aux(_len, _x) {
  while(true) {
    let x = _x;
    let len = _len;
    if (!x) {
      return len;
    }
    _x = x.tl;
    _len = len + 1 | 0;
    continue;
  };
}

function length(l) {
  return length_aux(0, l);
}

function hd(x) {
  if (x) {
    return x.hd;
  }
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: "hd"
        }
      });
}

function tl(x) {
  if (x) {
    return x.tl;
  }
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: "tl"
        }
      });
}

function nth(l, n) {
  if (n < 0) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.nth"
          }
        });
  }
  let _l = l;
  let _n = n;
  while(true) {
    let n$1 = _n;
    let l$1 = _l;
    if (l$1) {
      if (n$1 === 0) {
        return l$1.hd;
      }
      _n = n$1 - 1 | 0;
      _l = l$1.tl;
      continue;
    }
    throw new Error("Failure", {
          cause: {
            RE_EXN_ID: "Failure",
            _1: "nth"
          }
        });
  };
}

function rev_append(_l1, _l2) {
  while(true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      return l2;
    }
    _l2 = {
      hd: l1.hd,
      tl: l2
    };
    _l1 = l1.tl;
    continue;
  };
}

function rev(l) {
  return rev_append(l, /* [] */0);
}

function flatten(x) {
  if (x) {
    return Pervasives.$at(x.hd, flatten(x.tl));
  } else {
    return /* [] */0;
  }
}

function map(f, x) {
  if (!x) {
    return /* [] */0;
  }
  let r = f(x.hd);
  return {
    hd: r,
    tl: map(f, x.tl)
  };
}

function mapi(i, f, x) {
  if (!x) {
    return /* [] */0;
  }
  let r = f(i, x.hd);
  return {
    hd: r,
    tl: mapi(i + 1 | 0, f, x.tl)
  };
}

function mapi$1(f, l) {
  return mapi(0, f, l);
}

function rev_map(f, l) {
  let _accu = /* [] */0;
  let _x = l;
  while(true) {
    let x = _x;
    let accu = _accu;
    if (!x) {
      return accu;
    }
    _x = x.tl;
    _accu = {
      hd: f(x.hd),
      tl: accu
    };
    continue;
  };
}

function iter(f, _x) {
  while(true) {
    let x = _x;
    if (!x) {
      return;
    }
    f(x.hd);
    _x = x.tl;
    continue;
  };
}

function iteri(f, l) {
  let _i = 0;
  let _x = l;
  while(true) {
    let x = _x;
    let i = _i;
    if (!x) {
      return;
    }
    f(i, x.hd);
    _x = x.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function fold_left(f, _accu, _l) {
  while(true) {
    let l = _l;
    let accu = _accu;
    if (!l) {
      return accu;
    }
    _l = l.tl;
    _accu = f(accu, l.hd);
    continue;
  };
}

function fold_right(f, l, accu) {
  if (l) {
    return f(l.hd, fold_right(f, l.tl, accu));
  } else {
    return accu;
  }
}

function map2(f, l1, l2) {
  if (l1) {
    if (l2) {
      let r = f(l1.hd, l2.hd);
      return {
        hd: r,
        tl: map2(f, l1.tl, l2.tl)
      };
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.map2"
          }
        });
  }
  if (!l2) {
    return /* [] */0;
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.map2"
        }
      });
}

function rev_map2(f, l1, l2) {
  let _accu = /* [] */0;
  let _l1 = l1;
  let _l2 = l2;
  while(true) {
    let l2$1 = _l2;
    let l1$1 = _l1;
    let accu = _accu;
    if (l1$1) {
      if (l2$1) {
        _l2 = l2$1.tl;
        _l1 = l1$1.tl;
        _accu = {
          hd: f(l1$1.hd, l2$1.hd),
          tl: accu
        };
        continue;
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.rev_map2"
            }
          });
    }
    if (l2$1) {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.rev_map2"
            }
          });
    }
    return accu;
  };
}

function iter2(f, _l1, _l2) {
  while(true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        f(l1.hd, l2.hd);
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.iter2"
            }
          });
    }
    if (!l2) {
      return;
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.iter2"
          }
        });
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while(true) {
    let l2 = _l2;
    let l1 = _l1;
    let accu = _accu;
    if (l1) {
      if (l2) {
        _l2 = l2.tl;
        _l1 = l1.tl;
        _accu = f(accu, l1.hd, l2.hd);
        continue;
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.fold_left2"
            }
          });
    }
    if (l2) {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.fold_left2"
            }
          });
    }
    return accu;
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return f(l1.hd, l2.hd, fold_right2(f, l1.tl, l2.tl, accu));
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.fold_right2"
          }
        });
  }
  if (l2) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.fold_right2"
          }
        });
  }
  return accu;
}

function for_all(p, _x) {
  while(true) {
    let x = _x;
    if (!x) {
      return true;
    }
    if (!p(x.hd)) {
      return false;
    }
    _x = x.tl;
    continue;
  };
}

function exists(p, _x) {
  while(true) {
    let x = _x;
    if (!x) {
      return false;
    }
    if (p(x.hd)) {
      return true;
    }
    _x = x.tl;
    continue;
  };
}

function for_all2(p, _l1, _l2) {
  while(true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        if (!p(l1.hd, l2.hd)) {
          return false;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.for_all2"
            }
          });
    }
    if (!l2) {
      return true;
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.for_all2"
          }
        });
  };
}

function exists2(p, _l1, _l2) {
  while(true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1) {
      if (l2) {
        if (p(l1.hd, l2.hd)) {
          return true;
        }
        _l2 = l2.tl;
        _l1 = l1.tl;
        continue;
      }
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "List.exists2"
            }
          });
    }
    if (!l2) {
      return false;
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.exists2"
          }
        });
  };
}

function mem(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (!x_) {
      return false;
    }
    if (Caml_obj.equal(x_.hd, x)) {
      return true;
    }
    _x_ = x_.tl;
    continue;
  };
}

function memq(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (!x_) {
      return false;
    }
    if (x_.hd === x) {
      return true;
    }
    _x_ = x_.tl;
    continue;
  };
}

function assoc(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (x_) {
      let match = x_.hd;
      if (Caml_obj.equal(match[0], x)) {
        return match[1];
      }
      _x_ = x_.tl;
      continue;
    }
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  };
}

function assq(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (x_) {
      let match = x_.hd;
      if (match[0] === x) {
        return match[1];
      }
      _x_ = x_.tl;
      continue;
    }
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  };
}

function mem_assoc(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (!x_) {
      return false;
    }
    if (Caml_obj.equal(x_.hd[0], x)) {
      return true;
    }
    _x_ = x_.tl;
    continue;
  };
}

function mem_assq(x, _x_) {
  while(true) {
    let x_ = _x_;
    if (!x_) {
      return false;
    }
    if (x_.hd[0] === x) {
      return true;
    }
    _x_ = x_.tl;
    continue;
  };
}

function remove_assoc(x, x_) {
  if (!x_) {
    return /* [] */0;
  }
  let l = x_.tl;
  let pair = x_.hd;
  if (Caml_obj.equal(pair[0], x)) {
    return l;
  } else {
    return {
      hd: pair,
      tl: remove_assoc(x, l)
    };
  }
}

function remove_assq(x, x_) {
  if (!x_) {
    return /* [] */0;
  }
  let l = x_.tl;
  let pair = x_.hd;
  if (pair[0] === x) {
    return l;
  } else {
    return {
      hd: pair,
      tl: remove_assq(x, l)
    };
  }
}

function find(p, _x) {
  while(true) {
    let x = _x;
    if (x) {
      let x$1 = x.hd;
      if (p(x$1)) {
        return x$1;
      }
      _x = x.tl;
      continue;
    }
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  };
}

function find_all(p) {
  return function (__x) {
    let _accu = /* [] */0;
    let _x = __x;
    while(true) {
      let x = _x;
      let accu = _accu;
      if (!x) {
        return rev_append(accu, /* [] */0);
      }
      let l = x.tl;
      let x$1 = x.hd;
      if (p(x$1)) {
        _x = l;
        _accu = {
          hd: x$1,
          tl: accu
        };
        continue;
      }
      _x = l;
      continue;
    };
  };
}

function partition(p, l) {
  let _yes = /* [] */0;
  let _no = /* [] */0;
  let _x = l;
  while(true) {
    let x = _x;
    let no = _no;
    let yes = _yes;
    if (!x) {
      return [
        rev_append(yes, /* [] */0),
        rev_append(no, /* [] */0)
      ];
    }
    let l$1 = x.tl;
    let x$1 = x.hd;
    if (p(x$1)) {
      _x = l$1;
      _yes = {
        hd: x$1,
        tl: yes
      };
      continue;
    }
    _x = l$1;
    _no = {
      hd: x$1,
      tl: no
    };
    continue;
  };
}

function split(x) {
  if (!x) {
    return [
      /* [] */0,
      /* [] */0
    ];
  }
  let match = x.hd;
  let match$1 = split(x.tl);
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
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "List.combine"
          }
        });
  }
  if (!l2) {
    return /* [] */0;
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "List.combine"
        }
      });
}

function merge(cmp, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  let h2 = l2.hd;
  let h1 = l1.hd;
  if (cmp(h1, h2) <= 0) {
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
    let l = _l;
    let k = _k;
    if (k === 0) {
      return l;
    }
    if (l) {
      _l = l.tl;
      _k = k - 1 | 0;
      continue;
    }
    throw new Error("Assert_failure", {
          cause: {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "test_list.res",
              343,
              11
            ]
          }
        });
  };
}

function stable_sort(cmp, l) {
  let sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            if (cmp(x1, x2) <= 0) {
              if (cmp(x2, x3) <= 0) {
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
              } else if (cmp(x1, x3) <= 0) {
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
            } else if (cmp(x1, x3) <= 0) {
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
            } else if (cmp(x2, x3) <= 0) {
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
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        if (cmp(x1$1, x2$1) <= 0) {
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
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = rev_sort(n1, l);
    let s2 = rev_sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while(true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let h2 = l2$1.hd;
      let h1 = l1.hd;
      if (cmp(h1, h2) > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
      continue;
    };
  };
  let rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            if (cmp(x1, x2) > 0) {
              if (cmp(x2, x3) > 0) {
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
              } else if (cmp(x1, x3) > 0) {
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
            } else if (cmp(x1, x3) > 0) {
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
            } else if (cmp(x2, x3) > 0) {
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
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        if (cmp(x1$1, x2$1) > 0) {
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
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = sort(n1, l);
    let s2 = sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while(true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let h2 = l2$1.hd;
      let h1 = l1.hd;
      if (cmp(h1, h2) <= 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = l1.tl;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = l2$1.tl;
      continue;
    };
  };
  let len = length_aux(0, l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

function sort_uniq(cmp, l) {
  let sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            let c = cmp(x1, x2);
            if (c === 0) {
              let c$1 = cmp(x2, x3);
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
              let c$2 = cmp(x2, x3);
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
              let c$3 = cmp(x1, x3);
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
            let c$4 = cmp(x1, x3);
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
            let c$5 = cmp(x2, x3);
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
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        let c$6 = cmp(x1$1, x2$1);
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
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = rev_sort(n1, l);
    let s2 = rev_sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while(true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let t2 = l2$1.tl;
      let h2 = l2$1.hd;
      let t1 = l1.tl;
      let h1 = l1.hd;
      let c$7 = cmp(h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue;
      }
      if (c$7 > 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = t2;
      continue;
    };
  };
  let rev_sort = function (n, l) {
    if (n !== 2) {
      if (n === 3 && l) {
        let match = l.tl;
        if (match) {
          let match$1 = match.tl;
          if (match$1) {
            let x3 = match$1.hd;
            let x2 = match.hd;
            let x1 = l.hd;
            let c = cmp(x1, x2);
            if (c === 0) {
              let c$1 = cmp(x2, x3);
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
              let c$2 = cmp(x2, x3);
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
              let c$3 = cmp(x1, x3);
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
            let c$4 = cmp(x1, x3);
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
            let c$5 = cmp(x2, x3);
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
      let match$2 = l.tl;
      if (match$2) {
        let x2$1 = match$2.hd;
        let x1$1 = l.hd;
        let c$6 = cmp(x1$1, x2$1);
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
    let n1 = (n >> 1);
    let n2 = n - n1 | 0;
    let l2 = chop(n1, l);
    let s1 = sort(n1, l);
    let s2 = sort(n2, l2);
    let _l1 = s1;
    let _l2 = s2;
    let _accu = /* [] */0;
    while(true) {
      let accu = _accu;
      let l2$1 = _l2;
      let l1 = _l1;
      if (!l1) {
        return rev_append(l2$1, accu);
      }
      if (!l2$1) {
        return rev_append(l1, accu);
      }
      let t2 = l2$1.tl;
      let h2 = l2$1.hd;
      let t1 = l1.tl;
      let h1 = l1.hd;
      let c$7 = cmp(h1, h2);
      if (c$7 === 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l2 = t2;
        _l1 = t1;
        continue;
      }
      if (c$7 < 0) {
        _accu = {
          hd: h1,
          tl: accu
        };
        _l1 = t1;
        continue;
      }
      _accu = {
        hd: h2,
        tl: accu
      };
      _l2 = t2;
      continue;
    };
  };
  let len = length_aux(0, l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

let u = List.length;

let append = Pervasives.$at;

let concat = flatten;

let filter = find_all;

let sort = stable_sort;

let fast_sort = stable_sort;

exports.u = u;
exports.length_aux = length_aux;
exports.length = length;
exports.hd = hd;
exports.tl = tl;
exports.nth = nth;
exports.append = append;
exports.rev_append = rev_append;
exports.rev = rev;
exports.flatten = flatten;
exports.concat = concat;
exports.map = map;
exports.mapi = mapi$1;
exports.rev_map = rev_map;
exports.iter = iter;
exports.iteri = iteri;
exports.fold_left = fold_left;
exports.fold_right = fold_right;
exports.map2 = map2;
exports.rev_map2 = rev_map2;
exports.iter2 = iter2;
exports.fold_left2 = fold_left2;
exports.fold_right2 = fold_right2;
exports.for_all = for_all;
exports.exists = exists;
exports.for_all2 = for_all2;
exports.exists2 = exists2;
exports.mem = mem;
exports.memq = memq;
exports.assoc = assoc;
exports.assq = assq;
exports.mem_assoc = mem_assoc;
exports.mem_assq = mem_assq;
exports.remove_assoc = remove_assoc;
exports.remove_assq = remove_assq;
exports.find = find;
exports.find_all = find_all;
exports.filter = filter;
exports.partition = partition;
exports.split = split;
exports.combine = combine;
exports.merge = merge;
exports.chop = chop;
exports.stable_sort = stable_sort;
exports.sort = sort;
exports.fast_sort = fast_sort;
exports.sort_uniq = sort_uniq;
/* No side effect */
