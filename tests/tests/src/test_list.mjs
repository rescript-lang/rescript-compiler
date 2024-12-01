// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";

function length_aux(_len, _x) {
  while (true) {
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
  } else {
    return Pervasives.failwith("hd");
  }
}

function tl(x) {
  if (x) {
    return x.tl;
  } else {
    return Pervasives.failwith("tl");
  }
}

function nth(l, n) {
  if (n < 0) {
    return Pervasives.invalid_arg("List.nth");
  }
  let _l = l;
  let _n = n;
  while (true) {
    let n$1 = _n;
    let l$1 = _l;
    if (!l$1) {
      return Pervasives.failwith("nth");
    }
    if (n$1 === 0) {
      return l$1.hd;
    }
    _n = n$1 - 1 | 0;
    _l = l$1.tl;
    continue;
  };
}

function rev_append(_l1, _l2) {
  while (true) {
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
  while (true) {
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
  while (true) {
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
  while (true) {
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
  while (true) {
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
  if (!l1) {
    if (l2) {
      return Pervasives.invalid_arg("List.map2");
    } else {
      return /* [] */0;
    }
  }
  if (!l2) {
    return Pervasives.invalid_arg("List.map2");
  }
  let r = f(l1.hd, l2.hd);
  return {
    hd: r,
    tl: map2(f, l1.tl, l2.tl)
  };
}

function rev_map2(f, l1, l2) {
  let _accu = /* [] */0;
  let _l1 = l1;
  let _l2 = l2;
  while (true) {
    let l2$1 = _l2;
    let l1$1 = _l1;
    let accu = _accu;
    if (!l1$1) {
      if (l2$1) {
        return Pervasives.invalid_arg("List.rev_map2");
      } else {
        return accu;
      }
    }
    if (!l2$1) {
      return Pervasives.invalid_arg("List.rev_map2");
    }
    _l2 = l2$1.tl;
    _l1 = l1$1.tl;
    _accu = {
      hd: f(l1$1.hd, l2$1.hd),
      tl: accu
    };
    continue;
  };
}

function iter2(f, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      if (l2) {
        return Pervasives.invalid_arg("List.iter2");
      } else {
        return;
      }
    }
    if (!l2) {
      return Pervasives.invalid_arg("List.iter2");
    }
    f(l1.hd, l2.hd);
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function fold_left2(f, _accu, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    let accu = _accu;
    if (!l1) {
      if (l2) {
        return Pervasives.invalid_arg("List.fold_left2");
      } else {
        return accu;
      }
    }
    if (!l2) {
      return Pervasives.invalid_arg("List.fold_left2");
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    _accu = f(accu, l1.hd, l2.hd);
    continue;
  };
}

function fold_right2(f, l1, l2, accu) {
  if (l1) {
    if (l2) {
      return f(l1.hd, l2.hd, fold_right2(f, l1.tl, l2.tl, accu));
    } else {
      return Pervasives.invalid_arg("List.fold_right2");
    }
  } else if (l2) {
    return Pervasives.invalid_arg("List.fold_right2");
  } else {
    return accu;
  }
}

function for_all(p, _x) {
  while (true) {
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
  while (true) {
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
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      if (l2) {
        return Pervasives.invalid_arg("List.for_all2");
      } else {
        return true;
      }
    }
    if (!l2) {
      return Pervasives.invalid_arg("List.for_all2");
    }
    if (!p(l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function exists2(p, _l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (!l1) {
      if (l2) {
        return Pervasives.invalid_arg("List.exists2");
      } else {
        return false;
      }
    }
    if (!l2) {
      return Pervasives.invalid_arg("List.exists2");
    }
    if (p(l1.hd, l2.hd)) {
      return true;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function mem(x, _x_) {
  while (true) {
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

function memq(x, _x_) {
  while (true) {
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
  while (true) {
    let x_ = _x_;
    if (x_) {
      let match = x_.hd;
      if (match[0] === x) {
        return match[1];
      }
      _x_ = x_.tl;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function assq(x, _x_) {
  while (true) {
    let x_ = _x_;
    if (x_) {
      let match = x_.hd;
      if (match[0] === x) {
        return match[1];
      }
      _x_ = x_.tl;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function mem_assoc(x, _x_) {
  while (true) {
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

function mem_assq(x, _x_) {
  while (true) {
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
  if (pair[0] === x) {
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
  while (true) {
    let x = _x;
    if (x) {
      let x$1 = x.hd;
      if (p(x$1)) {
        return x$1;
      }
      _x = x.tl;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function find_all(p) {
  return __x => {
    let _accu = /* [] */0;
    let _x = __x;
    while (true) {
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
  while (true) {
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
    } else {
      return Pervasives.invalid_arg("List.combine");
    }
  } else if (l2) {
    return Pervasives.invalid_arg("List.combine");
  } else {
    return /* [] */0;
  }
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
  while (true) {
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
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "test_list.res",
        343,
        11
      ],
      Error: new Error()
    };
  };
}

function stable_sort(cmp, l) {
  let sort = (n, l) => {
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
    while (true) {
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
  let rev_sort = (n, l) => {
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
    while (true) {
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
  let sort = (n, l) => {
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
    while (true) {
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
  let rev_sort = (n, l) => {
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
    while (true) {
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

let u = Belt_List.length;

let append = Pervasives.$at;

let concat = flatten;

let filter = find_all;

let sort = stable_sort;

let fast_sort = stable_sort;

export {
  u,
  length_aux,
  length,
  hd,
  tl,
  nth,
  append,
  rev_append,
  rev,
  flatten,
  concat,
  map,
  mapi$1 as mapi,
  rev_map,
  iter,
  iteri,
  fold_left,
  fold_right,
  map2,
  rev_map2,
  iter2,
  fold_left2,
  fold_right2,
  for_all,
  exists,
  for_all2,
  exists2,
  mem,
  memq,
  assoc,
  assq,
  mem_assoc,
  mem_assq,
  remove_assoc,
  remove_assq,
  find,
  find_all,
  filter,
  partition,
  split,
  combine,
  merge,
  chop,
  stable_sort,
  sort,
  fast_sort,
  sort_uniq,
}
/* No side effect */
