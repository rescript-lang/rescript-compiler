'use strict';

let Caml_obj = require("./caml_obj.js");
let Caml_array = require("./caml_array.js");
let Caml_exceptions = require("./caml_exceptions.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

let make_float = Caml_array.make_float;

let Floatarray = {};

function init(l, f) {
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.init"
    });
  }
  let res = Caml_array.make(l, f(0));
  for (let i = 1; i < l; ++i) {
    res[i] = f(i);
  }
  return res;
}

function make_matrix(sx, sy, init) {
  let res = Caml_array.make(sx, []);
  for (let x = 0; x < sx; ++x) {
    res[x] = Caml_array.make(sy, init);
  }
  return res;
}

function copy(a) {
  let l = a.length;
  if (l === 0) {
    return [];
  } else {
    return Caml_array.sub(a, 0, l);
  }
}

function append(a1, a2) {
  let l1 = a1.length;
  if (l1 === 0) {
    return copy(a2);
  } else if (a2.length === 0) {
    return Caml_array.sub(a1, 0, l1);
  } else {
    return a1.concat(a2);
  }
}

function sub(a, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (a.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.sub"
    });
  }
  return Caml_array.sub(a, ofs, len);
}

function fill(a, ofs, len, v) {
  if (ofs < 0 || len < 0 || ofs > (a.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.fill"
    });
  }
  for (let i = ofs, i_finish = ofs + len | 0; i < i_finish; ++i) {
    a[i] = v;
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (a1.length - len | 0) || ofs2 < 0 || ofs2 > (a2.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.blit"
    });
  }
  Caml_array.blit(a1, ofs1, a2, ofs2, len);
}

function iter(f, a) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(a[i]);
  }
}

function iter2(f, a, b) {
  if (a.length !== b.length) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.iter2: arrays must have the same length"
    });
  }
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(a[i], b[i]);
  }
}

function map(f, a) {
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = Caml_array.make(l, f(a[0]));
  for (let i = 1; i < l; ++i) {
    r[i] = f(a[i]);
  }
  return r;
}

function map2(f, a, b) {
  let la = a.length;
  let lb = b.length;
  if (la !== lb) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Array.map2: arrays must have the same length"
    });
  }
  if (la === 0) {
    return [];
  }
  let r = Caml_array.make(la, f(a[0], b[0]));
  for (let i = 1; i < la; ++i) {
    r[i] = f(a[i], b[i]);
  }
  return r;
}

function iteri(f, a) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(i, a[i]);
  }
}

function mapi(f, a) {
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = Caml_array.make(l, f(0, a[0]));
  for (let i = 1; i < l; ++i) {
    r[i] = f(i, a[i]);
  }
  return r;
}

function to_list(a) {
  let _i = a.length - 1 | 0;
  let _res = /* [] */0;
  while (true) {
    let res = _res;
    let i = _i;
    if (i < 0) {
      return res;
    }
    _res = {
      hd: a[i],
      tl: res
    };
    _i = i - 1 | 0;
    continue;
  };
}

function list_length(_accu, _param) {
  while (true) {
    let param = _param;
    let accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param.tl;
    _accu = accu + 1 | 0;
    continue;
  };
}

function of_list(param) {
  if (!param) {
    return [];
  }
  let a = Caml_array.make(list_length(0, param), param.hd);
  let _i = 1;
  let _param = param.tl;
  while (true) {
    let param$1 = _param;
    let i = _i;
    if (!param$1) {
      return a;
    }
    a[i] = param$1.hd;
    _param = param$1.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function fold_left(f, x, a) {
  let r = x;
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    r = f(r, a[i]);
  }
  return r;
}

function fold_right(f, a, x) {
  let r = x;
  for (let i = a.length - 1 | 0; i >= 0; --i) {
    r = f(a[i], r);
  }
  return r;
}

function exists(p, a) {
  let n = a.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === n) {
      return false;
    }
    if (p(a[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function for_all(p, a) {
  let n = a.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === n) {
      return true;
    }
    if (!p(a[i])) {
      return false;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function mem(x, a) {
  let n = a.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === n) {
      return false;
    }
    if (Caml_obj.equal(a[i], x)) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function memq(x, a) {
  let n = a.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === n) {
      return false;
    }
    if (x === a[i]) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

let Bottom = /* @__PURE__ */Caml_exceptions.create("ArrayLabels.Bottom");

function sort(cmp, a) {
  let maxson = (l, i) => {
    let i31 = ((i + i | 0) + i | 0) + 1 | 0;
    let x = i31;
    if ((i31 + 2 | 0) < l) {
      if (cmp(Caml_array.get(a, i31), Caml_array.get(a, i31 + 1 | 0)) < 0) {
        x = i31 + 1 | 0;
      }
      if (cmp(Caml_array.get(a, x), Caml_array.get(a, i31 + 2 | 0)) < 0) {
        x = i31 + 2 | 0;
      }
      return x;
    }
    if ((i31 + 1 | 0) < l && cmp(Caml_array.get(a, i31), Caml_array.get(a, i31 + 1 | 0)) < 0) {
      return i31 + 1 | 0;
    }
    if (i31 < l) {
      return i31;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: Bottom,
      _1: i
    });
  };
  let trickle = (l, i, e) => {
    try {
      let _i = i;
      while (true) {
        let i$1 = _i;
        let j = maxson(l, i$1);
        if (cmp(Caml_array.get(a, j), e) <= 0) {
          return Caml_array.set(a, i$1, e);
        }
        Caml_array.set(a, i$1, Caml_array.get(a, j));
        _i = j;
        continue;
      };
    } catch (raw_i) {
      let i$2 = Caml_js_exceptions.internalAnyToExn(raw_i);
      if (i$2.RE_EXN_ID === Bottom) {
        return Caml_array.set(a, i$2._1, e);
      }
      throw i$2;
    }
  };
  let bubble = (l, i) => {
    try {
      let _i = i;
      while (true) {
        let i$1 = _i;
        let j = maxson(l, i$1);
        Caml_array.set(a, i$1, Caml_array.get(a, j));
        _i = j;
        continue;
      };
    } catch (raw_i) {
      let i$2 = Caml_js_exceptions.internalAnyToExn(raw_i);
      if (i$2.RE_EXN_ID === Bottom) {
        return i$2._1;
      }
      throw i$2;
    }
  };
  let trickleup = (_i, e) => {
    while (true) {
      let i = _i;
      let father = (i - 1 | 0) / 3 | 0;
      if (i === father) {
        throw Caml_js_exceptions.internalFromExtension({
          RE_EXN_ID: "Assert_failure",
          _1: [
            "arrayLabels.res",
            321,
            4
          ]
        });
      }
      if (cmp(Caml_array.get(a, father), e) >= 0) {
        return Caml_array.set(a, i, e);
      }
      Caml_array.set(a, i, Caml_array.get(a, father));
      if (father <= 0) {
        return Caml_array.set(a, 0, e);
      }
      _i = father;
      continue;
    };
  };
  let l = a.length;
  for (let i = ((l + 1 | 0) / 3 | 0) - 1 | 0; i >= 0; --i) {
    trickle(l, i, Caml_array.get(a, i));
  }
  for (let i$1 = l - 1 | 0; i$1 >= 2; --i$1) {
    let e = Caml_array.get(a, i$1);
    Caml_array.set(a, i$1, Caml_array.get(a, 0));
    trickleup(bubble(i$1, 0), e);
  }
  if (l <= 1) {
    return;
  }
  let e$1 = Caml_array.get(a, 1);
  Caml_array.set(a, 1, Caml_array.get(a, 0));
  Caml_array.set(a, 0, e$1);
}

function stable_sort(cmp, a) {
  let merge = (src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
    let src1r = src1ofs + src1len | 0;
    let src2r = src2ofs + src2len | 0;
    let _i1 = src1ofs;
    let _s1 = Caml_array.get(a, src1ofs);
    let _i2 = src2ofs;
    let _s2 = Caml_array.get(src2, src2ofs);
    let _d = dstofs;
    while (true) {
      let d = _d;
      let s2 = _s2;
      let i2 = _i2;
      let s1 = _s1;
      let i1 = _i1;
      if (cmp(s1, s2) <= 0) {
        Caml_array.set(dst, d, s1);
        let i1$1 = i1 + 1 | 0;
        if (i1$1 >= src1r) {
          return blit(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
        }
        _d = d + 1 | 0;
        _s1 = Caml_array.get(a, i1$1);
        _i1 = i1$1;
        continue;
      }
      Caml_array.set(dst, d, s2);
      let i2$1 = i2 + 1 | 0;
      if (i2$1 >= src2r) {
        return blit(a, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
      _d = d + 1 | 0;
      _s2 = Caml_array.get(src2, i2$1);
      _i2 = i2$1;
      continue;
    };
  };
  let isortto = (srcofs, dst, dstofs, len) => {
    for (let i = 0; i < len; ++i) {
      let e = Caml_array.get(a, srcofs + i | 0);
      let j = (dstofs + i | 0) - 1 | 0;
      while (j >= dstofs && cmp(Caml_array.get(dst, j), e) > 0) {
        Caml_array.set(dst, j + 1 | 0, Caml_array.get(dst, j));
        j = j - 1 | 0;
      };
      Caml_array.set(dst, j + 1 | 0, e);
    }
  };
  let sortto = (srcofs, dst, dstofs, len) => {
    if (len <= 5) {
      return isortto(srcofs, dst, dstofs, len);
    }
    let l1 = len / 2 | 0;
    let l2 = len - l1 | 0;
    sortto(srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
    sortto(srcofs, a, srcofs + l2 | 0, l1);
    merge(srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
  };
  let l = a.length;
  if (l <= 5) {
    return isortto(0, a, 0, l);
  }
  let l1 = l / 2 | 0;
  let l2 = l - l1 | 0;
  let t = Caml_array.make(l2, Caml_array.get(a, 0));
  sortto(l1, t, 0, l2);
  sortto(0, a, l2, l1);
  merge(l2, l1, t, 0, l2, a, 0);
}

let create_matrix = make_matrix;

let concat = Caml_array.concat;

let fast_sort = stable_sort;

exports.init = init;
exports.make_matrix = make_matrix;
exports.create_matrix = create_matrix;
exports.append = append;
exports.concat = concat;
exports.sub = sub;
exports.copy = copy;
exports.fill = fill;
exports.blit = blit;
exports.to_list = to_list;
exports.of_list = of_list;
exports.iter = iter;
exports.map = map;
exports.iteri = iteri;
exports.mapi = mapi;
exports.fold_left = fold_left;
exports.fold_right = fold_right;
exports.iter2 = iter2;
exports.map2 = map2;
exports.exists = exists;
exports.for_all = for_all;
exports.mem = mem;
exports.memq = memq;
exports.make_float = make_float;
exports.sort = sort;
exports.stable_sort = stable_sort;
exports.fast_sort = fast_sort;
exports.Floatarray = Floatarray;
/* No side effect */
