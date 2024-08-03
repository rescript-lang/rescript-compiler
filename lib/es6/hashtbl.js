

import * as Caml from "./caml.js";
import * as $$Array from "./array.js";
import * as Random from "./random.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_hash from "./caml_hash.js";
import * as Caml_array from "./caml_array.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_option from "./caml_option.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";

function hash(x) {
  return Caml_hash.hash(10, 100, 0, x);
}

function hash_param(n1, n2, x) {
  return Caml_hash.hash(n1, n2, 0, x);
}

function seeded_hash(seed, x) {
  return Caml_hash.hash(10, 100, seed, x);
}

function flip_ongoing_traversal(h) {
  h.initial_size = -h.initial_size | 0;
}

let randomized = {
  contents: false
};

function randomize() {
  randomized.contents = true;
}

function is_randomized() {
  return randomized.contents;
}

let prng = CamlinternalLazy.from_fun(function () {
  return Random.State.make_self_init();
});

function power_2_above(_x, n) {
  while (true) {
    let x = _x;
    if (x >= n) {
      return x;
    }
    if ((x << 1) < x) {
      return x;
    }
    _x = (x << 1);
    continue;
  };
}

function create(randomOpt, initial_size) {
  let random = randomOpt !== undefined ? randomOpt : randomized.contents;
  let s = power_2_above(16, initial_size);
  let seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
  return {
    size: 0,
    data: Caml_array.make(s, "Empty"),
    seed: seed,
    initial_size: s
  };
}

function clear(h) {
  h.size = 0;
  let len = h.data.length;
  for (let i = 0; i < len; ++i) {
    Caml_array.set(h.data, i, "Empty");
  }
}

function reset(h) {
  let len = h.data.length;
  if (len === Pervasives.abs(h.initial_size)) {
    return clear(h);
  } else {
    h.size = 0;
    h.data = Caml_array.make(Pervasives.abs(h.initial_size), "Empty");
    return;
  }
}

function copy_bucketlist(param) {
  if (typeof param !== "object") {
    return "Empty";
  }
  let key = param.key;
  let data = param.data;
  let next = param.next;
  let loop = function (_prec, _param) {
    while (true) {
      let param = _param;
      let prec = _prec;
      if (typeof param !== "object") {
        return;
      }
      let key = param.key;
      let data = param.data;
      let next = param.next;
      let r = {
        TAG: "Cons",
        key: key,
        data: data,
        next: next
      };
      if (typeof prec !== "object") {
        throw new Error("Assert_failure", {
              cause: {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "hashtbl.res",
                  110,
                  19
                ]
              }
            });
      }
      prec.next = r;
      _param = next;
      _prec = r;
      continue;
    };
  };
  let r = {
    TAG: "Cons",
    key: key,
    data: data,
    next: next
  };
  loop(r, next);
  return r;
}

function copy(h) {
  return {
    size: h.size,
    data: $$Array.map(copy_bucketlist, h.data),
    seed: h.seed,
    initial_size: h.initial_size
  };
}

function length(h) {
  return h.size;
}

function resize(indexfun, h) {
  let odata = h.data;
  let osize = odata.length;
  let nsize = (osize << 1);
  if (nsize < osize) {
    return;
  }
  let ndata = Caml_array.make(nsize, "Empty");
  let ndata_tail = Caml_array.make(nsize, "Empty");
  let inplace = h.initial_size >= 0;
  h.data = ndata;
  let insert_bucket = function (_param) {
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let key = param.key;
      let data = param.data;
      let next = param.next;
      let cell = inplace ? param : ({
          TAG: "Cons",
          key: key,
          data: data,
          next: "Empty"
        });
      let nidx = indexfun(h, key);
      let tail = Caml_array.get(ndata_tail, nidx);
      if (typeof tail !== "object") {
        Caml_array.set(ndata, nidx, cell);
      } else {
        tail.next = cell;
      }
      Caml_array.set(ndata_tail, nidx, cell);
      _param = next;
      continue;
    };
  };
  for (let i = 0; i < osize; ++i) {
    insert_bucket(Caml_array.get(odata, i));
  }
  if (!inplace) {
    return;
  }
  for (let i$1 = 0; i$1 < nsize; ++i$1) {
    let tail = Caml_array.get(ndata_tail, i$1);
    if (typeof tail === "object") {
      tail.next = "Empty";
    }
    
  }
}

function key_index(h, key) {
  return Caml_hash.hash(10, 100, h.seed, key) & (h.data.length - 1 | 0);
}

function add(h, key, data) {
  let i = key_index(h, key);
  let bucket = {
    TAG: "Cons",
    key: key,
    data: data,
    next: Caml_array.get(h.data, i)
  };
  Caml_array.set(h.data, i, bucket);
  h.size = h.size + 1 | 0;
  if (h.size > (h.data.length << 1)) {
    return resize(key_index, h);
  }
  
}

function remove(h, key) {
  let i = key_index(h, key);
  let _prec = "Empty";
  let _param = Caml_array.get(h.data, i);
  while (true) {
    let param = _param;
    let prec = _prec;
    if (typeof param !== "object") {
      return;
    }
    let k = param.key;
    let next = param.next;
    if (Caml_obj.equal(k, key)) {
      h.size = h.size - 1 | 0;
      if (typeof prec !== "object") {
        return Caml_array.set(h.data, i, next);
      } else {
        prec.next = next;
        return;
      }
    }
    _param = next;
    _prec = param;
    continue;
  };
}

function find(h, key) {
  let match = Caml_array.get(h.data, key_index(h, key));
  if (typeof match !== "object") {
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  }
  let k1 = match.key;
  let d1 = match.data;
  let next1 = match.next;
  if (Caml_obj.equal(key, k1)) {
    return d1;
  }
  if (typeof next1 !== "object") {
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  }
  let k2 = next1.key;
  let d2 = next1.data;
  let next2 = next1.next;
  if (Caml_obj.equal(key, k2)) {
    return d2;
  }
  if (typeof next2 !== "object") {
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  }
  let k3 = next2.key;
  let d3 = next2.data;
  let next3 = next2.next;
  if (Caml_obj.equal(key, k3)) {
    return d3;
  } else {
    let _param = next3;
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
      }
      let k = param.key;
      let data = param.data;
      let next = param.next;
      if (Caml_obj.equal(key, k)) {
        return data;
      }
      _param = next;
      continue;
    };
  }
}

function find_opt(h, key) {
  let match = Caml_array.get(h.data, key_index(h, key));
  if (typeof match !== "object") {
    return;
  }
  let k1 = match.key;
  let d1 = match.data;
  let next1 = match.next;
  if (Caml_obj.equal(key, k1)) {
    return Caml_option.some(d1);
  }
  if (typeof next1 !== "object") {
    return;
  }
  let k2 = next1.key;
  let d2 = next1.data;
  let next2 = next1.next;
  if (Caml_obj.equal(key, k2)) {
    return Caml_option.some(d2);
  }
  if (typeof next2 !== "object") {
    return;
  }
  let k3 = next2.key;
  let d3 = next2.data;
  let next3 = next2.next;
  if (Caml_obj.equal(key, k3)) {
    return Caml_option.some(d3);
  } else {
    let _param = next3;
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let k = param.key;
      let data = param.data;
      let next = param.next;
      if (Caml_obj.equal(key, k)) {
        return Caml_option.some(data);
      }
      _param = next;
      continue;
    };
  }
}

function find_all(h, key) {
  let find_in_bucket = function (_param) {
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return /* [] */0;
      }
      let k = param.key;
      let data = param.data;
      let next = param.next;
      if (Caml_obj.equal(k, key)) {
        return {
          hd: data,
          tl: find_in_bucket(next)
        };
      }
      _param = next;
      continue;
    };
  };
  return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
}

function replace_bucket(key, data, _param) {
  while (true) {
    let param = _param;
    if (typeof param !== "object") {
      return true;
    }
    let k = param.key;
    let next = param.next;
    if (Caml_obj.equal(k, key)) {
      param.key = key;
      param.data = data;
      return false;
    }
    _param = next;
    continue;
  };
}

function replace(h, key, data) {
  let i = key_index(h, key);
  let l = Caml_array.get(h.data, i);
  if (replace_bucket(key, data, l)) {
    Caml_array.set(h.data, i, {
      TAG: "Cons",
      key: key,
      data: data,
      next: l
    });
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    } else {
      return;
    }
  }
  
}

function mem(h, key) {
  let _param = Caml_array.get(h.data, key_index(h, key));
  while (true) {
    let param = _param;
    if (typeof param !== "object") {
      return false;
    }
    let k = param.key;
    let next = param.next;
    if (Caml_obj.equal(k, key)) {
      return true;
    }
    _param = next;
    continue;
  };
}

function iter(f, h) {
  let do_bucket = function (_param) {
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return;
      }
      let key = param.key;
      let data = param.data;
      let next = param.next;
      f(key, data);
      _param = next;
      continue;
    };
  };
  let old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    let d = h.data;
    for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
      do_bucket(Caml_array.get(d, i));
    }
    if (!old_trav) {
      return flip_ongoing_traversal(h);
    } else {
      return;
    }
  } catch (exn) {
    if (old_trav) {
      throw new Error(exn.RE_EXN_ID, {
            cause: exn
          });
    }
    flip_ongoing_traversal(h);
    throw new Error(exn.RE_EXN_ID, {
          cause: exn
        });
  }
}

function filter_map_inplace_bucket(f, h, i, _prec, _param) {
  while (true) {
    let param = _param;
    let prec = _prec;
    if (typeof param !== "object") {
      if (typeof prec !== "object") {
        return Caml_array.set(h.data, i, "Empty");
      } else {
        prec.next = "Empty";
        return;
      }
    }
    let key = param.key;
    let data = param.data;
    let next = param.next;
    let data$1 = f(key, data);
    if (data$1 !== undefined) {
      if (typeof prec !== "object") {
        Caml_array.set(h.data, i, param);
      } else {
        prec.next = param;
      }
      param.data = Caml_option.valFromOption(data$1);
      _param = next;
      _prec = param;
      continue;
    }
    h.size = h.size - 1 | 0;
    _param = next;
    continue;
  };
}

function filter_map_inplace(f, h) {
  let d = h.data;
  let old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
      filter_map_inplace_bucket(f, h, i, "Empty", Caml_array.get(h.data, i));
    }
    return;
  } catch (exn) {
    if (old_trav) {
      throw new Error(exn.RE_EXN_ID, {
            cause: exn
          });
    }
    flip_ongoing_traversal(h);
    throw new Error(exn.RE_EXN_ID, {
          cause: exn
        });
  }
}

function fold(f, h, init) {
  let do_bucket = function (_b, _accu) {
    while (true) {
      let accu = _accu;
      let b = _b;
      if (typeof b !== "object") {
        return accu;
      }
      let key = b.key;
      let data = b.data;
      let next = b.next;
      _accu = f(key, data, accu);
      _b = next;
      continue;
    };
  };
  let old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    let d = h.data;
    let accu = init;
    for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
      accu = do_bucket(Caml_array.get(d, i), accu);
    }
    if (!old_trav) {
      flip_ongoing_traversal(h);
    }
    return accu;
  } catch (exn) {
    if (old_trav) {
      throw new Error(exn.RE_EXN_ID, {
            cause: exn
          });
    }
    flip_ongoing_traversal(h);
    throw new Error(exn.RE_EXN_ID, {
          cause: exn
        });
  }
}

function bucket_length(_accu, _param) {
  while (true) {
    let param = _param;
    let accu = _accu;
    if (typeof param !== "object") {
      return accu;
    }
    let next = param.next;
    _param = next;
    _accu = accu + 1 | 0;
    continue;
  };
}

function stats(h) {
  let mbl = $$Array.fold_left((function (m, b) {
    return Caml.int_max(m, bucket_length(0, b));
  }), 0, h.data);
  let histo = Caml_array.make(mbl + 1 | 0, 0);
  $$Array.iter((function (b) {
    let l = bucket_length(0, b);
    Caml_array.set(histo, l, Caml_array.get(histo, l) + 1 | 0);
  }), h.data);
  return {
    num_bindings: h.size,
    num_buckets: h.data.length,
    max_bucket_length: mbl,
    bucket_histogram: histo
  };
}

function MakeSeeded(H) {
  let key_index = function (h, key) {
    return H.hash(h.seed, key) & (h.data.length - 1 | 0);
  };
  let add = function (h, key, data) {
    let i = key_index(h, key);
    let bucket = {
      TAG: "Cons",
      key: key,
      data: data,
      next: Caml_array.get(h.data, i)
    };
    Caml_array.set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  let remove = function (h, key) {
    let i = key_index(h, key);
    let _prec = "Empty";
    let _param = Caml_array.get(h.data, i);
    while (true) {
      let param = _param;
      let prec = _prec;
      if (typeof param !== "object") {
        return;
      }
      let k = param.key;
      let next = param.next;
      if (H.equal(k, key)) {
        h.size = h.size - 1 | 0;
        if (typeof prec !== "object") {
          return Caml_array.set(h.data, i, next);
        } else {
          prec.next = next;
          return;
        }
      }
      _param = next;
      _prec = param;
      continue;
    };
  };
  let find = function (h, key) {
    let match = Caml_array.get(h.data, key_index(h, key));
    if (typeof match !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k1 = match.key;
    let d1 = match.data;
    let next1 = match.next;
    if (H.equal(key, k1)) {
      return d1;
    }
    if (typeof next1 !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k2 = next1.key;
    let d2 = next1.data;
    let next2 = next1.next;
    if (H.equal(key, k2)) {
      return d2;
    }
    if (typeof next2 !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k3 = next2.key;
    let d3 = next2.data;
    let next3 = next2.next;
    if (H.equal(key, k3)) {
      return d3;
    } else {
      let _param = next3;
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          throw new Error("Not_found", {
                cause: {
                  RE_EXN_ID: "Not_found"
                }
              });
        }
        let k = param.key;
        let data = param.data;
        let next = param.next;
        if (H.equal(key, k)) {
          return data;
        }
        _param = next;
        continue;
      };
    }
  };
  let find_opt = function (h, key) {
    let match = Caml_array.get(h.data, key_index(h, key));
    if (typeof match !== "object") {
      return;
    }
    let k1 = match.key;
    let d1 = match.data;
    let next1 = match.next;
    if (H.equal(key, k1)) {
      return Caml_option.some(d1);
    }
    if (typeof next1 !== "object") {
      return;
    }
    let k2 = next1.key;
    let d2 = next1.data;
    let next2 = next1.next;
    if (H.equal(key, k2)) {
      return Caml_option.some(d2);
    }
    if (typeof next2 !== "object") {
      return;
    }
    let k3 = next2.key;
    let d3 = next2.data;
    let next3 = next2.next;
    if (H.equal(key, k3)) {
      return Caml_option.some(d3);
    } else {
      let _param = next3;
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          return;
        }
        let k = param.key;
        let data = param.data;
        let next = param.next;
        if (H.equal(key, k)) {
          return Caml_option.some(data);
        }
        _param = next;
        continue;
      };
    }
  };
  let find_all = function (h, key) {
    let find_in_bucket = function (_param) {
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          return /* [] */0;
        }
        let k = param.key;
        let d = param.data;
        let next = param.next;
        if (H.equal(k, key)) {
          return {
            hd: d,
            tl: find_in_bucket(next)
          };
        }
        _param = next;
        continue;
      };
    };
    return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
  };
  let replace_bucket = function (key, data, _param) {
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return true;
      }
      let k = param.key;
      let next = param.next;
      if (H.equal(k, key)) {
        param.key = key;
        param.data = data;
        return false;
      }
      _param = next;
      continue;
    };
  };
  let replace = function (h, key, data) {
    let i = key_index(h, key);
    let l = Caml_array.get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.set(h.data, i, {
        TAG: "Cons",
        key: key,
        data: data,
        next: l
      });
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return;
      }
    }
    
  };
  let mem = function (h, key) {
    let _param = Caml_array.get(h.data, key_index(h, key));
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return false;
      }
      let k = param.key;
      let next = param.next;
      if (H.equal(k, key)) {
        return true;
      }
      _param = next;
      continue;
    };
  };
  return {
    create: create,
    clear: clear,
    reset: reset,
    copy: copy,
    add: add,
    remove: remove,
    find: find,
    find_opt: find_opt,
    find_all: find_all,
    replace: replace,
    mem: mem,
    iter: iter,
    filter_map_inplace: filter_map_inplace,
    fold: fold,
    length: length,
    stats: stats
  };
}

function Make(H) {
  let equal = H.equal;
  let key_index = function (h, key) {
    return H.hash(key) & (h.data.length - 1 | 0);
  };
  let add = function (h, key, data) {
    let i = key_index(h, key);
    let bucket = {
      TAG: "Cons",
      key: key,
      data: data,
      next: Caml_array.get(h.data, i)
    };
    Caml_array.set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  let remove = function (h, key) {
    let i = key_index(h, key);
    let _prec = "Empty";
    let _param = Caml_array.get(h.data, i);
    while (true) {
      let param = _param;
      let prec = _prec;
      if (typeof param !== "object") {
        return;
      }
      let k = param.key;
      let next = param.next;
      if (equal(k, key)) {
        h.size = h.size - 1 | 0;
        if (typeof prec !== "object") {
          return Caml_array.set(h.data, i, next);
        } else {
          prec.next = next;
          return;
        }
      }
      _param = next;
      _prec = param;
      continue;
    };
  };
  let find = function (h, key) {
    let match = Caml_array.get(h.data, key_index(h, key));
    if (typeof match !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k1 = match.key;
    let d1 = match.data;
    let next1 = match.next;
    if (equal(key, k1)) {
      return d1;
    }
    if (typeof next1 !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k2 = next1.key;
    let d2 = next1.data;
    let next2 = next1.next;
    if (equal(key, k2)) {
      return d2;
    }
    if (typeof next2 !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k3 = next2.key;
    let d3 = next2.data;
    let next3 = next2.next;
    if (equal(key, k3)) {
      return d3;
    } else {
      let _param = next3;
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          throw new Error("Not_found", {
                cause: {
                  RE_EXN_ID: "Not_found"
                }
              });
        }
        let k = param.key;
        let data = param.data;
        let next = param.next;
        if (equal(key, k)) {
          return data;
        }
        _param = next;
        continue;
      };
    }
  };
  let find_opt = function (h, key) {
    let match = Caml_array.get(h.data, key_index(h, key));
    if (typeof match !== "object") {
      return;
    }
    let k1 = match.key;
    let d1 = match.data;
    let next1 = match.next;
    if (equal(key, k1)) {
      return Caml_option.some(d1);
    }
    if (typeof next1 !== "object") {
      return;
    }
    let k2 = next1.key;
    let d2 = next1.data;
    let next2 = next1.next;
    if (equal(key, k2)) {
      return Caml_option.some(d2);
    }
    if (typeof next2 !== "object") {
      return;
    }
    let k3 = next2.key;
    let d3 = next2.data;
    let next3 = next2.next;
    if (equal(key, k3)) {
      return Caml_option.some(d3);
    } else {
      let _param = next3;
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          return;
        }
        let k = param.key;
        let data = param.data;
        let next = param.next;
        if (equal(key, k)) {
          return Caml_option.some(data);
        }
        _param = next;
        continue;
      };
    }
  };
  let find_all = function (h, key) {
    let find_in_bucket = function (_param) {
      while (true) {
        let param = _param;
        if (typeof param !== "object") {
          return /* [] */0;
        }
        let k = param.key;
        let d = param.data;
        let next = param.next;
        if (equal(k, key)) {
          return {
            hd: d,
            tl: find_in_bucket(next)
          };
        }
        _param = next;
        continue;
      };
    };
    return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
  };
  let replace_bucket = function (key, data, _param) {
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return true;
      }
      let k = param.key;
      let next = param.next;
      if (equal(k, key)) {
        param.key = key;
        param.data = data;
        return false;
      }
      _param = next;
      continue;
    };
  };
  let replace = function (h, key, data) {
    let i = key_index(h, key);
    let l = Caml_array.get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.set(h.data, i, {
        TAG: "Cons",
        key: key,
        data: data,
        next: l
      });
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return;
      }
    }
    
  };
  let mem = function (h, key) {
    let _param = Caml_array.get(h.data, key_index(h, key));
    while (true) {
      let param = _param;
      if (typeof param !== "object") {
        return false;
      }
      let k = param.key;
      let next = param.next;
      if (equal(k, key)) {
        return true;
      }
      _param = next;
      continue;
    };
  };
  let create$1 = function (sz) {
    return create(false, sz);
  };
  return {
    create: create$1,
    clear: clear,
    reset: reset,
    copy: copy,
    add: add,
    remove: remove,
    find: find,
    find_opt: find_opt,
    find_all: find_all,
    replace: replace,
    mem: mem,
    iter: iter,
    filter_map_inplace: filter_map_inplace,
    fold: fold,
    length: length,
    stats: stats
  };
}

let seeded_hash_param = Caml_hash.hash;

export {
  create,
  clear,
  reset,
  copy,
  add,
  find,
  find_opt,
  find_all,
  mem,
  remove,
  replace,
  iter,
  filter_map_inplace,
  fold,
  length,
  randomize,
  is_randomized,
  stats,
  Make,
  MakeSeeded,
  hash,
  seeded_hash,
  hash_param,
  seeded_hash_param,
}
/* No side effect */
