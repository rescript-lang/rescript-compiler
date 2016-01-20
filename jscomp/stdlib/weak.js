// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var Sys = require("./sys");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("./array");

function length(x) {
  return x.length - 1;
}

function fill(ar, ofs, len, x) {
  if (ofs < 0 || len < 0 || ofs + len > length(ar)) {
    throw [
          0,
          Caml_exceptions.Invalid_argument,
          "Weak.fill"
        ];
  }
  else {
    for(var i = ofs ,i_finish = ofs + len - 1; i<= i_finish; ++i){
      Caml_primitive.caml_weak_set(ar, i, x);
    }
    return /* () */0;
  }
}

function Make(H) {
  var weak_create = function (prim) {
    return Caml_primitive.caml_weak_create(prim);
  };
  var emptybucket = weak_create(0);
  var get_index = function (t, h) {
    return (h & Pervasives.max_int) % t[1].length;
  };
  var limit = 7;
  var over_limit = 2;
  var create = function (sz) {
    var sz$1 = sz < 7 ? 7 : sz;
    var sz$2 = sz$1 > Sys.max_array_length ? Sys.max_array_length : sz$1;
    return [
            /* record */0,
            Caml_array.caml_make_vect(sz$2, emptybucket),
            Caml_array.caml_make_vect(sz$2, /* array */[]),
            limit,
            0,
            0
          ];
  };
  var clear = function (t) {
    for(var i = 0 ,i_finish = t[1].length - 1; i<= i_finish; ++i){
      t[1][i] = emptybucket;
      t[2][i] = /* array */[];
    }
    t[3] = limit;
    t[4] = 0;
    return /* () */0;
  };
  var fold = function (f, t, init) {
    return $$Array.fold_right(function (param, param$1) {
                var _i = 0;
                var b = param;
                var _accu = param$1;
                while(/* true */1) {
                  var accu = _accu;
                  var i = _i;
                  if (i >= length(b)) {
                    return accu;
                  }
                  else {
                    var match = Caml_primitive.caml_weak_get(b, i);
                    if (match) {
                      _accu = f(match[1], accu);
                      _i = i + 1;
                    }
                    else {
                      _i = i + 1;
                    }
                  }
                };
              }, t[1], init);
  };
  var iter = function (f, t) {
    return $$Array.iter(function (param) {
                var _i = 0;
                var b = param;
                while(/* true */1) {
                  var i = _i;
                  if (i >= length(b)) {
                    return /* () */0;
                  }
                  else {
                    var match = Caml_primitive.caml_weak_get(b, i);
                    if (match) {
                      f(match[1]);
                      _i = i + 1;
                    }
                    else {
                      _i = i + 1;
                    }
                  }
                };
              }, t[1]);
  };
  var iter_weak = function (f, t) {
    return $$Array.iteri(function (param, param$1) {
                var _i = 0;
                var j = param;
                var b = param$1;
                while(/* true */1) {
                  var i = _i;
                  if (i >= length(b)) {
                    return /* () */0;
                  }
                  else {
                    var match = Caml_primitive.caml_weak_check(b, i);
                    if (match !== 0) {
                      f(b, t[2][j], i);
                      _i = i + 1;
                    }
                    else {
                      _i = i + 1;
                    }
                  }
                };
              }, t[1]);
  };
  var count_bucket = function (_i, b, _accu) {
    while(/* true */1) {
      var accu = _accu;
      var i = _i;
      if (i >= length(b)) {
        return accu;
      }
      else {
        _accu = accu + (
          Caml_primitive.caml_weak_check(b, i) ? 1 : 0
        );
        _i = i + 1;
      }
    };
  };
  var count = function (t) {
    return $$Array.fold_right(function (param, param$1) {
                return count_bucket(0, param, param$1);
              }, t[1], 0);
  };
  var next_sz = function (n) {
    return Pervasives.min((3 * n / 2 | 0) + 3, Sys.max_array_length);
  };
  var prev_sz = function (n) {
    return ((n - 3) * 2 + 2) / 3 | 0;
  };
  var test_shrink_bucket = function (t) {
    var bucket = t[1][t[5]];
    var hbucket = t[2][t[5]];
    var len = length(bucket);
    var prev_len = prev_sz(len);
    var live = count_bucket(0, bucket, 0);
    if (live <= prev_len) {
      var loop = function (_i, _j) {
        while(/* true */1) {
          var j = _j;
          var i = _i;
          if (j >= prev_len) {
            if (Caml_primitive.caml_weak_check(bucket, i)) {
              _i = i + 1;
            }
            else {
              if (Caml_primitive.caml_weak_check(bucket, j)) {
                Caml_primitive.caml_weak_blit(bucket, j, bucket, i, 1);
                hbucket[i] = hbucket[j];
                _j = j - 1;
                _i = i + 1;
              }
              else {
                _j = j - 1;
              }
            }
          }
          else {
            return 0;
          }
        };
      };
      loop(0, length(bucket) - 1);
      if (prev_len) {
        Caml_obj_runtime.caml_obj_truncate(bucket, prev_len + 1);
        Caml_obj_runtime.caml_obj_truncate(hbucket, prev_len);
      }
      else {
        t[1][t[5]] = emptybucket;
        t[2][t[5]] = /* array */[];
      }
      if (len > t[3] && prev_len <= t[3]) {
        -- t[4];
      }
      
    }
    t[5] = (t[5] + 1) % t[1].length;
    return /* () */0;
  };
  var add_aux = function (t, setter, d, h, index) {
    var bucket = t[1][index];
    var hashes = t[2][index];
    var sz = length(bucket);
    var _i = 0;
    while(/* true */1) {
      var i = _i;
      if (i >= sz) {
        var newsz = Pervasives.min((3 * sz / 2 | 0) + 3, Sys.max_array_length - 1);
        if (newsz <= sz) {
          Pervasives.failwith("Weak.Make: hash bucket cannot grow more");
        }
        var newbucket = weak_create(newsz);
        var newhashes = Caml_array.caml_make_vect(newsz, 0);
        Caml_primitive.caml_weak_blit(bucket, 0, newbucket, 0, sz);
        $$Array.blit(hashes, 0, newhashes, 0, sz);
        setter(newbucket, sz, d);
        newhashes[sz] = h;
        t[1][index] = newbucket;
        t[2][index] = newhashes;
        if (sz <= t[3] && newsz > t[3]) {
          ++ t[4];
          for(var _i$1 = 0; _i$1<= over_limit; ++_i$1){
            test_shrink_bucket(t);
          }
        }
        if (t[4] > (t[1].length / over_limit | 0)) {
          var t$1 = t;
          var oldlen = t$1[1].length;
          var newlen = next_sz(oldlen);
          if (newlen > oldlen) {
            var newt = create(newlen);
            var add_weak = (function(newt){
            return function (ob, oh, oi) {
              var setter = function (nb, ni, _) {
                return Caml_primitive.caml_weak_blit(ob, oi, nb, ni, 1);
              };
              var h = oh[oi];
              return add_aux(newt, setter, /* None */0, h, get_index(newt, h));
            }
            }(newt));
            iter_weak(add_weak, t$1);
            t$1[1] = newt[1];
            t$1[2] = newt[2];
            t$1[3] = newt[3];
            t$1[4] = newt[4];
            t$1[5] = t$1[5] % newt[1].length;
            return /* () */0;
          }
          else {
            t$1[3] = Pervasives.max_int;
            t$1[4] = 0;
            return /* () */0;
          }
        }
        else {
          return 0;
        }
      }
      else {
        if (Caml_primitive.caml_weak_check(bucket, i)) {
          _i = i + 1;
        }
        else {
          setter(bucket, i, d);
          hashes[i] = h;
          return /* () */0;
        }
      }
    };
  };
  var add = function (t, d) {
    var h = H[2](d);
    return add_aux(t, function (prim, prim$1, prim$2) {
                return Caml_primitive.caml_weak_set(prim, prim$1, prim$2);
              }, [
                /* Some */0,
                d
              ], h, get_index(t, h));
  };
  var find_or = function (t, d, ifnotfound) {
    var h = H[2](d);
    var index = get_index(t, h);
    var bucket = t[1][index];
    var hashes = t[2][index];
    var sz = length(bucket);
    var _i = 0;
    while(/* true */1) {
      var i = _i;
      if (i >= sz) {
        return ifnotfound(h, index);
      }
      else {
        if (h === hashes[i]) {
          var match = Caml_primitive.caml_weak_get_copy(bucket, i);
          if (match) {
            if (H[1](match[1], d)) {
              var match$1 = Caml_primitive.caml_weak_get(bucket, i);
              if (match$1) {
                return match$1[1];
              }
              else {
                _i = i + 1;
              }
            }
            else {
              _i = i + 1;
            }
          }
          else {
            _i = i + 1;
          }
        }
        else {
          _i = i + 1;
        }
      }
    };
  };
  var merge = function (t, d) {
    return find_or(t, d, function (h, index) {
                add_aux(t, function (prim, prim$1, prim$2) {
                      return Caml_primitive.caml_weak_set(prim, prim$1, prim$2);
                    }, [
                      /* Some */0,
                      d
                    ], h, index);
                return d;
              });
  };
  var find = function (t, d) {
    return find_or(t, d, function (_, _$1) {
                throw Caml_exceptions.Not_found;
              });
  };
  var find_shadow = function (t, d, iffound, ifnotfound) {
    var h = H[2](d);
    var index = get_index(t, h);
    var bucket = t[1][index];
    var hashes = t[2][index];
    var sz = length(bucket);
    var _i = 0;
    while(/* true */1) {
      var i = _i;
      if (i >= sz) {
        return ifnotfound;
      }
      else {
        if (h === hashes[i]) {
          var match = Caml_primitive.caml_weak_get_copy(bucket, i);
          if (match) {
            if (H[1](match[1], d)) {
              return iffound(bucket, i);
            }
            else {
              _i = i + 1;
            }
          }
          else {
            _i = i + 1;
          }
        }
        else {
          _i = i + 1;
        }
      }
    };
  };
  var remove = function (t, d) {
    return find_shadow(t, d, function (w, i) {
                return Caml_primitive.caml_weak_set(w, i, /* None */0);
              }, /* () */0);
  };
  var mem = function (t, d) {
    return find_shadow(t, d, function (_, _$1) {
                return /* true */1;
              }, /* false */0);
  };
  var find_all = function (t, d) {
    var h = H[2](d);
    var index = get_index(t, h);
    var bucket = t[1][index];
    var hashes = t[2][index];
    var sz = length(bucket);
    var _i = 0;
    var _accu = /* [] */0;
    while(/* true */1) {
      var accu = _accu;
      var i = _i;
      if (i >= sz) {
        return accu;
      }
      else {
        if (h === hashes[i]) {
          var match = Caml_primitive.caml_weak_get_copy(bucket, i);
          var exit = 0;
          if (match) {
            if (H[1](match[1], d)) {
              var match$1 = Caml_primitive.caml_weak_get(bucket, i);
              if (match$1) {
                _accu = [
                  /* :: */0,
                  match$1[1],
                  accu
                ];
                _i = i + 1;
              }
              else {
                _i = i + 1;
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
            _i = i + 1;
          }
          
        }
        else {
          _i = i + 1;
        }
      }
    };
  };
  var stats = function (t) {
    var len = t[1].length;
    var lens = $$Array.map(length, t[1]);
    $$Array.sort(function (prim, prim$1) {
          return Caml_primitive.caml_compare(prim, prim$1);
        }, lens);
    var totlen = $$Array.fold_left(function (prim, prim$1) {
          return prim + prim$1;
        }, 0, lens);
    return [
            /* tuple */0,
            len,
            count(t),
            totlen,
            lens[0],
            lens[len / 2 | 0],
            lens[len - 1]
          ];
  };
  return [
          0,
          create,
          clear,
          merge,
          add,
          remove,
          find,
          find_all,
          mem,
          iter,
          fold,
          count,
          stats
        ];
}

function create(prim) {
  return Caml_primitive.caml_weak_create(prim);
}

function set(prim, prim$1, prim$2) {
  return Caml_primitive.caml_weak_set(prim, prim$1, prim$2);
}

function get(prim, prim$1) {
  return Caml_primitive.caml_weak_get(prim, prim$1);
}

function get_copy(prim, prim$1) {
  return Caml_primitive.caml_weak_get_copy(prim, prim$1);
}

function check(prim, prim$1) {
  return Caml_primitive.caml_weak_check(prim, prim$1);
}

function blit(prim, prim$1, prim$2, prim$3, prim$4) {
  return Caml_primitive.caml_weak_blit(prim, prim$1, prim$2, prim$3, prim$4);
}

exports.create = create;
exports.length = length;
exports.set = set;
exports.get = get;
exports.get_copy = get_copy;
exports.check = check;
exports.fill = fill;
exports.blit = blit;
exports.Make = Make;
/* No side effect */
