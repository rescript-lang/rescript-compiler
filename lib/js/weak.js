'use strict';

var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Caml_obj = require("./caml_obj.js");
var Caml_weak = require("./caml_weak.js");
var Caml_array = require("./caml_array.js");
var Caml_int32 = require("./caml_int32.js");
var Pervasives = require("./pervasives.js");
var Caml_option = require("./caml_option.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function fill(ar, ofs, len, x) {
  if (ofs < 0 || len < 0 || (ofs + len | 0) > ar.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Weak.fill"
        ];
  } else {
    for(var i = ofs ,i_finish = (ofs + len | 0) - 1 | 0; i <= i_finish; ++i){
      Caml_weak.caml_weak_set(ar, i, x);
    }
    return /* () */0;
  }
}

function Make(H) {
  var emptybucket = Caml_weak.caml_weak_create(0);
  var get_index = function (t, h) {
    return (h & Pervasives.max_int) % t[/* table */0].length;
  };
  var create = function (sz) {
    var sz$1 = sz < 7 ? 7 : sz;
    var sz$2 = sz$1 > 2147483647 ? 2147483647 : sz$1;
    return /* record */[
            /* table */Caml_array.caml_make_vect(sz$2, emptybucket),
            /* hashes */Caml_array.caml_make_vect(sz$2, /* array */[]),
            /* limit */7,
            /* oversize */0,
            /* rover */0
          ];
  };
  var clear = function (t) {
    for(var i = 0 ,i_finish = t[/* table */0].length - 1 | 0; i <= i_finish; ++i){
      Caml_array.caml_array_set(t[/* table */0], i, emptybucket);
      Caml_array.caml_array_set(t[/* hashes */1], i, /* array */[]);
    }
    t[/* limit */2] = 7;
    t[/* oversize */3] = 0;
    return /* () */0;
  };
  var fold = function (f, t, init) {
    return $$Array.fold_right((function (param, param$1) {
                  var _i = 0;
                  var b = param;
                  var _accu = param$1;
                  while(true) {
                    var accu = _accu;
                    var i = _i;
                    if (i >= b.length) {
                      return accu;
                    } else {
                      var match = Caml_weak.caml_weak_get(b, i);
                      if (match !== undefined) {
                        _accu = Curry._2(f, Caml_option.valFromOption(match), accu);
                        _i = i + 1 | 0;
                        continue ;
                      } else {
                        _i = i + 1 | 0;
                        continue ;
                      }
                    }
                  };
                }), t[/* table */0], init);
  };
  var iter = function (f, t) {
    return $$Array.iter((function (param) {
                  var _i = 0;
                  var b = param;
                  while(true) {
                    var i = _i;
                    if (i >= b.length) {
                      return /* () */0;
                    } else {
                      var match = Caml_weak.caml_weak_get(b, i);
                      if (match !== undefined) {
                        Curry._1(f, Caml_option.valFromOption(match));
                        _i = i + 1 | 0;
                        continue ;
                      } else {
                        _i = i + 1 | 0;
                        continue ;
                      }
                    }
                  };
                }), t[/* table */0]);
  };
  var iter_weak = function (f, t) {
    return $$Array.iteri((function (param, param$1) {
                  var _i = 0;
                  var j = param;
                  var b = param$1;
                  while(true) {
                    var i = _i;
                    if (i >= b.length) {
                      return /* () */0;
                    } else {
                      var match = Caml_weak.caml_weak_check(b, i);
                      if (match) {
                        Curry._3(f, b, Caml_array.caml_array_get(t[/* hashes */1], j), i);
                        _i = i + 1 | 0;
                        continue ;
                      } else {
                        _i = i + 1 | 0;
                        continue ;
                      }
                    }
                  };
                }), t[/* table */0]);
  };
  var count_bucket = function (_i, b, _accu) {
    while(true) {
      var accu = _accu;
      var i = _i;
      if (i >= b.length) {
        return accu;
      } else {
        _accu = accu + (
          Caml_weak.caml_weak_check(b, i) ? 1 : 0
        ) | 0;
        _i = i + 1 | 0;
        continue ;
      }
    };
  };
  var count = function (t) {
    return $$Array.fold_right((function (param, param$1) {
                  return count_bucket(0, param, param$1);
                }), t[/* table */0], 0);
  };
  var next_sz = function (n) {
    return Caml_primitive.caml_int_min((Caml_int32.imul(3, n) / 2 | 0) + 3 | 0, 2147483647);
  };
  var prev_sz = function (n) {
    return (((n - 3 | 0) << 1) + 2 | 0) / 3 | 0;
  };
  var test_shrink_bucket = function (t) {
    var bucket = Caml_array.caml_array_get(t[/* table */0], t[/* rover */4]);
    var hbucket = Caml_array.caml_array_get(t[/* hashes */1], t[/* rover */4]);
    var len = bucket.length;
    var prev_len = prev_sz(len);
    var live = count_bucket(0, bucket, 0);
    if (live <= prev_len) {
      var loop = function (_i, _j) {
        while(true) {
          var j = _j;
          var i = _i;
          if (j >= prev_len) {
            if (Caml_weak.caml_weak_check(bucket, i)) {
              _i = i + 1 | 0;
              continue ;
            } else if (Caml_weak.caml_weak_check(bucket, j)) {
              Caml_weak.caml_weak_blit(bucket, j, bucket, i, 1);
              Caml_array.caml_array_set(hbucket, i, Caml_array.caml_array_get(hbucket, j));
              _j = j - 1 | 0;
              _i = i + 1 | 0;
              continue ;
            } else {
              _j = j - 1 | 0;
              continue ;
            }
          } else {
            return 0;
          }
        };
      };
      loop(0, bucket.length - 1 | 0);
      if (prev_len === 0) {
        Caml_array.caml_array_set(t[/* table */0], t[/* rover */4], emptybucket);
        Caml_array.caml_array_set(t[/* hashes */1], t[/* rover */4], /* array */[]);
      } else {
        Caml_obj.caml_obj_truncate(bucket, prev_len + 1 | 0);
        Caml_obj.caml_obj_truncate(hbucket, prev_len);
      }
      if (len > t[/* limit */2] && prev_len <= t[/* limit */2]) {
        t[/* oversize */3] = t[/* oversize */3] - 1 | 0;
      }
      
    }
    t[/* rover */4] = (t[/* rover */4] + 1 | 0) % t[/* table */0].length;
    return /* () */0;
  };
  var add_aux = function (t, setter, d, h, index) {
    var bucket = Caml_array.caml_array_get(t[/* table */0], index);
    var hashes = Caml_array.caml_array_get(t[/* hashes */1], index);
    var sz = bucket.length;
    var _i = 0;
    while(true) {
      var i = _i;
      if (i >= sz) {
        var newsz = Caml_primitive.caml_int_min((Caml_int32.imul(3, sz) / 2 | 0) + 3 | 0, 2147483646);
        if (newsz <= sz) {
          throw [
                Caml_builtin_exceptions.failure,
                "Weak.Make: hash bucket cannot grow more"
              ];
        }
        var newbucket = Caml_weak.caml_weak_create(newsz);
        var newhashes = Caml_array.caml_make_vect(newsz, 0);
        Caml_weak.caml_weak_blit(bucket, 0, newbucket, 0, sz);
        $$Array.blit(hashes, 0, newhashes, 0, sz);
        Curry._3(setter, newbucket, sz, d);
        Caml_array.caml_array_set(newhashes, sz, h);
        Caml_array.caml_array_set(t[/* table */0], index, newbucket);
        Caml_array.caml_array_set(t[/* hashes */1], index, newhashes);
        if (sz <= t[/* limit */2] && newsz > t[/* limit */2]) {
          t[/* oversize */3] = t[/* oversize */3] + 1 | 0;
          for(var _i$1 = 0; _i$1 <= 2; ++_i$1){
            test_shrink_bucket(t);
          }
        }
        if (t[/* oversize */3] > (t[/* table */0].length >> 1)) {
          var t$1 = t;
          var oldlen = t$1[/* table */0].length;
          var newlen = next_sz(oldlen);
          if (newlen > oldlen) {
            var newt = create(newlen);
            var add_weak = (function(newt){
            return function add_weak(ob, oh, oi) {
              var setter = function (nb, ni, param) {
                return Caml_weak.caml_weak_blit(ob, oi, nb, ni, 1);
              };
              var h = Caml_array.caml_array_get(oh, oi);
              return add_aux(newt, setter, undefined, h, get_index(newt, h));
            }
            }(newt));
            iter_weak(add_weak, t$1);
            t$1[/* table */0] = newt[/* table */0];
            t$1[/* hashes */1] = newt[/* hashes */1];
            t$1[/* limit */2] = newt[/* limit */2];
            t$1[/* oversize */3] = newt[/* oversize */3];
            t$1[/* rover */4] = t$1[/* rover */4] % newt[/* table */0].length;
            return /* () */0;
          } else {
            t$1[/* limit */2] = Pervasives.max_int;
            t$1[/* oversize */3] = 0;
            return /* () */0;
          }
        } else {
          return 0;
        }
      } else if (Caml_weak.caml_weak_check(bucket, i)) {
        _i = i + 1 | 0;
        continue ;
      } else {
        Curry._3(setter, bucket, i, d);
        return Caml_array.caml_array_set(hashes, i, h);
      }
    };
  };
  var add = function (t, d) {
    var h = Curry._1(H[/* hash */1], d);
    return add_aux(t, Caml_weak.caml_weak_set, Caml_option.some(d), h, get_index(t, h));
  };
  var find_or = function (t, d, ifnotfound) {
    var h = Curry._1(H[/* hash */1], d);
    var index = get_index(t, h);
    var bucket = Caml_array.caml_array_get(t[/* table */0], index);
    var hashes = Caml_array.caml_array_get(t[/* hashes */1], index);
    var sz = bucket.length;
    var _i = 0;
    while(true) {
      var i = _i;
      if (i >= sz) {
        return Curry._2(ifnotfound, h, index);
      } else if (h === Caml_array.caml_array_get(hashes, i)) {
        var match = Caml_weak.caml_weak_get_copy(bucket, i);
        if (match !== undefined) {
          if (Curry._2(H[/* equal */0], Caml_option.valFromOption(match), d)) {
            var match$1 = Caml_weak.caml_weak_get(bucket, i);
            if (match$1 !== undefined) {
              return Caml_option.valFromOption(match$1);
            } else {
              _i = i + 1 | 0;
              continue ;
            }
          } else {
            _i = i + 1 | 0;
            continue ;
          }
        } else {
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    };
  };
  var merge = function (t, d) {
    return find_or(t, d, (function (h, index) {
                  add_aux(t, Caml_weak.caml_weak_set, Caml_option.some(d), h, index);
                  return d;
                }));
  };
  var find = function (t, d) {
    return find_or(t, d, (function (h, index) {
                  throw Caml_builtin_exceptions.not_found;
                }));
  };
  var find_shadow = function (t, d, iffound, ifnotfound) {
    var h = Curry._1(H[/* hash */1], d);
    var index = get_index(t, h);
    var bucket = Caml_array.caml_array_get(t[/* table */0], index);
    var hashes = Caml_array.caml_array_get(t[/* hashes */1], index);
    var sz = bucket.length;
    var _i = 0;
    while(true) {
      var i = _i;
      if (i >= sz) {
        return ifnotfound;
      } else if (h === Caml_array.caml_array_get(hashes, i)) {
        var match = Caml_weak.caml_weak_get_copy(bucket, i);
        if (match !== undefined) {
          if (Curry._2(H[/* equal */0], Caml_option.valFromOption(match), d)) {
            return Curry._2(iffound, bucket, i);
          } else {
            _i = i + 1 | 0;
            continue ;
          }
        } else {
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    };
  };
  var remove = function (t, d) {
    return find_shadow(t, d, (function (w, i) {
                  return Caml_weak.caml_weak_set(w, i, undefined);
                }), /* () */0);
  };
  var mem = function (t, d) {
    return find_shadow(t, d, (function (w, i) {
                  return true;
                }), false);
  };
  var find_all = function (t, d) {
    var h = Curry._1(H[/* hash */1], d);
    var index = get_index(t, h);
    var bucket = Caml_array.caml_array_get(t[/* table */0], index);
    var hashes = Caml_array.caml_array_get(t[/* hashes */1], index);
    var sz = bucket.length;
    var _i = 0;
    var _accu = /* [] */0;
    while(true) {
      var accu = _accu;
      var i = _i;
      if (i >= sz) {
        return accu;
      } else if (h === Caml_array.caml_array_get(hashes, i)) {
        var match = Caml_weak.caml_weak_get_copy(bucket, i);
        if (match !== undefined) {
          if (Curry._2(H[/* equal */0], Caml_option.valFromOption(match), d)) {
            var match$1 = Caml_weak.caml_weak_get(bucket, i);
            if (match$1 !== undefined) {
              _accu = /* :: */[
                Caml_option.valFromOption(match$1),
                accu
              ];
              _i = i + 1 | 0;
              continue ;
            } else {
              _i = i + 1 | 0;
              continue ;
            }
          } else {
            _i = i + 1 | 0;
            continue ;
          }
        } else {
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    };
  };
  var stats = function (t) {
    var len = t[/* table */0].length;
    var lens = $$Array.map((function (prim) {
            return prim.length;
          }), t[/* table */0]);
    $$Array.sort(Caml_obj.caml_compare, lens);
    var totlen = $$Array.fold_left((function (prim, prim$1) {
            return prim + prim$1 | 0;
          }), 0, lens);
    return /* tuple */[
            len,
            count(t),
            totlen,
            Caml_array.caml_array_get(lens, 0),
            Caml_array.caml_array_get(lens, len / 2 | 0),
            Caml_array.caml_array_get(lens, len - 1 | 0)
          ];
  };
  return /* module */[
          /* create */create,
          /* clear */clear,
          /* merge */merge,
          /* add */add,
          /* remove */remove,
          /* find */find,
          /* find_all */find_all,
          /* mem */mem,
          /* iter */iter,
          /* fold */fold,
          /* count */count,
          /* stats */stats
        ];
}

var create = Caml_weak.caml_weak_create;

function length(prim) {
  return prim.length;
}

var set = Caml_weak.caml_weak_set;

var get = Caml_weak.caml_weak_get;

var get_copy = Caml_weak.caml_weak_get_copy;

var check = Caml_weak.caml_weak_check;

var blit = Caml_weak.caml_weak_blit;

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
