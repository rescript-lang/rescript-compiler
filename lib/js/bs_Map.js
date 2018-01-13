'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function add0(t, x, data, cmp) {
  if (t !== null) {
    var l = t.left;
    var k = t.key;
    var v = t.value;
    var r = t.right;
    var c = cmp(x, k);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(add0(l, x, data, cmp), k, v, r);
      } else {
        return Bs_internalAVLtree.bal(l, k, v, add0(r, x, data, cmp));
      }
    } else {
      return {
              left: l,
              key: x,
              value: data,
              right: r,
              h: t.h
            };
    }
  } else {
    return {
            left: null,
            key: x,
            value: data,
            right: null,
            h: 1
          };
  }
}

function findOpt0(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return /* Some */[n.value];
      }
    } else {
      return /* None */0;
    }
  };
}

function findAssert0(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return n.value;
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function findWithDefault0(_n, x, def, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return n.value;
      }
    } else {
      return def;
    }
  };
}

function mem0(x, _n, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function remove0(n, x, cmp) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var r = n.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(remove0(l, x, cmp), v, n.value, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, n.value, remove0(r, x, cmp));
      }
    } else if (l !== null) {
      if (r !== null) {
        var kr = [r.key];
        var vr = [r.value];
        var r$1 = Bs_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
        return Bs_internalAVLtree.bal(l, kr[0], vr[0], r$1);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else {
    return n;
  }
}

function splitAux(cmp, x, n) {
  var l = n.left;
  var v = n.key;
  var d = n.value;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAux(cmp, x, l);
        return /* tuple */[
                match[0],
                match[1],
                Bs_internalAVLtree.join(match[2], v, d, r)
              ];
      } else {
        return /* tuple */[
                null,
                /* None */0,
                n
              ];
      }
    } else if (r !== null) {
      var match$1 = splitAux(cmp, x, r);
      return /* tuple */[
              Bs_internalAVLtree.join(l, v, d, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    } else {
      return /* tuple */[
              n,
              /* None */0,
              null
            ];
    }
  } else {
    return /* tuple */[
            l,
            /* Some */[d],
            r
          ];
  }
}

function split0(cmp, x, n) {
  if (n !== null) {
    return splitAux(cmp, x, n);
  } else {
    return /* tuple */[
            null,
            /* None */0,
            null
          ];
  }
}

function merge0(s1, s2, f, cmp) {
  var exit = 0;
  if (s1 !== null) {
    if (s1.h >= (
        s2 !== null ? s2.h : 0
      )) {
      var l1 = s1.left;
      var v1 = s1.key;
      var d1 = s1.value;
      var r1 = s1.right;
      var match = split0(cmp, v1, s2);
      return Bs_internalAVLtree.concatOrJoin(merge0(l1, match[0], f, cmp), v1, f(v1, /* Some */[d1], match[1]), merge0(r1, match[2], f, cmp));
    } else {
      exit = 1;
    }
  } else if (s2 !== null) {
    exit = 1;
  } else {
    return null;
  }
  if (exit === 1) {
    if (s2 !== null) {
      var l2 = s2.left;
      var v2 = s2.key;
      var d2 = s2.value;
      var r2 = s2.right;
      var match$1 = split0(cmp, v2, s1);
      return Bs_internalAVLtree.concatOrJoin(merge0(match$1[0], l2, f, cmp), v2, f(v2, match$1[1], /* Some */[d2]), merge0(match$1[2], r2, f, cmp));
    } else {
      return /* assert false */0;
    }
  }
  
}

function cmp0(s1, s2, kcmp, vcmp) {
  var len1 = Bs_internalAVLtree.length0(s1);
  var len2 = Bs_internalAVLtree.length0(s2);
  if (len1 === len2) {
    var _e1 = Bs_internalAVLtree.stackAllLeft(s1, /* [] */0);
    var _e2 = Bs_internalAVLtree.stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var vcmp$1 = vcmp;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var h2 = e2[0];
          var h1 = e1[0];
          var c = kcmp$1(h1.key, h2.key);
          if (c) {
            return c;
          } else {
            var cx = vcmp$1(h1.value, h2.value);
            if (cx) {
              return cx;
            } else {
              _e2 = Bs_internalAVLtree.stackAllLeft(h2.right, e2[1]);
              _e1 = Bs_internalAVLtree.stackAllLeft(h1.right, e1[1]);
              continue ;
              
            }
          }
        } else {
          return 0;
        }
      } else {
        return 0;
      }
    };
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq0(s1, s2, kcmp, vcmp) {
  var len1 = Bs_internalAVLtree.length0(s1);
  var len2 = Bs_internalAVLtree.length0(s2);
  if (len1 === len2) {
    var _e1 = Bs_internalAVLtree.stackAllLeft(s1, /* [] */0);
    var _e2 = Bs_internalAVLtree.stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var vcmp$1 = vcmp;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var h2 = e2[0];
          var h1 = e1[0];
          if (kcmp$1(h1.key, h2.key) === 0 && vcmp$1(h1.value, h2.value)) {
            _e2 = Bs_internalAVLtree.stackAllLeft(h2.right, e2[1]);
            _e1 = Bs_internalAVLtree.stackAllLeft(h1.right, e1[1]);
            continue ;
            
          } else {
            return /* false */0;
          }
        } else {
          return /* true */1;
        }
      } else {
        return /* true */1;
      }
    };
  } else {
    return /* false */0;
  }
}

function ofArray0(cmp, xs) {
  var result = null;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    var match = xs[i];
    result = add0(result, match[0], match[1], cmp);
  }
  return result;
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(map) {
  return Bs_internalAVLtree.isEmpty0(map.data);
}

function singleton(dict, k, v) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.singleton0(k, v)
        };
}

function iter(map, f) {
  return Bs_internalAVLtree.iter0(map.data, f);
}

function fold(map, acc, f) {
  return Bs_internalAVLtree.fold0(map.data, acc, f);
}

function forAll(map, f) {
  return Bs_internalAVLtree.forAll0(map.data, f);
}

function exists(map, f) {
  return Bs_internalAVLtree.exists0(map.data, f);
}

function filter(f, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.filter0(f, map$1)
        };
}

function partition(p, map) {
  var dict = map.dict;
  var map$1 = map.data;
  var match = Bs_internalAVLtree.partition0(p, map$1);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          {
            dict: dict,
            data: match[1]
          }
        ];
}

function length(map) {
  return Bs_internalAVLtree.length0(map.data);
}

function toList(map) {
  return Bs_internalAVLtree.toList0(map.data);
}

function toArray(m) {
  return Bs_internalAVLtree.toArray0(m.data);
}

function minKVOpt(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minKVNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maxKVOpt(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxKVNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function map(m, f) {
  var dict = m.dict;
  var map$1 = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(map$1, f)
        };
}

function mapi(map, f) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(map$1, f)
        };
}

function add(map, key, data) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: add0(map$1, key, data, dict[/* cmp */0])
        };
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: ofArray0(dict[/* cmp */0], data)
        };
}

function findOpt(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return findOpt0(map$1, x, dict[/* cmp */0]);
}

function findAssert(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return findAssert0(map$1, x, dict[/* cmp */0]);
}

function findWithDefault(map, x, def) {
  var dict = map.dict;
  var map$1 = map.data;
  return findWithDefault0(map$1, x, def, dict[/* cmp */0]);
}

function mem(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return mem0(x, map$1, dict[/* cmp */0]);
}

function remove(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: remove0(map$1, x, dict[/* cmp */0])
        };
}

function split(x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  var match = split0(dict[/* cmp */0], x, map$1);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          match[1],
          {
            dict: dict,
            data: match[2]
          }
        ];
}

function merge(s1, s2, f) {
  var dict = s1.dict;
  var s1_data = s1.data;
  var s2_data = s2.data;
  return {
          dict: dict,
          data: merge0(s1_data, s2_data, f, dict[/* cmp */0])
        };
}

function cmp(m1, m2, cmp$1) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return cmp0(m1_data, m2_data, dict[/* cmp */0], cmp$1);
}

function eq(m1, m2, cmp) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return eq0(m1_data, m2_data, dict[/* cmp */0], cmp);
}

var empty0 = Bs_internalAVLtree.empty0;

var isEmpty0 = Bs_internalAVLtree.isEmpty0;

var singleton0 = Bs_internalAVLtree.singleton0;

var iter0 = Bs_internalAVLtree.iter0;

var fold0 = Bs_internalAVLtree.fold0;

var forAll0 = Bs_internalAVLtree.forAll0;

var exists0 = Bs_internalAVLtree.exists0;

var filter0 = Bs_internalAVLtree.filter0;

var partition0 = Bs_internalAVLtree.partition0;

var length0 = Bs_internalAVLtree.length0;

var toList0 = Bs_internalAVLtree.toList0;

var minBinding0 = Bs_internalAVLtree.minKVOpt0;

var maxBinding0 = Bs_internalAVLtree.maxKVOpt0;

var map0 = Bs_internalAVLtree.map0;

var mapi0 = Bs_internalAVLtree.mapi0;

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.merge = merge;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.minKVOpt = minKVOpt;
exports.minKVNull = minKVNull;
exports.maxKVOpt = maxKVOpt;
exports.maxKVNull = maxKVNull;
exports.split = split;
exports.findOpt = findOpt;
exports.findAssert = findAssert;
exports.findWithDefault = findWithDefault;
exports.map = map;
exports.mapi = mapi;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.mem0 = mem0;
exports.add0 = add0;
exports.singleton0 = singleton0;
exports.remove0 = remove0;
exports.merge0 = merge0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.iter0 = iter0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.length0 = length0;
exports.toList0 = toList0;
exports.minBinding0 = minBinding0;
exports.maxBinding0 = maxBinding0;
exports.split0 = split0;
exports.findOpt0 = findOpt0;
exports.findAssert0 = findAssert0;
exports.findWithDefault0 = findWithDefault0;
exports.map0 = map0;
exports.mapi0 = mapi0;
/* No side effect */
