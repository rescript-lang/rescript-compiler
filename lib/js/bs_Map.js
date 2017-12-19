'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function add0(cmp, x, data, t) {
  if (t !== null) {
    var l = t.left;
    var k = t.key;
    var v = t.value;
    var r = t.right;
    var c = cmp(x, k);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(add0(cmp, x, data, l), k, v, r);
      } else {
        return Bs_internalAVLtree.bal(l, k, v, add0(cmp, x, data, r));
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

function findOpt0(cmp, x, _n) {
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

function findAssert0(cmp, x, _n) {
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

function findWithDefault0(cmp, def, x, _n) {
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

function mem0(cmp, x, _n) {
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

function remove0(cmp, x, n) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var r = n.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(remove0(cmp, x, l), v, n.value, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, n.value, remove0(cmp, x, r));
      }
    } else {
      return Bs_internalAVLtree.merge(l, r);
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

function merge0(cmp, f, s1, s2) {
  var exit = 0;
  if (s1 !== null) {
    if (s1.h >= Bs_internalAVLtree.height(s2)) {
      var l1 = s1.left;
      var v1 = s1.key;
      var d1 = s1.value;
      var r1 = s1.right;
      var match = split0(cmp, v1, s2);
      return Bs_internalAVLtree.concat_or_join(merge0(cmp, f, l1, match[0]), v1, f(v1, /* Some */[d1], match[1]), merge0(cmp, f, r1, match[2]));
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
      return Bs_internalAVLtree.concat_or_join(merge0(cmp, f, match$1[0], l2), v2, f(v2, match$1[1], /* Some */[d2]), merge0(cmp, f, match$1[2], r2));
    } else {
      return /* assert false */0;
    }
  }
  
}

function compare0(keycmp, cmp, m1, m2) {
  var _e1 = Bs_internalAVLtree.cons_enum(m1, /* End */0);
  var _e2 = Bs_internalAVLtree.cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = keycmp(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = cmp(e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = Bs_internalAVLtree.cons_enum(e2[2], e2[3]);
            _e1 = Bs_internalAVLtree.cons_enum(e1[2], e1[3]);
            continue ;
            
          }
        }
      } else {
        return 1;
      }
    } else if (e2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function equal0(keycmp, cmp, m1, m2) {
  var _e1 = Bs_internalAVLtree.cons_enum(m1, /* End */0);
  var _e2 = Bs_internalAVLtree.cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (keycmp(e1[0], e2[0])) {
          return /* false */0;
        } else if (cmp(e1[1], e2[1])) {
          _e2 = Bs_internalAVLtree.cons_enum(e2[2], e2[3]);
          _e1 = Bs_internalAVLtree.cons_enum(e1[2], e1[3]);
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else if (e2) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function ofArray0(cmp, xs) {
  var result = null;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    var match = xs[i];
    result = add0(cmp, match[0], match[1], result);
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

function iter(f, map) {
  return Bs_internalAVLtree.iter0(f, map.data);
}

function fold(f, map, acc) {
  return Bs_internalAVLtree.fold0(f, map.data, acc);
}

function forAll(f, map) {
  return Bs_internalAVLtree.forAll0(f, map.data);
}

function exists(f, map) {
  return Bs_internalAVLtree.exists0(f, map.data);
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

function cardinal(map) {
  return Bs_internalAVLtree.cardinal0(map.data);
}

function bindings(map) {
  return Bs_internalAVLtree.bindings0(map.data);
}

function minBinding(map) {
  return Bs_internalAVLtree.minBinding0(map.data);
}

function maxBinding(map) {
  return Bs_internalAVLtree.maxBinding0(map.data);
}

function map(f, map$1) {
  var dict = map$1.dict;
  var map$2 = map$1.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(f, map$2)
        };
}

function mapi(f, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(f, map$1)
        };
}

function add(key, data, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: add0(dict[/* cmp */0], key, data, map$1)
        };
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: ofArray0(dict[/* cmp */0], data)
        };
}

function findOpt(x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return findOpt0(dict[/* cmp */0], x, map$1);
}

function findAssert(x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return findAssert0(dict[/* cmp */0], x, map$1);
}

function findWithDefault(def, x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return findWithDefault0(dict[/* cmp */0], def, x, map$1);
}

function mem(x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return mem0(dict[/* cmp */0], x, map$1);
}

function remove(x, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: remove0(dict[/* cmp */0], x, map$1)
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

function merge(f, s1, s2) {
  var dict = s1.dict;
  var s1_data = s1.data;
  var s2_data = s2.data;
  return {
          dict: dict,
          data: merge0(dict[/* cmp */0], f, s1_data, s2_data)
        };
}

function compare(cmp, m1, m2) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return compare0(dict[/* cmp */0], cmp, m1_data, m2_data);
}

function equal(cmp, m1, m2) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return equal0(dict[/* cmp */0], cmp, m1_data, m2_data);
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

var cardinal0 = Bs_internalAVLtree.cardinal0;

var bindings0 = Bs_internalAVLtree.bindings0;

var minBinding0 = Bs_internalAVLtree.minBinding0;

var maxBinding0 = Bs_internalAVLtree.maxBinding0;

var map0 = Bs_internalAVLtree.map0;

var mapi0 = Bs_internalAVLtree.mapi0;

exports.empty0           = empty0;
exports.empty            = empty;
exports.ofArray0         = ofArray0;
exports.ofArray          = ofArray;
exports.isEmpty0         = isEmpty0;
exports.isEmpty          = isEmpty;
exports.mem0             = mem0;
exports.mem              = mem;
exports.add0             = add0;
exports.add              = add;
exports.singleton0       = singleton0;
exports.singleton        = singleton;
exports.remove0          = remove0;
exports.remove           = remove;
exports.merge0           = merge0;
exports.merge            = merge;
exports.compare0         = compare0;
exports.compare          = compare;
exports.equal0           = equal0;
exports.equal            = equal;
exports.iter0            = iter0;
exports.iter             = iter;
exports.fold0            = fold0;
exports.fold             = fold;
exports.forAll0          = forAll0;
exports.forAll           = forAll;
exports.exists0          = exists0;
exports.exists           = exists;
exports.filter0          = filter0;
exports.filter           = filter;
exports.partition0       = partition0;
exports.partition        = partition;
exports.cardinal0        = cardinal0;
exports.cardinal         = cardinal;
exports.bindings0        = bindings0;
exports.bindings         = bindings;
exports.minBinding0      = minBinding0;
exports.minBinding       = minBinding;
exports.maxBinding0      = maxBinding0;
exports.maxBinding       = maxBinding;
exports.split0           = split0;
exports.split            = split;
exports.findOpt0         = findOpt0;
exports.findOpt          = findOpt;
exports.findAssert0      = findAssert0;
exports.findAssert       = findAssert;
exports.findWithDefault0 = findWithDefault0;
exports.findWithDefault  = findWithDefault;
exports.map0             = map0;
exports.map              = map;
exports.mapi0            = mapi0;
exports.mapi             = mapi;
/* Bs_internalAVLtree Not a pure module */
