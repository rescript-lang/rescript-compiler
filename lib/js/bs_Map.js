'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function add0(cmp, x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(add0(cmp, x, data, l), v, d, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, d, add0(cmp, x, data, r));
      }
    } else {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function findOpt0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return /* Some */[param[2]];
      }
    } else {
      return /* None */0;
    }
  };
}

function findAssert0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return param[2];
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function findWithDefault0(cmp, def, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return param[2];
      }
    } else {
      return def;
    }
  };
}

function mem0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function remove0(cmp, x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(remove0(cmp, x, l), v, d, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, d, remove0(cmp, x, r));
      }
    } else {
      return Bs_internalAVLtree.merge(l, r);
    }
  } else {
    return /* Empty */0;
  }
}

function split0(cmp, x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var match = split0(cmp, x, l);
        return /* tuple */[
                match[0],
                match[1],
                Bs_internalAVLtree.join(match[2], v, d, r)
              ];
      } else {
        var match$1 = split0(cmp, x, r);
        return /* tuple */[
                Bs_internalAVLtree.join(l, v, d, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    } else {
      return /* tuple */[
              l,
              /* Some */[d],
              r
            ];
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge0(cmp, f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= Bs_internalAVLtree.height(s2)) {
      var match = split0(cmp, v1, s2);
      return Bs_internalAVLtree.concat_or_join(merge0(cmp, f, s1[0], match[0]), v1, f(v1, /* Some */[s1[2]], match[1]), merge0(cmp, f, s1[3], match[2]));
    } else {
      exit = 1;
    }
  } else if (s2) {
    exit = 1;
  } else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[1];
      var match$1 = split0(cmp, v2, s1);
      return Bs_internalAVLtree.concat_or_join(merge0(cmp, f, match$1[0], s2[0]), v2, f(v2, match$1[1], /* Some */[s2[2]]), merge0(cmp, f, match$1[2], s2[3]));
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

function empty(dict) {
  return /* record */[
          /* dict */dict,
          /* data : Empty */0
        ];
}

function isEmpty(map) {
  if (map[/* data */1]) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function singleton(dict, k, v) {
  return /* record */[
          /* dict */dict,
          /* data */Bs_internalAVLtree.singleton0(k, v)
        ];
}

function iter(f, map) {
  return Bs_internalAVLtree.iter0(f, map[/* data */1]);
}

function fold(f, map) {
  var partial_arg = map[/* data */1];
  return (function (param) {
      return Bs_internalAVLtree.fold0(f, partial_arg, param);
    });
}

function forAll(f, map) {
  return Bs_internalAVLtree.forAll0(f, map[/* data */1]);
}

function exists(f, map) {
  return Bs_internalAVLtree.exists0(f, map[/* data */1]);
}

function filter(f, map) {
  return /* record */[
          /* dict */map[/* dict */0],
          /* data */Bs_internalAVLtree.filter0(f, map[/* data */1])
        ];
}

function partition(p, map) {
  var match = Bs_internalAVLtree.partition0(p, map[/* data */1]);
  var map_dict = map[/* dict */0];
  return /* tuple */[
          /* record */[
            /* dict */map_dict,
            /* data */match[0]
          ],
          /* record */[
            /* dict */map_dict,
            /* data */match[1]
          ]
        ];
}

function cardinal(map) {
  return Bs_internalAVLtree.cardinal0(map[/* data */1]);
}

function bindings(map) {
  return Bs_internalAVLtree.bindings0(map[/* data */1]);
}

function minBinding(map) {
  return Bs_internalAVLtree.minBinding0(map[/* data */1]);
}

function maxBinding(map) {
  return Bs_internalAVLtree.maxBinding0(map[/* data */1]);
}

function map(f, m) {
  var m_dict = m[/* dict */0];
  return /* record */[
          /* dict */m_dict,
          /* data */Bs_internalAVLtree.map0(f, m[/* data */1])
        ];
}

function mapi(f, m) {
  var m_dict = m[/* dict */0];
  return /* record */[
          /* dict */m_dict,
          /* data */Bs_internalAVLtree.mapi0(f, m[/* data */1])
        ];
}

function add(key, data, map) {
  var map_dict = map[/* dict */0];
  return /* record */[
          /* dict */map_dict,
          /* data */add0(map_dict[/* cmp */0], key, data, map[/* data */1])
        ];
}

function findOpt(x, map) {
  var X = map[/* dict */0];
  return findOpt0(X[/* cmp */0], x, map[/* data */1]);
}

function findAssert(x, map) {
  var X = map[/* dict */0];
  return findAssert0(X[/* cmp */0], x, map[/* data */1]);
}

function findWithDefault(def, x, map) {
  var X = map[/* dict */0];
  return findWithDefault0(X[/* cmp */0], def, x, map[/* data */1]);
}

function mem(x, map) {
  var X = map[/* dict */0];
  return mem0(X[/* cmp */0], x, map[/* data */1]);
}

function remove(x, map) {
  var map_dict = map[/* dict */0];
  return /* record */[
          /* dict */map_dict,
          /* data */remove0(map_dict[/* cmp */0], x, map[/* data */1])
        ];
}

function split(x, map) {
  var map_dict = map[/* dict */0];
  var match = split0(map_dict[/* cmp */0], x, map[/* data */1]);
  return /* tuple */[
          /* record */[
            /* dict */map_dict,
            /* data */match[0]
          ],
          match[1],
          /* record */[
            /* dict */map_dict,
            /* data */match[2]
          ]
        ];
}

function merge(f, s1, s2) {
  var s1_dict = s1[/* dict */0];
  return /* record */[
          /* dict */s1_dict,
          /* data */merge0(s1_dict[/* cmp */0], f, s1[/* data */1], s2[/* data */1])
        ];
}

function compare(cmp, m1, m2) {
  var X = m1[/* dict */0];
  return compare0(X[/* cmp */0], cmp, m1[/* data */1], m2[/* data */1]);
}

function equal(cmp, m1, m2) {
  var X = m1[/* dict */0];
  return equal0(X[/* cmp */0], cmp, m1[/* data */1], m2[/* data */1]);
}

var empty0 = /* Empty */0;

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
/* No side effect */
