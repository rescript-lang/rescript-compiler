'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function add(x, data, param) {
  if (param) {
    var r = param[3];
    var v = param[1];
    var l = param[0];
    if (x === v) {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    } else {
      var d = param[2];
      if (x < v) {
        return Bs_internalAVLtree.bal(add(x, data, l), v, d, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, d, add(x, data, r));
      }
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

function findOpt(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      if (x === v) {
        return /* Some */[param[2]];
      } else {
        _param = x < v ? param[0] : param[3];
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function findAssert(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      if (x === v) {
        return param[2];
      } else {
        _param = x < v ? param[0] : param[3];
        continue ;
        
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function findWithDefault(def, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      if (x === v) {
        return param[2];
      } else {
        _param = x < v ? param[0] : param[3];
        continue ;
        
      }
    } else {
      return def;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      if (x === v) {
        return /* true */1;
      } else {
        _param = x < v ? param[0] : param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function remove(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    if (x === v) {
      return Bs_internalAVLtree.merge(l, r);
    } else if (x < v) {
      return Bs_internalAVLtree.bal(remove(x, l), v, d, r);
    } else {
      return Bs_internalAVLtree.bal(l, v, d, remove(x, r));
    }
  } else {
    return /* Empty */0;
  }
}

function split(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    if (x === v) {
      return /* tuple */[
              l,
              /* Some */[d],
              r
            ];
    } else if (x < v) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              Bs_internalAVLtree.join(match[2], v, d, r)
            ];
    } else {
      var match$1 = split(x, r);
      return /* tuple */[
              Bs_internalAVLtree.join(l, v, d, match$1[0]),
              match$1[1],
              match$1[2]
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

function merge(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= Bs_internalAVLtree.height(s2)) {
      var match = split(v1, s2);
      return Bs_internalAVLtree.concat_or_join(merge(f, s1[0], match[0]), v1, f(v1, /* Some */[s1[2]], match[1]), merge(f, s1[3], match[2]));
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
      var match$1 = split(v2, s1);
      return Bs_internalAVLtree.concat_or_join(merge(f, match$1[0], s2[0]), v2, f(v2, match$1[1], /* Some */[s2[2]]), merge(f, match$1[2], s2[3]));
    } else {
      return /* assert false */0;
    }
  }
  
}

function compare(cmp, m1, m2) {
  var _e1 = Bs_internalAVLtree.cons_enum(m1, /* End */0);
  var _e2 = Bs_internalAVLtree.cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var v2 = e2[0];
        var v1 = e1[0];
        if (v1 !== v2) {
          if (v1 < v2) {
            return -1;
          } else {
            return 1;
          }
        } else {
          var c = cmp(e1[1], e2[1]);
          if (c !== 0) {
            return c;
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

function equal(cmp, m1, m2) {
  var _e1 = Bs_internalAVLtree.cons_enum(m1, /* End */0);
  var _e2 = Bs_internalAVLtree.cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (e1[0] === e2[0]) {
          if (cmp(e1[1], e2[1])) {
            _e2 = Bs_internalAVLtree.cons_enum(e2[2], e2[3]);
            _e1 = Bs_internalAVLtree.cons_enum(e1[2], e1[3]);
            continue ;
            
          } else {
            return /* false */0;
          }
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

var empty = /* Empty */0;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var singleton = Bs_internalAVLtree.singleton0;

var iter = Bs_internalAVLtree.iter0;

var fold = Bs_internalAVLtree.fold0;

var forAll = Bs_internalAVLtree.forAll0;

var exists = Bs_internalAVLtree.exists0;

var filter = Bs_internalAVLtree.filter0;

var partition = Bs_internalAVLtree.partition0;

var cardinal = Bs_internalAVLtree.cardinal0;

var bindings = Bs_internalAVLtree.bindings0;

var minBinding = Bs_internalAVLtree.minBinding0;

var maxBinding = Bs_internalAVLtree.maxBinding0;

var map = Bs_internalAVLtree.map0;

var mapi = Bs_internalAVLtree.mapi0;

exports.empty           = empty;
exports.isEmpty         = isEmpty;
exports.mem             = mem;
exports.add             = add;
exports.singleton       = singleton;
exports.remove          = remove;
exports.merge           = merge;
exports.compare         = compare;
exports.equal           = equal;
exports.iter            = iter;
exports.fold            = fold;
exports.forAll          = forAll;
exports.exists          = exists;
exports.filter          = filter;
exports.partition       = partition;
exports.cardinal        = cardinal;
exports.bindings        = bindings;
exports.minBinding      = minBinding;
exports.maxBinding      = maxBinding;
exports.split           = split;
exports.findOpt         = findOpt;
exports.findAssert      = findAssert;
exports.findWithDefault = findWithDefault;
exports.map             = map;
exports.mapi            = mapi;
/* No side effect */
