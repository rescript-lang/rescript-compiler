'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add(x, t) {
  if (t !== null) {
    var l = t.left;
    var v = t.value;
    var r = t.right;
    if (x === v) {
      return t;
    } else if (x < v) {
      return Bs_internalAVLset.bal(add(x, l), v, r);
    } else {
      return Bs_internalAVLset.bal(l, v, add(x, r));
    }
  } else {
    return {
            left: null,
            value: x,
            right: null,
            h: 1
          };
  }
}

function split(x, t) {
  if (t !== null) {
    var l = t.left;
    var v = t.value;
    var r = t.right;
    if (x === v) {
      return /* tuple */[
              l,
              /* true */1,
              r
            ];
    } else if (x < v) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              Bs_internalAVLset.join(match[2], v, r)
            ];
    } else {
      var match$1 = split(x, r);
      return /* tuple */[
              Bs_internalAVLset.join(l, v, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
    return /* tuple */[
            null,
            /* false */0,
            null
          ];
  }
}

function mem(x, _t) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var l = t.left;
      var v = t.value;
      var r = t.right;
      if (x === v) {
        return /* true */1;
      } else {
        _t = x < v ? l : r;
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function remove(x, t) {
  if (t !== null) {
    var l = t.left;
    var v = t.value;
    var r = t.right;
    if (x === v) {
      return Bs_internalAVLset.merge(l, r);
    } else if (x < v) {
      return Bs_internalAVLset.bal(remove(x, l), v, r);
    } else {
      return Bs_internalAVLset.bal(l, v, remove(x, r));
    }
  } else {
    return t;
  }
}

function union(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.value;
      var r1 = s1.right;
      var h1 = s1.h;
      var l2 = s2.left;
      var v2 = s2.value;
      var r2 = s2.right;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(v2, s1);
        } else {
          var match = split(v1, s2);
          return Bs_internalAVLset.join(union(l1, match[0]), v1, union(r1, match[2]));
        }
      } else if (h1 === 1) {
        return add(v1, s2);
      } else {
        var match$1 = split(v2, s1);
        return Bs_internalAVLset.join(union(match$1[0], l2), v2, union(match$1[2], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.value;
      var r1 = s1.right;
      var match = split(v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bs_internalAVLset.join(inter(l1, l2), v1, inter(r1, match[2]));
      } else {
        return Bs_internalAVLset.concat(inter(l1, l2), inter(r1, match[2]));
      }
    } else {
      return s2;
    }
  } else {
    return s1;
  }
}

function diff(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.value;
      var r1 = s1.right;
      var match = split(v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bs_internalAVLset.concat(diff(l1, l2), diff(r1, match[2]));
      } else {
        return Bs_internalAVLset.join(diff(l1, l2), v1, diff(r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return s1;
  }
}

function cmp(s1, s2) {
  var _e1 = Bs_internalAVLset.cons_enum(s1, /* End */0);
  var _e2 = Bs_internalAVLset.cons_enum(s2, /* End */0);
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
          _e2 = Bs_internalAVLset.cons_enum(e2[1], e2[2]);
          _e1 = Bs_internalAVLset.cons_enum(e1[1], e1[2]);
          continue ;
          
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

function eq(s1, s2) {
  return +(cmp(s1, s2) === 0);
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1 !== null) {
      if (s2 !== null) {
        var l1 = s1.left;
        var v1 = s1.value;
        var r1 = s1.right;
        var l2 = s2.left;
        var v2 = s2.value;
        var r2 = s2.right;
        if (v1 === v2) {
          if (subset(l1, l2)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (v1 < v2) {
          if (subset({
                  left: l1,
                  value: v1,
                  right: null,
                  h: 0
                }, l2)) {
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (subset({
                left: null,
                value: v1,
                right: r1,
                h: 0
              }, r2)) {
          _s1 = l1;
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function find(x, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var l = n.left;
      var v = n.value;
      var r = n.right;
      if (x === v) {
        return /* Some */[v];
      } else {
        _n = x < v ? l : r;
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var singleton = Bs_internalAVLset.singleton0;

var iter = Bs_internalAVLset.iter0;

var fold = Bs_internalAVLset.fold0;

var forAll = Bs_internalAVLset.forAll0;

var exists = Bs_internalAVLset.exists0;

var filter = Bs_internalAVLset.filter0;

var partition = Bs_internalAVLset.partition0;

var cardinal = Bs_internalAVLset.cardinal0;

var elements = Bs_internalAVLset.elements0;

var min = Bs_internalAVLset.min0;

var max = Bs_internalAVLset.max0;

exports.empty     = empty;
exports.isEmpty   = isEmpty;
exports.mem       = mem;
exports.add       = add;
exports.singleton = singleton;
exports.remove    = remove;
exports.union     = union;
exports.inter     = inter;
exports.diff      = diff;
exports.cmp       = cmp;
exports.eq        = eq;
exports.subset    = subset;
exports.iter      = iter;
exports.fold      = fold;
exports.forAll    = forAll;
exports.exists    = exists;
exports.filter    = filter;
exports.partition = partition;
exports.cardinal  = cardinal;
exports.elements  = elements;
exports.min       = min;
exports.max       = max;
exports.split     = split;
exports.find      = find;
/* Bs_internalAVLset Not a pure module */
