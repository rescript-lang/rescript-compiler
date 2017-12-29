'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add0(cmp, x, t) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLset.bal(add0(cmp, x, l), v, r);
      } else {
        return Bs_internalAVLset.bal(l, v, add0(cmp, x, r));
      }
    } else {
      return t;
    }
  } else {
    return {
            left: null,
            key: x,
            right: null,
            h: 1
          };
  }
}

function splitAux(cmp, x, n) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAux(cmp, x, l);
        return /* tuple */[
                match[0],
                match[1],
                Bs_internalAVLset.join(match[2], v, r)
              ];
      } else {
        return /* tuple */[
                null,
                /* false */0,
                n
              ];
      }
    } else if (r !== null) {
      var match$1 = splitAux(cmp, x, r);
      return /* tuple */[
              Bs_internalAVLset.join(l, v, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    } else {
      return /* tuple */[
              n,
              /* false */0,
              null
            ];
    }
  } else {
    return /* tuple */[
            l,
            /* true */1,
            r
          ];
  }
}

function split0(cmp, x, t) {
  if (t !== null) {
    return splitAux(cmp, x, t);
  } else {
    return /* tuple */[
            null,
            /* false */0,
            null
          ];
  }
}

function mem0(cmp, x, _t) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var v = t.key;
      var c = cmp(x, v);
      if (c) {
        _t = c < 0 ? t.left : t.right;
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function remove0(cmp, x, t) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLset.bal(remove0(cmp, x, l), v, r);
      } else {
        return Bs_internalAVLset.bal(l, v, remove0(cmp, x, r));
      }
    } else {
      return Bs_internalAVLset.merge(l, r);
    }
  } else {
    return t;
  }
}

function union0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add0(cmp, s2.key, s1);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = split0(cmp, v1, s2);
          return Bs_internalAVLset.join(union0(cmp, l1, match[0]), v1, union0(cmp, r1, match[2]));
        }
      } else if (h1 === 1) {
        return add0(cmp, s1.key, s2);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = split0(cmp, v2, s1);
        return Bs_internalAVLset.join(union0(cmp, match$1[0], l2), v2, union0(cmp, match$1[2], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function cmp0(cmp, s1, s2) {
  var cmp$1 = cmp;
  var _e1 = Bs_internalAVLset.cons_enum(s1, /* End */0);
  var _e2 = Bs_internalAVLset.cons_enum(s2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = cmp$1(e1[0], e2[0]);
        if (c !== 0) {
          return c;
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

function eq0(cmp, s1, s2) {
  return +(cmp0(cmp, s1, s2) === 0);
}

function ofArray0(cmp, xs) {
  var result = null;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    result = add0(cmp, xs[i], result);
  }
  return result;
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.empty0
        };
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: ofArray0(dict[/* cmp */0], data)
        };
}

function isEmpty(m) {
  return Bs_internalAVLset.isEmpty0(m.data);
}

function mem(e, m) {
  var dict = m.dict;
  var data = m.data;
  return mem0(dict[/* cmp */0], e, data);
}

function add(e, m) {
  var dict = m.dict;
  var data = m.data;
  return {
          dict: dict,
          data: add0(dict[/* cmp */0], e, data)
        };
}

function singleton(dict, e) {
  return {
          dict: dict,
          data: Bs_internalAVLset.singleton0(e)
        };
}

function remove(e, m) {
  var dict = m.dict;
  var data = m.data;
  return {
          dict: dict,
          data: remove0(dict[/* cmp */0], e, data)
        };
}

function union(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: union0(dict[/* cmp */0], mdata, ndata)
        };
}

function cmp(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return cmp0(dict[/* cmp */0], mdata, ndata);
}

function eq(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return eq0(dict[/* cmp */0], mdata, ndata);
}

function iter(f, m) {
  return Bs_internalAVLset.iter0(f, m.data);
}

function fold(f, m, acc) {
  return Bs_internalAVLset.fold0(f, m.data, acc);
}

function forAll(f, m) {
  return Bs_internalAVLset.forAll0(f, m.data);
}

function exists(f, m) {
  return Bs_internalAVLset.exists0(f, m.data);
}

function filter(f, m) {
  var data = m.data;
  var dict = m.dict;
  return {
          dict: dict,
          data: Bs_internalAVLset.filter0(f, data)
        };
}

function partition(f, m) {
  var mdata = m.data;
  var dict = m.dict;
  var match = Bs_internalAVLset.partition0(f, mdata);
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

function cardinal(m) {
  return Bs_internalAVLset.cardinal0(m.data);
}

function elements(m) {
  return Bs_internalAVLset.elements0(m.data);
}

function toArray(m) {
  return Bs_internalAVLset.toArray0(m.data);
}

function min(m) {
  return Bs_internalAVLset.min0(m.data);
}

function max(m) {
  return Bs_internalAVLset.max0(m.data);
}

function split(e, m) {
  var dict = m.dict;
  var data = m.data;
  var match = split0(dict[/* cmp */0], e, data);
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

var empty0 = Bs_internalAVLset.empty0;

var isEmpty0 = Bs_internalAVLset.isEmpty0;

var singleton0 = Bs_internalAVLset.singleton0;

var iter0 = Bs_internalAVLset.iter0;

var fold0 = Bs_internalAVLset.fold0;

var forAll0 = Bs_internalAVLset.forAll0;

var exists0 = Bs_internalAVLset.exists0;

var filter0 = Bs_internalAVLset.filter0;

var partition0 = Bs_internalAVLset.partition0;

var cardinal0 = Bs_internalAVLset.cardinal0;

var elements0 = Bs_internalAVLset.elements0;

var toArray0 = Bs_internalAVLset.toArray0;

var min0 = Bs_internalAVLset.min0;

var max0 = Bs_internalAVLset.max0;

exports.empty0 = empty0;
exports.empty = empty;
exports.ofArray0 = ofArray0;
exports.ofArray = ofArray;
exports.isEmpty0 = isEmpty0;
exports.isEmpty = isEmpty;
exports.mem0 = mem0;
exports.mem = mem;
exports.add0 = add0;
exports.add = add;
exports.singleton0 = singleton0;
exports.singleton = singleton;
exports.remove0 = remove0;
exports.remove = remove;
exports.union0 = union0;
exports.union = union;
exports.cmp0 = cmp0;
exports.cmp = cmp;
exports.eq0 = eq0;
exports.eq = eq;
exports.iter0 = iter0;
exports.iter = iter;
exports.fold0 = fold0;
exports.fold = fold;
exports.forAll0 = forAll0;
exports.forAll = forAll;
exports.exists0 = exists0;
exports.exists = exists;
exports.filter0 = filter0;
exports.filter = filter;
exports.partition0 = partition0;
exports.partition = partition;
exports.cardinal0 = cardinal0;
exports.cardinal = cardinal;
exports.elements0 = elements0;
exports.elements = elements;
exports.toArray0 = toArray0;
exports.toArray = toArray;
exports.min0 = min0;
exports.min = min;
exports.max0 = max0;
exports.max = max;
exports.split0 = split0;
exports.split = split;
/* No side effect */
