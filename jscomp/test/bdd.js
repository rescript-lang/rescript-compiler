'use strict';

var Caml_array = require("../../lib/js/caml_array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function $$eval(_bdd, vars) {
  while(true) {
    var bdd = _bdd;
    if (typeof bdd === "string") {
      if (bdd === "One") {
        return true;
      } else {
        return false;
      }
    } else if (Caml_array.caml_array_get(vars, bdd.Arg1)) {
      _bdd = bdd.Arg3;
      continue ;
    } else {
      _bdd = bdd.Arg0;
      continue ;
    }
  };
}

function getId(bdd) {
  if (typeof bdd === "string") {
    if (bdd === "One") {
      return 1;
    } else {
      return 0;
    }
  } else {
    return bdd.Arg2;
  }
}

var nodeC = /* record */[/* contents */1];

var sz_1 = /* record */[/* contents */8191];

var htab = /* record */[/* contents */Caml_array.caml_make_vect(sz_1[0] + 1 | 0, "[]")];

var n_items = /* record */[/* contents */0];

function hashVal(x, y, v) {
  return ((x << 1) + y | 0) + (v << 2) | 0;
}

function resize(newSize) {
  var arr = htab[0];
  var newSz_1 = newSize - 1 | 0;
  var newArr = Caml_array.caml_make_vect(newSize, "[]");
  var copyBucket = function (_bucket) {
    while(true) {
      var bucket = _bucket;
      if (bucket !== "[]") {
        var n = bucket.Arg0;
        if (typeof n === "string") {
          if (n === "One") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "bdd.ml",
                    54,
                    27
                  ]
                ];
          } else {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "bdd.ml",
                    54,
                    27
                  ]
                ];
          }
        } else {
          var ind = hashVal(getId(n.Arg0), getId(n.Arg3), n.Arg1) & newSz_1;
          Caml_array.caml_array_set(newArr, ind, /* constructor */{
                tag: "::",
                Arg0: n,
                Arg1: Caml_array.caml_array_get(newArr, ind)
              });
          _bucket = bucket.Arg1;
          continue ;
        }
      } else {
        return /* () */0;
      }
    };
  };
  for(var n = 0 ,n_finish = sz_1[0]; n <= n_finish; ++n){
    copyBucket(Caml_array.caml_array_get(arr, n));
  }
  htab[0] = newArr;
  sz_1[0] = newSz_1;
  return /* () */0;
}

function insert(idl, idh, v, ind, bucket, newNode) {
  if (n_items[0] <= sz_1[0]) {
    Caml_array.caml_array_set(htab[0], ind, /* constructor */{
          tag: "::",
          Arg0: newNode,
          Arg1: bucket
        });
    n_items[0] = n_items[0] + 1 | 0;
    return /* () */0;
  } else {
    resize((sz_1[0] + sz_1[0] | 0) + 2 | 0);
    var ind$1 = hashVal(idl, idh, v) & sz_1[0];
    return Caml_array.caml_array_set(htab[0], ind$1, /* constructor */{
                tag: "::",
                Arg0: newNode,
                Arg1: Caml_array.caml_array_get(htab[0], ind$1)
              });
  }
}

function resetUnique(param) {
  sz_1[0] = 8191;
  htab[0] = Caml_array.caml_make_vect(sz_1[0] + 1 | 0, "[]");
  n_items[0] = 0;
  nodeC[0] = 1;
  return /* () */0;
}

function mkNode(low, v, high) {
  var idl = getId(low);
  var idh = getId(high);
  if (idl === idh) {
    return low;
  } else {
    var ind = hashVal(idl, idh, v) & sz_1[0];
    var bucket = Caml_array.caml_array_get(htab[0], ind);
    var _b = bucket;
    while(true) {
      var b = _b;
      if (b !== "[]") {
        var n = b.Arg0;
        if (typeof n === "string") {
          if (n === "One") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "bdd.ml",
                    99,
                    31
                  ]
                ];
          } else {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "bdd.ml",
                    99,
                    31
                  ]
                ];
          }
        } else if (v === n.Arg1 && idl === getId(n.Arg0) && idh === getId(n.Arg3)) {
          return n;
        } else {
          _b = b.Arg1;
          continue ;
        }
      } else {
        var n$1 = /* constructor */{
          tag: "Node",
          Arg0: low,
          Arg1: v,
          Arg2: (nodeC[0] = nodeC[0] + 1 | 0, nodeC[0]),
          Arg3: high
        };
        insert(getId(low), getId(high), v, ind, bucket, n$1);
        return n$1;
      }
    };
  }
}

function cmpVar(x, y) {
  if (x < y) {
    return "LESS";
  } else if (x > y) {
    return "GREATER";
  } else {
    return "EQUAL";
  }
}

function mkVar(x) {
  return mkNode("Zero", x, "One");
}

var andslot1 = Caml_array.caml_make_vect(1999, 0);

var andslot2 = Caml_array.caml_make_vect(1999, 0);

var andslot3 = Caml_array.caml_make_vect(1999, "Zero");

var xorslot1 = Caml_array.caml_make_vect(1999, 0);

var xorslot2 = Caml_array.caml_make_vect(1999, 0);

var xorslot3 = Caml_array.caml_make_vect(1999, "Zero");

var notslot1 = Caml_array.caml_make_vect(1999, 0);

var notslot2 = Caml_array.caml_make_vect(1999, "One");

function hash(x, y) {
  return ((x << 1) + y | 0) % 1999;
}

function not(n) {
  if (typeof n === "string") {
    if (n === "One") {
      return "Zero";
    } else {
      return "One";
    }
  } else {
    var id = n.Arg2;
    var h = id % 1999;
    if (id === Caml_array.caml_array_get(notslot1, h)) {
      return Caml_array.caml_array_get(notslot2, h);
    } else {
      var f = mkNode(not(n.Arg0), n.Arg1, not(n.Arg3));
      Caml_array.caml_array_set(notslot1, h, id);
      Caml_array.caml_array_set(notslot2, h, f);
      return f;
    }
  }
}

function and2(n1, n2) {
  if (typeof n1 === "string") {
    if (n1 === "One") {
      return n2;
    } else {
      return "Zero";
    }
  } else {
    var r1 = n1.Arg3;
    var i1 = n1.Arg2;
    var v1 = n1.Arg1;
    var l1 = n1.Arg0;
    if (typeof n2 === "string") {
      if (n2 === "One") {
        return n1;
      } else {
        return "Zero";
      }
    } else {
      var r2 = n2.Arg3;
      var i2 = n2.Arg2;
      var v2 = n2.Arg1;
      var l2 = n2.Arg0;
      var h = hash(i1, i2);
      if (i1 === Caml_array.caml_array_get(andslot1, h) && i2 === Caml_array.caml_array_get(andslot2, h)) {
        return Caml_array.caml_array_get(andslot3, h);
      } else {
        var match = cmpVar(v1, v2);
        var f;
        switch (match) {
          case "LESS" :
              f = mkNode(and2(l1, n2), v1, and2(r1, n2));
              break;
          case "EQUAL" :
              f = mkNode(and2(l1, l2), v1, and2(r1, r2));
              break;
          case "GREATER" :
              f = mkNode(and2(n1, l2), v2, and2(n1, r2));
              break;
          
        }
        Caml_array.caml_array_set(andslot1, h, i1);
        Caml_array.caml_array_set(andslot2, h, i2);
        Caml_array.caml_array_set(andslot3, h, f);
        return f;
      }
    }
  }
}

function xor(n1, n2) {
  if (typeof n1 === "string") {
    if (n1 === "One") {
      return not(n2);
    } else {
      return n2;
    }
  } else {
    var r1 = n1.Arg3;
    var i1 = n1.Arg2;
    var v1 = n1.Arg1;
    var l1 = n1.Arg0;
    if (typeof n2 === "string") {
      if (n2 === "One") {
        return not(n1);
      } else {
        return n1;
      }
    } else {
      var r2 = n2.Arg3;
      var i2 = n2.Arg2;
      var v2 = n2.Arg1;
      var l2 = n2.Arg0;
      var h = hash(i1, i2);
      if (i1 === Caml_array.caml_array_get(andslot1, h) && i2 === Caml_array.caml_array_get(andslot2, h)) {
        return Caml_array.caml_array_get(andslot3, h);
      } else {
        var match = cmpVar(v1, v2);
        var f;
        switch (match) {
          case "LESS" :
              f = mkNode(xor(l1, n2), v1, xor(r1, n2));
              break;
          case "EQUAL" :
              f = mkNode(xor(l1, l2), v1, xor(r1, r2));
              break;
          case "GREATER" :
              f = mkNode(xor(n1, l2), v2, xor(n1, r2));
              break;
          
        }
        Caml_array.caml_array_set(andslot1, h, i1);
        Caml_array.caml_array_set(andslot2, h, i2);
        Caml_array.caml_array_set(andslot3, h, f);
        return f;
      }
    }
  }
}

function hwb(n) {
  var h = function (i, j) {
    if (i === j) {
      return mkNode("Zero", i, "One");
    } else {
      return xor(and2(not(mkNode("Zero", j, "One")), h(i, j - 1 | 0)), and2(mkNode("Zero", j, "One"), g(i, j - 1 | 0)));
    }
  };
  var g = function (i, j) {
    if (i === j) {
      return mkNode("Zero", i, "One");
    } else {
      return xor(and2(not(mkNode("Zero", i, "One")), h(i + 1 | 0, j)), and2(mkNode("Zero", i, "One"), g(i + 1 | 0, j)));
    }
  };
  return h(0, n - 1 | 0);
}

var seed = /* record */[/* contents */0];

function random(param) {
  seed[0] = Caml_int32.imul(seed[0], 25173) + 17431 | 0;
  return (seed[0] & 1) > 0;
}

function random_vars(n) {
  var vars = Caml_array.caml_make_vect(n, false);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    Caml_array.caml_array_set(vars, i, random(/* () */0));
  }
  return vars;
}

function bool_equal(a, b) {
  if (a) {
    if (b) {
      return true;
    } else {
      return false;
    }
  } else if (b) {
    return false;
  } else {
    return true;
  }
}

function test_hwb(bdd, vars) {
  var ntrue = 0;
  for(var i = 0 ,i_finish = vars.length - 1 | 0; i <= i_finish; ++i){
    if (Caml_array.caml_array_get(vars, i)) {
      ntrue = ntrue + 1 | 0;
    }
    
  }
  return bool_equal($$eval(bdd, vars), ntrue > 0 ? Caml_array.caml_array_get(vars, ntrue - 1 | 0) : false);
}

function main(param) {
  var bdd = hwb(22);
  var succeeded = true;
  for(var i = 1; i <= 100; ++i){
    succeeded = succeeded && test_hwb(bdd, random_vars(22));
  }
  if (succeeded) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "bdd.ml",
            233,
            2
          ]
        ];
  }
}

main(/* () */0);

var initSize_1 = 8191;

var zero = "Zero";

var one = "One";

var cacheSize = 1999;

exports.$$eval = $$eval;
exports.getId = getId;
exports.initSize_1 = initSize_1;
exports.nodeC = nodeC;
exports.sz_1 = sz_1;
exports.htab = htab;
exports.n_items = n_items;
exports.hashVal = hashVal;
exports.resize = resize;
exports.insert = insert;
exports.resetUnique = resetUnique;
exports.mkNode = mkNode;
exports.cmpVar = cmpVar;
exports.zero = zero;
exports.one = one;
exports.mkVar = mkVar;
exports.cacheSize = cacheSize;
exports.andslot1 = andslot1;
exports.andslot2 = andslot2;
exports.andslot3 = andslot3;
exports.xorslot1 = xorslot1;
exports.xorslot2 = xorslot2;
exports.xorslot3 = xorslot3;
exports.notslot1 = notslot1;
exports.notslot2 = notslot2;
exports.hash = hash;
exports.not = not;
exports.and2 = and2;
exports.xor = xor;
exports.hwb = hwb;
exports.seed = seed;
exports.random = random;
exports.random_vars = random_vars;
exports.bool_equal = bool_equal;
exports.test_hwb = test_hwb;
exports.main = main;
/*  Not a pure module */
