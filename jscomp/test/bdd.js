'use strict';

var Caml_array = require("../../lib/js/caml_array.js");

function $$eval(_bdd, vars) {
  while(true) {
    var bdd = _bdd;
    if (typeof bdd === "number") {
      return bdd === 0;
    }
    if (Caml_array.get(vars, bdd._1)) {
      _bdd = bdd._3;
      continue ;
    }
    _bdd = bdd._0;
    continue ;
  };
}

function getId(bdd) {
  if (typeof bdd === "number") {
    if (bdd !== 0) {
      return 0;
    } else {
      return 1;
    }
  } else {
    return bdd._2;
  }
}

var nodeC = {
  contents: 1
};

var sz_1 = {
  contents: 8191
};

var htab = {
  contents: Caml_array.make(sz_1.contents + 1 | 0, /* [] */0)
};

var n_items = {
  contents: 0
};

function hashVal(x, y, v) {
  return ((x << 1) + y | 0) + (v << 2) | 0;
}

function resize(newSize) {
  var arr = htab.contents;
  var newSz_1 = newSize - 1 | 0;
  var newArr = Caml_array.make(newSize, /* [] */0);
  var copyBucket = function (_bucket) {
    while(true) {
      var bucket = _bucket;
      if (!bucket) {
        return ;
      }
      var n = bucket.hd;
      if (typeof n === "number") {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bdd.ml",
                54,
                27
              ],
              Error: new Error()
            };
      }
      var ind = hashVal(getId(n._0), getId(n._3), n._1) & newSz_1;
      Caml_array.set(newArr, ind, {
            hd: n,
            tl: Caml_array.get(newArr, ind)
          });
      _bucket = bucket.tl;
      continue ;
    };
  };
  for(var n = 0 ,n_finish = sz_1.contents; n <= n_finish; ++n){
    copyBucket(Caml_array.get(arr, n));
  }
  htab.contents = newArr;
  sz_1.contents = newSz_1;
  
}

function insert(idl, idh, v, ind, bucket, newNode) {
  if (n_items.contents <= sz_1.contents) {
    Caml_array.set(htab.contents, ind, {
          hd: newNode,
          tl: bucket
        });
    n_items.contents = n_items.contents + 1 | 0;
    return ;
  }
  resize((sz_1.contents + sz_1.contents | 0) + 2 | 0);
  var ind$1 = hashVal(idl, idh, v) & sz_1.contents;
  return Caml_array.set(htab.contents, ind$1, {
              hd: newNode,
              tl: Caml_array.get(htab.contents, ind$1)
            });
}

function resetUnique(param) {
  sz_1.contents = 8191;
  htab.contents = Caml_array.make(sz_1.contents + 1 | 0, /* [] */0);
  n_items.contents = 0;
  nodeC.contents = 1;
  
}

function mkNode(low, v, high) {
  var idl = getId(low);
  var idh = getId(high);
  if (idl === idh) {
    return low;
  }
  var ind = hashVal(idl, idh, v) & sz_1.contents;
  var bucket = Caml_array.get(htab.contents, ind);
  var _b = bucket;
  while(true) {
    var b = _b;
    if (b) {
      var n = b.hd;
      if (typeof n === "number") {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bdd.ml",
                99,
                31
              ],
              Error: new Error()
            };
      }
      if (v === n._1 && idl === getId(n._0) && idh === getId(n._3)) {
        return n;
      }
      _b = b.tl;
      continue ;
    }
    var n_2 = (nodeC.contents = nodeC.contents + 1 | 0, nodeC.contents);
    var n$1 = /* Node */{
      _0: low,
      _1: v,
      _2: n_2,
      _3: high
    };
    insert(getId(low), getId(high), v, ind, bucket, n$1);
    return n$1;
  };
}

function cmpVar(x, y) {
  if (x < y) {
    return /* LESS */0;
  } else if (x > y) {
    return /* GREATER */2;
  } else {
    return /* EQUAL */1;
  }
}

function mkVar(x) {
  return mkNode(/* Zero */1, x, /* One */0);
}

var andslot1 = Caml_array.make(1999, 0);

var andslot2 = Caml_array.make(1999, 0);

var andslot3 = Caml_array.make(1999, /* Zero */1);

var xorslot1 = Caml_array.make(1999, 0);

var xorslot2 = Caml_array.make(1999, 0);

var xorslot3 = Caml_array.make(1999, /* Zero */1);

var notslot1 = Caml_array.make(1999, 0);

var notslot2 = Caml_array.make(1999, /* One */0);

function hash(x, y) {
  return ((x << 1) + y | 0) % 1999;
}

function not(n) {
  if (typeof n === "number") {
    if (n !== 0) {
      return /* One */0;
    } else {
      return /* Zero */1;
    }
  }
  var id = n._2;
  var h = id % 1999;
  if (id === Caml_array.get(notslot1, h)) {
    return Caml_array.get(notslot2, h);
  }
  var f = mkNode(not(n._0), n._1, not(n._3));
  Caml_array.set(notslot1, h, id);
  Caml_array.set(notslot2, h, f);
  return f;
}

function and2(n1, n2) {
  if (typeof n1 === "number") {
    if (n1 !== 0) {
      return /* Zero */1;
    } else {
      return n2;
    }
  }
  var r1 = n1._3;
  var i1 = n1._2;
  var v1 = n1._1;
  var l1 = n1._0;
  if (typeof n2 === "number") {
    if (n2 !== 0) {
      return /* Zero */1;
    } else {
      return n1;
    }
  }
  var r2 = n2._3;
  var i2 = n2._2;
  var v2 = n2._1;
  var l2 = n2._0;
  var h = hash(i1, i2);
  if (i1 === Caml_array.get(andslot1, h) && i2 === Caml_array.get(andslot2, h)) {
    return Caml_array.get(andslot3, h);
  }
  var match = cmpVar(v1, v2);
  var f;
  switch (match) {
    case /* LESS */0 :
        f = mkNode(and2(l1, n2), v1, and2(r1, n2));
        break;
    case /* EQUAL */1 :
        f = mkNode(and2(l1, l2), v1, and2(r1, r2));
        break;
    case /* GREATER */2 :
        f = mkNode(and2(n1, l2), v2, and2(n1, r2));
        break;
    
  }
  Caml_array.set(andslot1, h, i1);
  Caml_array.set(andslot2, h, i2);
  Caml_array.set(andslot3, h, f);
  return f;
}

function xor(n1, n2) {
  if (typeof n1 === "number") {
    if (n1 !== 0) {
      return n2;
    } else {
      return not(n2);
    }
  }
  var r1 = n1._3;
  var i1 = n1._2;
  var v1 = n1._1;
  var l1 = n1._0;
  if (typeof n2 === "number") {
    if (n2 !== 0) {
      return n1;
    } else {
      return not(n1);
    }
  }
  var r2 = n2._3;
  var i2 = n2._2;
  var v2 = n2._1;
  var l2 = n2._0;
  var h = hash(i1, i2);
  if (i1 === Caml_array.get(andslot1, h) && i2 === Caml_array.get(andslot2, h)) {
    return Caml_array.get(andslot3, h);
  }
  var match = cmpVar(v1, v2);
  var f;
  switch (match) {
    case /* LESS */0 :
        f = mkNode(xor(l1, n2), v1, xor(r1, n2));
        break;
    case /* EQUAL */1 :
        f = mkNode(xor(l1, l2), v1, xor(r1, r2));
        break;
    case /* GREATER */2 :
        f = mkNode(xor(n1, l2), v2, xor(n1, r2));
        break;
    
  }
  Caml_array.set(andslot1, h, i1);
  Caml_array.set(andslot2, h, i2);
  Caml_array.set(andslot3, h, f);
  return f;
}

function hwb(n) {
  var h = function (i, j) {
    if (i === j) {
      return mkNode(/* Zero */1, i, /* One */0);
    } else {
      return xor(and2(not(mkNode(/* Zero */1, j, /* One */0)), h(i, j - 1 | 0)), and2(mkNode(/* Zero */1, j, /* One */0), g(i, j - 1 | 0)));
    }
  };
  var g = function (i, j) {
    if (i === j) {
      return mkNode(/* Zero */1, i, /* One */0);
    } else {
      return xor(and2(not(mkNode(/* Zero */1, i, /* One */0)), h(i + 1 | 0, j)), and2(mkNode(/* Zero */1, i, /* One */0), g(i + 1 | 0, j)));
    }
  };
  return h(0, n - 1 | 0);
}

var seed = {
  contents: 0
};

function random(param) {
  seed.contents = Math.imul(seed.contents, 25173) + 17431 | 0;
  return (seed.contents & 1) > 0;
}

function random_vars(n) {
  var vars = Caml_array.make(n, false);
  for(var i = 0; i < n; ++i){
    Caml_array.set(vars, i, random(undefined));
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
  for(var i = 0 ,i_finish = vars.length; i < i_finish; ++i){
    if (Caml_array.get(vars, i)) {
      ntrue = ntrue + 1 | 0;
    }
    
  }
  return bool_equal($$eval(bdd, vars), ntrue > 0 ? Caml_array.get(vars, ntrue - 1 | 0) : false);
}

function main(param) {
  var bdd = hwb(22);
  var succeeded = true;
  for(var i = 1; i <= 100; ++i){
    succeeded = succeeded && test_hwb(bdd, random_vars(22));
  }
  if (succeeded) {
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bdd.ml",
          233,
          2
        ],
        Error: new Error()
      };
}

main(undefined);

var initSize_1 = 8191;

var zero = /* Zero */1;

var one = /* One */0;

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
