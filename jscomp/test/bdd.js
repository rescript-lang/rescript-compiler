// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Caml_array = require("../runtime/caml_array");

function $$eval(_bdd, vars) {
  while(/* true */1) {
    var bdd = _bdd;
    if (typeof bdd === "number") {
      if (bdd !== 0) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    }
    else {
      _bdd = vars[bdd[2]] ? bdd[4] : bdd[1];
    }
  };
}

function getId(bdd) {
  if (typeof bdd === "number") {
    if (bdd !== 0) {
      return 0;
    }
    else {
      return 1;
    }
  }
  else {
    return bdd[3];
  }
}

var initSize_1 = 8 * 1024 - 1;

var nodeC = [
  0,
  1
];

var sz_1 = [
  0,
  initSize_1
];

var htab = [
  0,
  Caml_array.caml_make_vect(sz_1[1] + 1, /* [] */0)
];

var n_items = [
  0,
  0
];

function hashVal(x, y, v) {
  return (x << 1) + y + (v << 2);
}

function resize(newSize) {
  var arr = htab[1];
  var newSz_1 = newSize - 1;
  var newArr = Caml_array.caml_make_vect(newSize, /* [] */0);
  var copyBucket = function (_bucket) {
    while(/* true */1) {
      var bucket = _bucket;
      if (bucket) {
        var n = bucket[1];
        if (typeof n === "number") {
          throw [
                0,
                Caml_exceptions.Assert_failure,
                [
                  0,
                  "bdd.ml",
                  54,
                  27
                ]
              ];
        }
        else {
          var ind = hashVal(getId(n[1]), getId(n[4]), n[2]) & newSz_1;
          newArr[ind] = [
            /* :: */0,
            n,
            newArr[ind]
          ];
          _bucket = bucket[2];
        }
      }
      else {
        return /* () */0;
      }
    };
  };
  for(var n = 0 ,n_finish = sz_1[1]; n<= n_finish; ++n){
    copyBucket(arr[n]);
  }
  htab[1] = newArr;
  sz_1[1] = newSz_1;
  return /* () */0;
}

function insert(idl, idh, v, ind, bucket, newNode) {
  if (n_items[1] <= sz_1[1]) {
    htab[1][ind] = [
      /* :: */0,
      newNode,
      bucket
    ];
    return ++ n_items[1];
  }
  else {
    resize(sz_1[1] + sz_1[1] + 2);
    var ind$1 = hashVal(idl, idh, v) & sz_1[1];
    htab[1][ind$1] = [
      /* :: */0,
      newNode,
      htab[1][ind$1]
    ];
    return /* () */0;
  }
}

function resetUnique() {
  sz_1[1] = initSize_1;
  htab[1] = Caml_array.caml_make_vect(sz_1[1] + 1, /* [] */0);
  n_items[1] = 0;
  nodeC[1] = 1;
  return /* () */0;
}

function mkNode(low, v, high) {
  var idl = getId(low);
  var idh = getId(high);
  if (idl === idh) {
    return low;
  }
  else {
    var ind = hashVal(idl, idh, v) & sz_1[1];
    var bucket = htab[1][ind];
    var lookup = function (_b) {
      while(/* true */1) {
        var b = _b;
        if (b) {
          var n = b[1];
          if (typeof n === "number") {
            throw [
                  0,
                  Caml_exceptions.Assert_failure,
                  [
                    0,
                    "bdd.ml",
                    99,
                    31
                  ]
                ];
          }
          else {
            if (v === n[2] && idl === getId(n[1]) && idh === getId(n[4])) {
              return n;
            }
            else {
              _b = b[2];
            }
          }
        }
        else {
          var n_003 = (++ nodeC[1], nodeC[1]);
          var n$1 = [
            /* Node */0,
            low,
            v,
            n_003,
            high
          ];
          insert(getId(low), getId(high), v, ind, bucket, n$1);
          return n$1;
        }
      };
    };
    return lookup(bucket);
  }
}

function cmpVar(x, y) {
  if (x < y) {
    return /* LESS */0;
  }
  else {
    if (x > y) {
      return /* GREATER */2;
    }
    else {
      return /* EQUAL */1;
    }
  }
}

var zero = /* Zero */1;

var one = /* One */0;

function mkVar(x) {
  return mkNode(zero, x, one);
}

var cacheSize = 1999;

var andslot1 = Caml_array.caml_make_vect(cacheSize, 0);

var andslot2 = Caml_array.caml_make_vect(cacheSize, 0);

var andslot3 = Caml_array.caml_make_vect(cacheSize, zero);

var xorslot1 = Caml_array.caml_make_vect(cacheSize, 0);

var xorslot2 = Caml_array.caml_make_vect(cacheSize, 0);

var xorslot3 = Caml_array.caml_make_vect(cacheSize, zero);

var notslot1 = Caml_array.caml_make_vect(cacheSize, 0);

var notslot2 = Caml_array.caml_make_vect(cacheSize, one);

function hash(x, y) {
  return ((x << 1) + y) % cacheSize;
}

function not(n) {
  if (typeof n === "number") {
    if (n !== 0) {
      return /* One */0;
    }
    else {
      return /* Zero */1;
    }
  }
  else {
    var id = n[3];
    var h = id % cacheSize;
    if (id === notslot1[h]) {
      return notslot2[h];
    }
    else {
      var f = mkNode(not(n[1]), n[2], not(n[4]));
      notslot1[h] = id;
      notslot2[h] = f;
      return f;
    }
  }
}

function and2(n1, n2) {
  if (typeof n1 === "number") {
    if (n1 !== 0) {
      return /* Zero */1;
    }
    else {
      return n2;
    }
  }
  else {
    var r1 = n1[4];
    var i1 = n1[3];
    var v1 = n1[2];
    var l1 = n1[1];
    if (typeof n2 === "number") {
      if (n2 !== 0) {
        return /* Zero */1;
      }
      else {
        return n1;
      }
    }
    else {
      var r2 = n2[4];
      var i2 = n2[3];
      var v2 = n2[2];
      var l2 = n2[1];
      var h = hash(i1, i2);
      if (i1 === andslot1[h] && i2 === andslot2[h]) {
        return andslot3[h];
      }
      else {
        var match = cmpVar(v1, v2);
        var f;
        switch (match) {
          case 0 : 
              f = mkNode(and2(l1, n2), v1, and2(r1, n2));
              break;
          case 1 : 
              f = mkNode(and2(l1, l2), v1, and2(r1, r2));
              break;
          case 2 : 
              f = mkNode(and2(n1, l2), v2, and2(n1, r2));
              break;
          
        }
        andslot1[h] = i1;
        andslot2[h] = i2;
        andslot3[h] = f;
        return f;
      }
    }
  }
}

function xor(n1, n2) {
  if (typeof n1 === "number") {
    if (n1 !== 0) {
      return n2;
    }
    else {
      return not(n2);
    }
  }
  else {
    var r1 = n1[4];
    var i1 = n1[3];
    var v1 = n1[2];
    var l1 = n1[1];
    if (typeof n2 === "number") {
      if (n2 !== 0) {
        return n1;
      }
      else {
        return not(n1);
      }
    }
    else {
      var r2 = n2[4];
      var i2 = n2[3];
      var v2 = n2[2];
      var l2 = n2[1];
      var h = hash(i1, i2);
      if (i1 === andslot1[h] && i2 === andslot2[h]) {
        return andslot3[h];
      }
      else {
        var match = cmpVar(v1, v2);
        var f;
        switch (match) {
          case 0 : 
              f = mkNode(xor(l1, n2), v1, xor(r1, n2));
              break;
          case 1 : 
              f = mkNode(xor(l1, l2), v1, xor(r1, r2));
              break;
          case 2 : 
              f = mkNode(xor(n1, l2), v2, xor(n1, r2));
              break;
          
        }
        andslot1[h] = i1;
        andslot2[h] = i2;
        andslot3[h] = f;
        return f;
      }
    }
  }
}

function hwb(n) {
  var h = function (i, j) {
    if (i === j) {
      return mkVar(i);
    }
    else {
      return xor(and2(not(mkVar(j)), h(i, j - 1)), and2(mkVar(j), g(i, j - 1)));
    }
  };
  var g = function (i, j) {
    if (i === j) {
      return mkVar(i);
    }
    else {
      return xor(and2(not(mkVar(i)), h(i + 1, j)), and2(mkVar(i), g(i + 1, j)));
    }
  };
  return h(0, n - 1);
}

var seed = [
  0,
  0
];

function random() {
  seed[1] = seed[1] * 25173 + 17431;
  return +((seed[1] & 1) > 0);
}

function random_vars(n) {
  var vars = Caml_array.caml_make_vect(n, /* false */0);
  for(var i = 0 ,i_finish = n - 1; i<= i_finish; ++i){
    vars[i] = random(/* () */0);
  }
  return vars;
}

function bool_equal(a, b) {
  if (a !== 0) {
    if (b !== 0) {
      return /* true */1;
    }
    else {
      return /* false */0;
    }
  }
  else {
    if (b !== 0) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  }
}

function test_hwb(bdd, vars) {
  var ntrue = 0;
  for(var i = 0 ,i_finish = vars.length - 1; i<= i_finish; ++i){
    if (vars[i]) {
      ++ ntrue;
    }
    
  }
  return bool_equal($$eval(bdd, vars), ntrue > 0 ? vars[ntrue - 1] : /* false */0);
}

function main() {
  var n = 22;
  var bdd = hwb(n);
  var succeeded = /* true */1;
  for(var i = 1; i<= 100; ++i){
    succeeded = +(succeeded && test_hwb(bdd, random_vars(n)));
  }
  if (succeeded) {
    return 0;
  }
  else {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "bdd.ml",
            233,
            2
          ]
        ];
  }
}

main(/* () */0);

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
