// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_array              = require("../runtime/caml_array");

function $$eval(_bdd, vars) {
  while(true) {
    var bdd = _bdd;
    if (typeof bdd === "number") {
      if (bdd !== 0) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    }
    else if (vars[bdd[1]]) {
      _bdd = bdd[3];
      continue ;
      
    }
    else {
      _bdd = bdd[0];
      continue ;
      
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
    return bdd[2];
  }
}

var initSize_1 = 8 * 1024 - 1;

var nodeC = [1];

var sz_1 = [initSize_1];

var htab = [Caml_array.caml_make_vect(sz_1[0] + 1, /* [] */0)];

var n_items = [0];

function hashVal(x, y, v) {
  return (x << 1) + y + (v << 2);
}

function resize(newSize) {
  var arr = htab[0];
  var newSz_1 = newSize - 1;
  var newArr = Caml_array.caml_make_vect(newSize, /* [] */0);
  var copyBucket = function (_bucket) {
    while(true) {
      var bucket = _bucket;
      if (bucket) {
        var n = bucket[0];
        if (typeof n === "number") {
          throw [
                Caml_builtin_exceptions.Assert_failure,
                [
                  "bdd.ml",
                  54,
                  27
                ]
              ];
        }
        else {
          var ind = hashVal(getId(n[0]), getId(n[3]), n[1]) & newSz_1;
          newArr[ind] = /* :: */[
            n,
            newArr[ind]
          ];
          _bucket = bucket[1];
          continue ;
          
        }
      }
      else {
        return /* () */0;
      }
    };
  };
  for(var n = 0 ,n_finish = sz_1[0]; n<= n_finish; ++n){
    copyBucket(arr[n]);
  }
  htab[0] = newArr;
  sz_1[0] = newSz_1;
  return /* () */0;
}

function insert(idl, idh, v, ind, bucket, newNode) {
  if (n_items[0] <= sz_1[0]) {
    htab[0][ind] = /* :: */[
      newNode,
      bucket
    ];
    return ++ n_items[0];
  }
  else {
    resize(sz_1[0] + sz_1[0] + 2);
    var ind$1 = hashVal(idl, idh, v) & sz_1[0];
    htab[0][ind$1] = /* :: */[
      newNode,
      htab[0][ind$1]
    ];
    return /* () */0;
  }
}

function resetUnique() {
  sz_1[0] = initSize_1;
  htab[0] = Caml_array.caml_make_vect(sz_1[0] + 1, /* [] */0);
  n_items[0] = 0;
  nodeC[0] = 1;
  return /* () */0;
}

function mkNode(low, v, high) {
  var idl = getId(low);
  var idh = getId(high);
  if (idl === idh) {
    return low;
  }
  else {
    var ind = hashVal(idl, idh, v) & sz_1[0];
    var bucket = htab[0][ind];
    var _b = bucket;
    while(true) {
      var b = _b;
      if (b) {
        var n = b[0];
        if (typeof n === "number") {
          throw [
                Caml_builtin_exceptions.Assert_failure,
                [
                  "bdd.ml",
                  99,
                  31
                ]
              ];
        }
        else if (v === n[1] && idl === getId(n[0]) && idh === getId(n[3])) {
          return n;
        }
        else {
          _b = b[1];
          continue ;
          
        }
      }
      else {
        var n_002 = (++ nodeC[0], nodeC[0]);
        var n$1 = /* Node */{
          0: low,
          1: v,
          2: n_002,
          3: high,
          length: 4,
          tag: 0
        };
        insert(getId(low), getId(high), v, ind, bucket, n$1);
        return n$1;
      }
    };
  }
}

function cmpVar(x, y) {
  if (x < y) {
    return /* LESS */0;
  }
  else if (x > y) {
    return /* GREATER */2;
  }
  else {
    return /* EQUAL */1;
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
    var id = n[2];
    var h = id % cacheSize;
    if (id === notslot1[h]) {
      return notslot2[h];
    }
    else {
      var f = mkNode(not(n[0]), n[1], not(n[3]));
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
    var r1 = n1[3];
    var i1 = n1[2];
    var v1 = n1[1];
    var l1 = n1[0];
    if (typeof n2 === "number") {
      if (n2 !== 0) {
        return /* Zero */1;
      }
      else {
        return n1;
      }
    }
    else {
      var r2 = n2[3];
      var i2 = n2[2];
      var v2 = n2[1];
      var l2 = n2[0];
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
    var r1 = n1[3];
    var i1 = n1[2];
    var v1 = n1[1];
    var l1 = n1[0];
    if (typeof n2 === "number") {
      if (n2 !== 0) {
        return n1;
      }
      else {
        return not(n1);
      }
    }
    else {
      var r2 = n2[3];
      var i2 = n2[2];
      var v2 = n2[1];
      var l2 = n2[0];
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
      return mkNode(zero, i, one);
    }
    else {
      return xor(and2(not(mkNode(zero, j, one)), h(i, j - 1)), and2(mkNode(zero, j, one), g(i, j - 1)));
    }
  };
  var g = function (i, j) {
    if (i === j) {
      return mkNode(zero, i, one);
    }
    else {
      return xor(and2(not(mkNode(zero, i, one)), h(i + 1, j)), and2(mkNode(zero, i, one), g(i + 1, j)));
    }
  };
  return h(0, n - 1);
}

var seed = [0];

function random() {
  seed[0] = seed[0] * 25173 + 17431;
  return +((seed[0] & 1) > 0);
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
  else if (b !== 0) {
    return /* false */0;
  }
  else {
    return /* true */1;
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
          Caml_builtin_exceptions.Assert_failure,
          [
            "bdd.ml",
            233,
            2
          ]
        ];
  }
}

main(/* () */0);

exports.$$eval      = $$eval;
exports.getId       = getId;
exports.initSize_1  = initSize_1;
exports.nodeC       = nodeC;
exports.sz_1        = sz_1;
exports.htab        = htab;
exports.n_items     = n_items;
exports.hashVal     = hashVal;
exports.resize      = resize;
exports.insert      = insert;
exports.resetUnique = resetUnique;
exports.mkNode      = mkNode;
exports.cmpVar      = cmpVar;
exports.zero        = zero;
exports.one         = one;
exports.mkVar       = mkVar;
exports.cacheSize   = cacheSize;
exports.andslot1    = andslot1;
exports.andslot2    = andslot2;
exports.andslot3    = andslot3;
exports.xorslot1    = xorslot1;
exports.xorslot2    = xorslot2;
exports.xorslot3    = xorslot3;
exports.notslot1    = notslot1;
exports.notslot2    = notslot2;
exports.hash        = hash;
exports.not         = not;
exports.and2        = and2;
exports.xor         = xor;
exports.hwb         = hwb;
exports.seed        = seed;
exports.random      = random;
exports.random_vars = random_vars;
exports.bool_equal  = bool_equal;
exports.test_hwb    = test_hwb;
exports.main        = main;
/*  Not a pure module */
