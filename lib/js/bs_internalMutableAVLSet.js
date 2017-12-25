'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function rotateWithLeftChild(k2) {
  var k1 = k2.left;
  k2.left = k1.right;
  k1.right = k2;
  var hlk2 = Bs_internalAVLset.height(k2.left);
  var hrk2 = Bs_internalAVLset.height(k2.right);
  k2.h = (
    hlk2 > hrk2 ? hlk2 : hrk2
  ) + 1 | 0;
  var hlk1 = Bs_internalAVLset.height(k1.left);
  var hk2 = k2.h;
  k1.h = (
    hlk1 > hk2 ? hlk1 : hk2
  ) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  var k2 = k1.right;
  k1.right = k2.left;
  k2.left = k1;
  var hlk1 = Bs_internalAVLset.height(k1.left);
  var hrk1 = Bs_internalAVLset.height(k1.right);
  k1.h = (
    hlk1 > hrk1 ? hlk1 : hrk1
  ) + 1 | 0;
  var hrk2 = Bs_internalAVLset.height(k2.right);
  var hk1 = k1.h;
  k2.h = (
    hrk2 > hk1 ? hrk2 : hk1
  ) + 1 | 0;
  return k2;
}

function doubleWithLeftChild(k3) {
  var v = rotateWithLeftChild(k3.left);
  k3.left = v;
  return rotateWithLeftChild(k3);
}

function doubleWithRightChild(k2) {
  var v = rotateWithRightChild(k2.right);
  k2.right = v;
  return rotateWithRightChild(k2);
}

function add(x, t) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      var t$1 = x < k ? (t.left = add(x, l), Bs_internalAVLset.height(l) > (2 + Bs_internalAVLset.height(r) | 0) ? (
              x < l.key ? rotateWithLeftChild(t) : doubleWithLeftChild(t)
            ) : t) : (t.right = add(x, r), Bs_internalAVLset.height(r) > (2 + Bs_internalAVLset.height(l) | 0) ? (
              r.key < x ? rotateWithRightChild(t) : doubleWithRightChild(t)
            ) : t);
      var hlt = Bs_internalAVLset.height(t$1.left);
      var hrt = Bs_internalAVLset.height(t$1.right);
      t$1.h = (
        hlt > hrt ? hlt : hrt
      ) + 1 | 0;
      return t$1;
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

var N = 0;

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var singleton = Bs_internalAVLset.singleton0;

var min = Bs_internalAVLset.min0;

var max = Bs_internalAVLset.max0;

var iter = Bs_internalAVLset.iter0;

var fold = Bs_internalAVLset.fold0;

var forAll = Bs_internalAVLset.forAll0;

var exists = Bs_internalAVLset.exists0;

var filter = Bs_internalAVLset.filter0;

var partition = Bs_internalAVLset.partition0;

var cardinal = Bs_internalAVLset.cardinal0;

var elements = Bs_internalAVLset.elements0;

var checkInvariant = Bs_internalAVLset.checkInvariant;

exports.N                    = N;
exports.empty                = empty;
exports.isEmpty              = isEmpty;
exports.singleton            = singleton;
exports.min                  = min;
exports.max                  = max;
exports.iter                 = iter;
exports.fold                 = fold;
exports.forAll               = forAll;
exports.exists               = exists;
exports.filter               = filter;
exports.partition            = partition;
exports.cardinal             = cardinal;
exports.elements             = elements;
exports.checkInvariant       = checkInvariant;
exports.rotateWithLeftChild  = rotateWithLeftChild;
exports.rotateWithRightChild = rotateWithRightChild;
exports.doubleWithLeftChild  = doubleWithLeftChild;
exports.doubleWithRightChild = doubleWithRightChild;
exports.add                  = add;
/* No side effect */
