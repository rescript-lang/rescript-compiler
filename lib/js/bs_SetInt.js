'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

function splitAuxNoPivot(n, x) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxNoPivot(l, x);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              null,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxNoPivot(r, x);
    return /* tuple */[
            Bs_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            null
          ];
  }
}

function splitAuxPivot(n, x, pres) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    pres[0] = /* true */1;
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxPivot(l, x, pres);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              null,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxPivot(r, x, pres);
    return /* tuple */[
            Bs_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            null
          ];
  }
}

function split(t, x) {
  if (t !== null) {
    var pres = [/* false */0];
    var v = splitAuxPivot(t, x, pres);
    return /* tuple */[
            v,
            pres[0]
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              null,
              null
            ],
            /* false */0
          ];
  }
}

function union(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return Bs_internalSetInt.add(s1, s2.key);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = splitAuxNoPivot(s2, v1);
          return Bs_internalAVLset.joinShared(union(l1, match[0]), v1, union(r1, match[1]));
        }
      } else if (h1 === 1) {
        return Bs_internalSetInt.add(s2, s1.key);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(s1, v2);
        return Bs_internalAVLset.joinShared(union(match$1[0], l2), v2, union(match$1[1], r2));
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
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = inter(l1, match[0]);
      var rr = inter(r1, match[1]);
      if (pres[0]) {
        return Bs_internalAVLset.joinShared(ll, v1, rr);
      } else {
        return Bs_internalAVLset.concatShared(ll, rr);
      }
    } else {
      return null;
    }
  } else {
    return null;
  }
}

function diff(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = diff(l1, match[0]);
      var rr = diff(r1, match[1]);
      if (pres[0]) {
        return Bs_internalAVLset.concatShared(ll, rr);
      } else {
        return Bs_internalAVLset.joinShared(ll, v1, rr);
      }
    } else {
      return s1;
    }
  } else {
    return s1;
  }
}

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var mem = Bs_internalSetInt.mem;

var add = Bs_internalSetInt.add;

var singleton = Bs_internalAVLset.singleton0;

var remove = Bs_internalSetInt.remove;

var cmp = Bs_internalSetInt.cmp;

var eq = Bs_internalSetInt.eq;

var subset = Bs_internalSetInt.subset;

var iter = Bs_internalAVLset.iter0;

var fold = Bs_internalAVLset.fold0;

var forAll = Bs_internalAVLset.forAll0;

var exists = Bs_internalAVLset.exists0;

var filter = Bs_internalAVLset.filterShared0;

var partition = Bs_internalAVLset.partitionShared0;

var length = Bs_internalAVLset.length0;

var toList = Bs_internalAVLset.toList0;

var toArray = Bs_internalAVLset.toArray0;

var ofArray = Bs_internalSetInt.ofArray;

var ofSortedArrayUnsafe = Bs_internalAVLset.ofSortedArrayUnsafe0;

var minOpt = Bs_internalAVLset.minOpt0;

var minNull = Bs_internalAVLset.minNull0;

var maxOpt = Bs_internalAVLset.maxOpt0;

var maxNull = Bs_internalAVLset.maxNull0;

var findOpt = Bs_internalSetInt.findOpt;

var checkInvariant = Bs_internalAVLset.checkInvariant;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.cmp = cmp;
exports.eq = eq;
exports.subset = subset;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.minOpt = minOpt;
exports.minNull = minNull;
exports.maxOpt = maxOpt;
exports.maxNull = maxNull;
exports.split = split;
exports.findOpt = findOpt;
exports.checkInvariant = checkInvariant;
/* No side effect */
