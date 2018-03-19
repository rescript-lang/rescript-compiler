'use strict';


function binarySearch(upper, id, array) {
  var _lower = 0;
  var _upper = upper;
  var xs = array;
  var k = id;
  while(true) {
    var upper$1 = _upper;
    var lower = _lower;
    if (lower >= upper$1) {
      throw new Error("File \"js_mapperRt.ml\", line 35, characters 4-10");
    }
    var mid = (lower + upper$1 | 0) / 2 | 0;
    var match = xs[mid];
    var i = match[0];
    if (i === k) {
      return match[1];
    } else if (i < k) {
      _lower = mid + 1 | 0;
      continue ;
    } else {
      _upper = mid;
      continue ;
    }
  };
}

function revSearch(len, array, x) {
  var _i = 0;
  var len$1 = len;
  var xs = array;
  var k = x;
  while(true) {
    var i = _i;
    if (i === len$1) {
      return /* None */0;
    } else {
      var match = xs[i];
      if (match[1] === k) {
        return /* Some */[match[0]];
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    }
  };
}

function revSearchAssert(len, array, x) {
  var len$1 = len;
  var _i = 0;
  var xs = array;
  var k = x;
  while(true) {
    var i = _i;
    if (i >= len$1) {
      throw new Error("File \"js_mapperRt.ml\", line 64, characters 4-10");
    }
    var match = xs[i];
    if (match[1] === k) {
      return match[0];
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function toInt(i, xs) {
  return xs[i];
}

function fromInt(len, xs, $$enum) {
  var $$enum$1 = $$enum;
  var _i = 0;
  var len$1 = len;
  var xs$1 = xs;
  while(true) {
    var i = _i;
    if (i === len$1) {
      return /* None */0;
    } else {
      var k = xs$1[i];
      if (k === $$enum$1) {
        return /* Some */[i];
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    }
  };
}

function fromIntAssert(len, xs, $$enum) {
  var len$1 = len;
  var $$enum$1 = $$enum;
  var _i = 0;
  var xs$1 = xs;
  while(true) {
    var i = _i;
    if (i >= len$1) {
      throw new Error("File \"js_mapperRt.ml\", line 88, characters 4-10");
    }
    var k = xs$1[i];
    if (k === $$enum$1) {
      return i;
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

exports.binarySearch = binarySearch;
exports.revSearch = revSearch;
exports.revSearchAssert = revSearchAssert;
exports.toInt = toInt;
exports.fromInt = fromInt;
exports.fromIntAssert = fromIntAssert;
/* No side effect */
