'use strict';


function binarySearch(upper, id, array) {
  var _lower = 0;
  var _upper = upper;
  while(true) {
    var upper$1 = _upper;
    var lower = _lower;
    if (lower >= upper$1) {
      throw new Error("binarySearchAux");
    }
    var mid = (lower + upper$1 | 0) / 2 | 0;
    var match = array[mid];
    var i = match[0];
    if (i === id) {
      return match[1];
    }
    if (i < id) {
      _lower = mid + 1 | 0;
      continue ;
    }
    _upper = mid;
    continue ;
  };
}

function revSearch(len, array, x) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === len) {
      return ;
    }
    var match = array[i];
    if (match[1] === x) {
      return match[0];
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function revSearchAssert(len, array, x) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= len) {
      throw new Error("File \"js_mapperRt.ml\", line 63, characters 4-10");
    }
    var match = array[i];
    if (match[1] === x) {
      return match[0];
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function toInt(i, xs) {
  return xs[i];
}

function fromInt(len, xs, $$enum) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === len) {
      return ;
    }
    var k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function fromIntAssert(len, xs, $$enum) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= len) {
      throw new Error("File \"js_mapperRt.ml\", line 87, characters 4-10");
    }
    var k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

exports.binarySearch = binarySearch;
exports.revSearch = revSearch;
exports.revSearchAssert = revSearchAssert;
exports.toInt = toInt;
exports.fromInt = fromInt;
exports.fromIntAssert = fromIntAssert;
/* No side effect */
