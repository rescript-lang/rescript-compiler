'use strict';


function binSearch(upper, id, array) {
  var _lower = 0;
  var _upper = upper;
  var xs = array;
  var k = id;
  while(true) {
    var upper$1 = _upper;
    var lower = _lower;
    var mid = (lower + upper$1 | 0) / 2 | 0;
    var match = xs[mid];
    var i = match[0];
    if (i === k) {
      return match[1];
    } else if (i < k) {
      _lower = mid + 1 | 0;
      continue ;
      
    } else {
      _upper = mid - 1 | 0;
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

exports.binSearch = binSearch;
exports.revSearch = revSearch;
exports.toInt     = toInt;
exports.fromInt   = fromInt;
/* No side effect */
