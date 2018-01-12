'use strict';

var Bs_Array = require("./bs_Array.js");

function bucket_length(_accu, _buckets) {
  while(true) {
    var buckets = _buckets;
    var accu = _accu;
    if (buckets !== undefined) {
      _buckets = buckets.next;
      _accu = accu + 1 | 0;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function do_bucket_iter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      f(buckets.key);
      _buckets = buckets.next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function iter0(h, f) {
  var d = h.buckets;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    do_bucket_iter(f, d[i]);
  }
  return /* () */0;
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = cell.key;
    var match = cell.next;
    if (match !== undefined) {
      _cell = match;
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return i + 1 | 0;
    }
  };
}

function toArray0(h) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    var cell = d[i];
    if (cell !== undefined) {
      current = fillArray(current, arr, cell);
    }
    
  }
  return arr;
}

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b !== undefined) {
      _accu = f(b.key, accu);
      _b = b.next;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function fold0(h, init, f) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = do_bucket_fold(f, d[i], accu);
  }
  return accu;
}

function logStats0(h) {
  var mbl = Bs_Array.foldLeft(h.buckets, 0, (function (m, b) {
          var len = bucket_length(0, b);
          if (m > len) {
            return m;
          } else {
            return len;
          }
        }));
  var histo = Bs_Array.init(mbl + 1 | 0, (function () {
          return 0;
        }));
  Bs_Array.iter(h.buckets, (function (b) {
          var l = bucket_length(0, b);
          histo[l] = histo[l] + 1 | 0;
          return /* () */0;
        }));
  console.log({
        num_bindings: h.size,
        num_buckets: h.buckets.length,
        max_bucket_length: mbl,
        bucket_histogram: histo
      });
  return /* () */0;
}

var C = 0;

exports.C = C;
exports.iter0 = iter0;
exports.fillArray = fillArray;
exports.toArray0 = toArray0;
exports.fold0 = fold0;
exports.logStats0 = logStats0;
/* No side effect */
