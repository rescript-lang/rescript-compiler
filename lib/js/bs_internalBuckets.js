'use strict';

var Bs_Array               = require("./bs_Array.js");
var Caml_array             = require("./caml_array.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

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
      f(buckets.key, buckets.value);
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

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b !== undefined) {
      _accu = f(b.key, b.value, accu);
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
  var mbl = Bs_Array.foldLeft((function (m, b) {
          var len = bucket_length(0, b);
          if (m > len) {
            return m;
          } else {
            return len;
          }
        }), 0, h.buckets);
  var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
  Bs_Array.iter((function (b) {
          var l = bucket_length(0, b);
          histo[l] = histo[l] + 1 | 0;
          return /* () */0;
        }), h.buckets);
  console.log({
        num_bindings: h.size,
        num_buckets: h.buckets.length,
        max_bucket_length: mbl,
        bucket_histogram: histo
      });
  return /* () */0;
}

function filterMapInplaceBucket(f, h, i, _prec, _cell) {
  while(true) {
    var cell = _cell;
    var prec = _prec;
    var n = cell.next;
    var match = f(cell.key, cell.value);
    if (match) {
      if (prec !== undefined) {
        cell.next = cell;
      } else {
        h.buckets[i] = cell;
      }
      cell.value = match[0];
      if (n !== undefined) {
        _cell = n;
        _prec = cell;
        continue ;
        
      } else {
        cell.next = n;
        return /* () */0;
      }
    } else {
      h.size = h.size - 1 | 0;
      if (n !== undefined) {
        _cell = n;
        continue ;
        
      } else if (prec !== undefined) {
        prec.next = n;
        return /* () */0;
      } else {
        h.buckets[i] = prec;
        return /* () */0;
      }
    }
  };
}

function filterMapInplace0(h, f) {
  var h_buckets = h.buckets;
  for(var i = 0 ,i_finish = h_buckets.length - 1 | 0; i <= i_finish; ++i){
    var v = h_buckets[i];
    if (v !== undefined) {
      filterMapInplaceBucket(f, h, i, Bs_internalBucketsType.emptyOpt, v);
    }
    
  }
  return /* () */0;
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = /* tuple */[
      cell.key,
      cell.value
    ];
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

function fillArrayMap(_i, arr, _cell, f) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = f(cell);
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

function linear(h, f) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    var cell = d[i];
    if (cell !== undefined) {
      current = fillArrayMap(current, arr, cell, f);
    }
    
  }
  return arr;
}

function keys0(h) {
  return linear(h, (function (x) {
                return x.key;
              }));
}

function values0(h) {
  return linear(h, (function (x) {
                return x.value;
              }));
}

function toArray0(h) {
  return linear(h, (function (x) {
                return /* tuple */[
                        x.key,
                        x.value
                      ];
              }));
}

var C = 0;

exports.C                      = C;
exports.bucket_length          = bucket_length;
exports.do_bucket_iter         = do_bucket_iter;
exports.iter0                  = iter0;
exports.do_bucket_fold         = do_bucket_fold;
exports.fold0                  = fold0;
exports.logStats0              = logStats0;
exports.filterMapInplaceBucket = filterMapInplaceBucket;
exports.filterMapInplace0      = filterMapInplace0;
exports.fillArray              = fillArray;
exports.fillArrayMap           = fillArrayMap;
exports.linear                 = linear;
exports.keys0                  = keys0;
exports.values0                = values0;
exports.toArray0               = toArray0;
/* No side effect */
