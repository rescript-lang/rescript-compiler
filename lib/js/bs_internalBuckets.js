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

function max(m, n) {
  if (m > n) {
    return m;
  } else {
    return n;
  }
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

function iter0(f, h) {
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

function fold0(f, h, init) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = do_bucket_fold(f, d[i], accu);
  }
  return accu;
}

function logStats0(h) {
  var mbl = Bs_Array.foldLeft((function (m, b) {
          return max(m, bucket_length(0, b));
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

function filterMapInplaceBucket(f, h, i, _prec, _bucket) {
  while(true) {
    var bucket = _bucket;
    var prec = _prec;
    if (bucket !== undefined) {
      var match = f(bucket.key, bucket.value);
      if (match) {
        if (prec !== undefined) {
          bucket.next = bucket;
        } else {
          h.buckets[i] = bucket;
        }
        bucket.value = match[0];
        _bucket = bucket.next;
        _prec = bucket;
        continue ;
        
      } else {
        h.size = h.size - 1 | 0;
        _bucket = bucket.next;
        continue ;
        
      }
    } else if (prec !== undefined) {
      prec.next = Bs_internalBucketsType.emptyOpt;
      return /* () */0;
    } else {
      h.buckets[i] = Bs_internalBucketsType.emptyOpt;
      return /* () */0;
    }
  };
}

function filterMapInplace0(f, h) {
  var h_buckets = h.buckets;
  for(var i = 0 ,i_finish = h_buckets.length - 1 | 0; i <= i_finish; ++i){
    filterMapInplaceBucket(f, h, i, Bs_internalBucketsType.emptyOpt, h_buckets[i]);
  }
  return /* () */0;
}

var C = 0;

exports.C                      = C;
exports.bucket_length          = bucket_length;
exports.max                    = max;
exports.do_bucket_iter         = do_bucket_iter;
exports.iter0                  = iter0;
exports.do_bucket_fold         = do_bucket_fold;
exports.fold0                  = fold0;
exports.logStats0              = logStats0;
exports.filterMapInplaceBucket = filterMapInplaceBucket;
exports.filterMapInplace0      = filterMapInplace0;
/* Bs_internalBucketsType Not a pure module */
