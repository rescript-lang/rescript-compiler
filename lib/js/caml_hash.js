'use strict';

var Caml_int32 = require("./caml_int32.js");
var Caml_queue = require("./caml_queue.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function rotl32(x, n) {
  return (x << n) | (x >>> (32 - n | 0));
}

function caml_hash_mix_int(h, d) {
  var d$1 = d;
  d$1 = Caml_int32.imul(d$1, 3432918353);
  d$1 = rotl32(d$1, 15);
  d$1 = Caml_int32.imul(d$1, 461845907);
  var h$1 = h ^ d$1;
  h$1 = rotl32(h$1, 13);
  return (h$1 + (h$1 << 2) | 0) + 3864292196 | 0;
}

function caml_hash_final_mix(h) {
  var h$1 = h ^ (h >>> 16);
  h$1 = Caml_int32.imul(h$1, 2246822507);
  h$1 = h$1 ^ (h$1 >>> 13);
  h$1 = Caml_int32.imul(h$1, 3266489909);
  return h$1 ^ (h$1 >>> 16);
}

function caml_hash_mix_string(h, s) {
  var len = s.length;
  var block = (len / 4 | 0) - 1 | 0;
  var hash = h;
  for(var i = 0; i <= block; ++i){
    var j = (i << 2);
    var w = s.charCodeAt(j) | (s.charCodeAt(j + 1 | 0) << 8) | (s.charCodeAt(j + 2 | 0) << 16) | (s.charCodeAt(j + 3 | 0) << 24);
    hash = caml_hash_mix_int(hash, w);
  }
  var modulo = len & 3;
  if (modulo !== 0) {
    var w$1 = modulo === 3 ? (s.charCodeAt(len - 1 | 0) << 16) | (s.charCodeAt(len - 2 | 0) << 8) | s.charCodeAt(len - 3 | 0) : (
        modulo === 2 ? (s.charCodeAt(len - 1 | 0) << 8) | s.charCodeAt(len - 2 | 0) : s.charCodeAt(len - 1 | 0)
      );
    hash = caml_hash_mix_int(hash, w$1);
  }
  hash = hash ^ len;
  return hash;
}

function caml_hash(count, _, seed, obj) {
  var hash = seed;
  if (typeof obj === "number") {
    var u = obj | 0;
    hash = caml_hash_mix_int(hash, (u + u | 0) + 1 | 0);
    return caml_hash_final_mix(hash);
  } else if (typeof obj === "string") {
    hash = caml_hash_mix_string(hash, obj);
    return caml_hash_final_mix(hash);
  } else {
    var queue = /* record */[
      /* length */0,
      /* tail : None */0
    ];
    var num = count;
    Caml_queue.push(obj, queue);
    num = num - 1 | 0;
    while(queue[/* length */0] !== 0 && num > 0) {
      var obj$1 = Caml_queue.unsafe_pop(queue);
      if (typeof obj$1 === "number") {
        var u$1 = obj$1 | 0;
        hash = caml_hash_mix_int(hash, (u$1 + u$1 | 0) + 1 | 0);
        num = num - 1 | 0;
      } else if (typeof obj$1 === "string") {
        hash = caml_hash_mix_string(hash, obj$1);
        num = num - 1 | 0;
      } else if (typeof obj$1 !== "boolean") {
        if (typeof obj$1 !== "undefined") {
          if (typeof obj$1 === "symbol") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  [
                    "caml_hash.ml",
                    135,
                    8
                  ]
                ];
          } else if (typeof obj$1 !== "function") {
            var size = obj$1.length;
            if (size !== undefined) {
              var obj_tag = obj$1.tag | 0;
              var tag = (size << 10) | obj_tag;
              if (tag === 248) {
                hash = caml_hash_mix_int(hash, obj$1[1]);
              } else {
                hash = caml_hash_mix_int(hash, tag);
                var v = size - 1 | 0;
                var block = v < num ? v : num;
                for(var i = 0; i <= block; ++i){
                  Caml_queue.push(obj$1[i], queue);
                }
              }
            }
            
          }
          
        }
        
      }
      
    };
    return caml_hash_final_mix(hash);
  }
}

exports.caml_hash_mix_int = caml_hash_mix_int;
exports.caml_hash_mix_string = caml_hash_mix_string;
exports.caml_hash_final_mix = caml_hash_final_mix;
exports.caml_hash = caml_hash;
/* No side effect */
