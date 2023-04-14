'use strict';

var Caml_hash_primitive = require("./caml_hash_primitive.js");

function push_back(q, v) {
  var cell = {
    content: v,
    next: undefined
  };
  var last = q.last;
  if (last !== undefined) {
    q.length = q.length + 1 | 0;
    last.next = cell;
    q.last = cell;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
  }
}

function unsafe_pop(q) {
  var cell = q.first;
  var next = cell.next;
  if (next === undefined) {
    q.length = 0;
    q.first = undefined;
    q.last = undefined;
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
  }
  return cell.content;
}

function hash(count, _limit, seed, obj) {
  var s = seed;
  if (typeof obj === "number") {
    var u = obj | 0;
    s = Caml_hash_primitive.hash_mix_int(s, (u + u | 0) + 1 | 0);
    return Caml_hash_primitive.hash_final_mix(s);
  }
  if (typeof obj === "string") {
    s = Caml_hash_primitive.hash_mix_string(s, obj);
    return Caml_hash_primitive.hash_final_mix(s);
  }
  var queue = {
    length: 0,
    first: undefined,
    last: undefined
  };
  var num = count;
  push_back(queue, obj);
  num = num - 1 | 0;
  while(queue.length !== 0 && num > 0) {
    var obj$1 = unsafe_pop(queue);
    if (typeof obj$1 === "number") {
      var u$1 = obj$1 | 0;
      s = Caml_hash_primitive.hash_mix_int(s, (u$1 + u$1 | 0) + 1 | 0);
      num = num - 1 | 0;
    } else if (typeof obj$1 === "string") {
      s = Caml_hash_primitive.hash_mix_string(s, obj$1);
      num = num - 1 | 0;
    } else if (typeof obj$1 !== "boolean" && typeof obj$1 !== "undefined" && typeof obj$1 !== "symbol" && typeof obj$1 !== "function") {
      var size = obj$1.length | 0;
      if (size !== 0) {
        var obj_tag = obj$1.TAG;
        var tag = (size << 10) | obj_tag;
        if (obj_tag === 248) {
          s = Caml_hash_primitive.hash_mix_int(s, obj$1[1]);
        } else {
          s = Caml_hash_primitive.hash_mix_int(s, tag);
          var v = size - 1 | 0;
          var block = v < num ? v : num;
          for(var i = 0; i <= block; ++i){
            push_back(queue, obj$1[i]);
          }
        }
      } else {
        var size$1 = (function(obj,cb){
            var size = 0  
            for(var k in obj){
              cb(obj[k])
              ++ size
            }
            return size
          })(obj$1, (function (v) {
                push_back(queue, v);
              }));
        s = Caml_hash_primitive.hash_mix_int(s, (size$1 << 10) | 0);
      }
    }
    
  };
  return Caml_hash_primitive.hash_final_mix(s);
}

exports.hash = hash;
/* No side effect */
