

import * as Caml_hash_primitive from "./caml_hash_primitive.js";

function push_back(q, v) {
  let cell = {
    content: v,
    next: undefined
  };
  let last = q.last;
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
  let cell = q.first;
  let next = cell.next;
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
  let s = seed;
  if (typeof obj === "number") {
    let u = obj | 0;
    s = Caml_hash_primitive.hash_mix_int(s, (u + u | 0) + 1 | 0);
    return Caml_hash_primitive.hash_final_mix(s);
  }
  if (typeof obj === "string") {
    s = Caml_hash_primitive.hash_mix_string(s, obj);
    return Caml_hash_primitive.hash_final_mix(s);
  }
  let queue = {
    length: 0,
    first: undefined,
    last: undefined
  };
  let num = count;
  push_back(queue, obj);
  num = num - 1 | 0;
  while (queue.length !== 0 && num > 0) {
    let obj$1 = unsafe_pop(queue);
    if (typeof obj$1 === "number") {
      let u$1 = obj$1 | 0;
      s = Caml_hash_primitive.hash_mix_int(s, (u$1 + u$1 | 0) + 1 | 0);
      num = num - 1 | 0;
    } else if (typeof obj$1 === "string") {
      s = Caml_hash_primitive.hash_mix_string(s, obj$1);
      num = num - 1 | 0;
    } else if (typeof obj$1 !== "boolean" && typeof obj$1 !== "undefined" && typeof obj$1 !== "symbol" && typeof obj$1 !== "function") {
      let size = obj$1.length | 0;
      if (size !== 0) {
        let obj_tag = obj$1.TAG;
        let tag = (size << 10) | obj_tag;
        if (obj_tag === 248) {
          s = Caml_hash_primitive.hash_mix_int(s, obj$1[1]);
        } else {
          s = Caml_hash_primitive.hash_mix_int(s, tag);
          let v = size - 1 | 0;
          let block = v < num ? v : num;
          for (let i = 0; i <= block; ++i) {
            push_back(queue, obj$1[i]);
          }
        }
      } else {
        let size$1 = (function(obj,cb){
            var size = 0  
            for(var k in obj){
              cb(obj[k])
              ++ size
            }
            return size
          })(obj$1, v => push_back(queue, v));
        s = Caml_hash_primitive.hash_mix_int(s, (size$1 << 10) | 0);
      }
    }
    
  };
  return Caml_hash_primitive.hash_final_mix(s);
}

export {
  hash,
}
/* No side effect */
