

import * as Caml from "./caml.js";

let for_in = (function(o,foo){
        for (var x in o) { foo(x) }});

let obj_dup = (function(x){
  if(Array.isArray(x)){
    var len = x.length  
    var v = new Array(len)
    for(var i = 0 ; i < len ; ++i){
      v[i] = x[i]
    }
    if(x.TAG !== undefined){
      v.TAG = x.TAG // TODO this can be removed eventually
    }  
    return v 
  } 
  return Object.assign({},x)    
});

let update_dummy = (function(x,y){
  var k  
  if(Array.isArray(y)){
    for(k = 0; k < y.length ; ++k){
      x[k] = y[k]
    }
    if(y.TAG !== undefined){
      x.TAG = y.TAG
    }
  } else {
    for (var k in y){
      x[k] = y[k]
    }
  }
});

function compare(a, b) {
  if (a === b) {
    return 0;
  }
  let a_type = typeof a;
  let b_type = typeof b;
  switch (a_type) {
    case "bigint" :
      if (b_type === "bigint") {
        return Caml.float_compare(a, b);
      }
      break;
    case "boolean" :
      if (b_type === "boolean") {
        return Caml.bool_compare(a, b);
      }
      break;
    case "function" :
      if (b_type === "function") {
        throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "compare: functional value"
          }
        });
      }
      break;
    case "number" :
      if (b_type === "number") {
        return Caml.float_compare(a, b);
      }
      break;
    case "string" :
      if (b_type === "string") {
        return Caml.string_compare(a, b);
      } else {
        return 1;
      }
    case "undefined" :
      return -1;
  }
  switch (b_type) {
    case "string" :
      return -1;
    case "undefined" :
      return 1;
    default:
      if (a_type === "boolean") {
        return 1;
      }
      if (b_type === "boolean") {
        return -1;
      }
      if (a_type === "function") {
        return 1;
      }
      if (b_type === "function") {
        return -1;
      }
      if (a_type === "number") {
        if (b === null || b.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
          return 1;
        } else {
          return -1;
        }
      }
      if (b_type === "number") {
        if (a === null || a.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
          return -1;
        } else {
          return 1;
        }
      }
      if (a === null) {
        if (b.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
          return 1;
        } else {
          return -1;
        }
      }
      if (b === null) {
        if (a.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
          return -1;
        } else {
          return 1;
        }
      }
      if (a.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
        if (b.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
          return aux_obj_compare(a, b);
        } else {
          return -1;
        }
      }
      let tag_a = a.TAG;
      let tag_b = b.TAG;
      if (tag_a !== tag_b) {
        if (tag_a < tag_b) {
          return -1;
        } else {
          return 1;
        }
      }
      let len_a = a.length | 0;
      let len_b = b.length | 0;
      if (len_a === len_b) {
        if (Array.isArray(a)) {
          let _i = 0;
          while (true) {
            let i = _i;
            if (i === len_a) {
              return 0;
            }
            let res = compare(a[i], b[i]);
            if (res !== 0) {
              return res;
            }
            _i = i + 1 | 0;
            continue;
          };
        } else if ((a instanceof Date && b instanceof Date)) {
          return (a - b);
        } else {
          return aux_obj_compare(a, b);
        }
      } else if (len_a < len_b) {
        let _i$1 = 0;
        while (true) {
          let i$1 = _i$1;
          if (i$1 === len_a) {
            return -1;
          }
          let res$1 = compare(a[i$1], b[i$1]);
          if (res$1 !== 0) {
            return res$1;
          }
          _i$1 = i$1 + 1 | 0;
          continue;
        };
      } else {
        let _i$2 = 0;
        while (true) {
          let i$2 = _i$2;
          if (i$2 === len_b) {
            return 1;
          }
          let res$2 = compare(a[i$2], b[i$2]);
          if (res$2 !== 0) {
            return res$2;
          }
          _i$2 = i$2 + 1 | 0;
          continue;
        };
      }
  }
}

function aux_obj_compare(a, b) {
  let min_key_lhs = {
    contents: undefined
  };
  let min_key_rhs = {
    contents: undefined
  };
  let do_key = function (param, key) {
    let min_key = param[2];
    let b = param[1];
    if (!(!Object.prototype.hasOwnProperty.call(b, key) || compare(param[0][key], b[key]) > 0)) {
      return;
    }
    let mk = min_key.contents;
    if (mk !== undefined && key >= mk) {
      return;
    } else {
      min_key.contents = key;
      return;
    }
  };
  let do_key_a = function (key) {
    do_key([
      a,
      b,
      min_key_rhs
    ], key);
  };
  let do_key_b = function (key) {
    do_key([
      b,
      a,
      min_key_lhs
    ], key);
  };
  for_in(a, do_key_a);
  for_in(b, do_key_b);
  let match = min_key_lhs.contents;
  let match$1 = min_key_rhs.contents;
  if (match !== undefined) {
    if (match$1 !== undefined) {
      return Caml.string_compare(match, match$1);
    } else {
      return -1;
    }
  } else if (match$1 !== undefined) {
    return 1;
  } else {
    return 0;
  }
}

function equal(_a, _b) {
  while (true) {
    let b = _b;
    let a = _a;
    if (a === b) {
      return true;
    }
    let a_type = typeof a;
    if (a_type !== "object" || a === null) {
      return false;
    }
    let b_type = typeof b;
    if (b_type !== "object" || b === null) {
      return false;
    }
    let tag_a = a.TAG;
    let tag_b = b.TAG;
    if (tag_a !== tag_b) {
      return false;
    }
    if (Array.isArray(a)) {
      let len_a = a.length | 0;
      let len_b = b.length | 0;
      if (len_a !== len_b) {
        return false;
      } else {
        let _i = 0;
        while (true) {
          let i = _i;
          if (i === len_a) {
            return true;
          }
          if (!equal(a[i], b[i])) {
            return false;
          }
          _i = i + 1 | 0;
          continue;
        };
      }
    }
    if (!(a instanceof Error)) {
      if ((a instanceof Date)) {
        if ((b instanceof Date)) {
          return !(a > b || a < b);
        } else {
          return false;
        }
      } else {
        let result = {
          contents: true
        };
        let do_key_a = function (key) {
          if (!Object.prototype.hasOwnProperty.call(b, key)) {
            result.contents = false;
            return;
          }
          
        };
        let do_key_b = function (key) {
          if (!Object.prototype.hasOwnProperty.call(a, key) || !equal(b[key], a[key])) {
            result.contents = false;
            return;
          }
          
        };
        for_in(a, do_key_a);
        if (result.contents) {
          for_in(b, do_key_b);
        }
        return result.contents;
      }
    }
    if (!((b instanceof Error) && a.message === b.message)) {
      return false;
    }
    _b = b.cause;
    _a = a.cause;
    continue;
  };
}

function equal_null(x, y) {
  if (y !== null) {
    return equal(x, y);
  } else {
    return x === y;
  }
}

function equal_undefined(x, y) {
  if (y !== undefined) {
    return equal(x, y);
  } else {
    return x === y;
  }
}

function equal_nullable(x, y) {
  if (y == null) {
    return x === y;
  } else {
    return equal(x, y);
  }
}

function notequal(a, b) {
  if ((typeof a === "number" || typeof a === "bigint") && (typeof b === "number" || typeof b === "bigint")) {
    return a !== b;
  } else {
    return !equal(a, b);
  }
}

function greaterequal(a, b) {
  if ((typeof a === "number" || typeof a === "bigint") && (typeof b === "number" || typeof b === "bigint")) {
    return a >= b;
  } else {
    return compare(a, b) >= 0;
  }
}

function greaterthan(a, b) {
  if ((typeof a === "number" || typeof a === "bigint") && (typeof b === "number" || typeof b === "bigint")) {
    return a > b;
  } else {
    return compare(a, b) > 0;
  }
}

function lessequal(a, b) {
  if ((typeof a === "number" || typeof a === "bigint") && (typeof b === "number" || typeof b === "bigint")) {
    return a <= b;
  } else {
    return compare(a, b) <= 0;
  }
}

function lessthan(a, b) {
  if ((typeof a === "number" || typeof a === "bigint") && (typeof b === "number" || typeof b === "bigint")) {
    return a < b;
  } else {
    return compare(a, b) < 0;
  }
}

function min(x, y) {
  if (compare(x, y) <= 0) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (compare(x, y) >= 0) {
    return x;
  } else {
    return y;
  }
}

export {
  obj_dup,
  update_dummy,
  compare,
  equal,
  equal_null,
  equal_undefined,
  equal_nullable,
  notequal,
  greaterequal,
  greaterthan,
  lessthan,
  lessequal,
  min,
  max,
}
/* No side effect */
