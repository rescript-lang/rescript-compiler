

import * as Caml_primitive from "./caml_primitive.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

var for_in = (function(o,foo){
        for (var x in o) { foo(x) }});

function caml_obj_block(tag, size) {
  var v = new Array(size);
  v.tag = tag;
  return v;
}

var caml_obj_dup = (function(x){
  if(Array.isArray(x)){
    var len = x.length  
    var v = new Array(len)
    for(var i = 0 ; i < len ; ++i){
      v[i] = x[i]
    }
    if(x.tag !== undefined){
      v.tag = x.tag 
    }  
    return v 
  } 
  return Object.assign({},x)    
});

function caml_obj_truncate(x, new_size) {
  var len = x.length | 0;
  if (new_size <= 0 || new_size > len) {
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Obj.truncate"
        };
  }
  if (len === new_size) {
    return ;
  }
  for(var i = new_size; i < len; ++i){
    x[i] = 0;
  }
  x.length = new_size;
  
}

var caml_update_dummy = (function(x,y){
  var k  
  if(Array.isArray(y)){
    for(k = 0; k < y.length ; ++k){
      x[k] = y[k]
    }
    if(y.tag !== undefined){
      x.tag = y.tag
    }
  } else {
    for (var k in y){
      x[k] = y[k]
    }
  }
});

function caml_compare(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return 0;
    }
    var a_type = typeof a;
    var b_type = typeof b;
    switch (a_type) {
      case "boolean" :
          if (b_type === "boolean") {
            return Caml_primitive.caml_bool_compare(a, b);
          }
          break;
      case "function" :
          if (b_type === "function") {
            throw {
                  CamlExt: Caml_builtin_exceptions.invalid_argument,
                  _1: "compare: functional value"
                };
          }
          break;
      case "number" :
          if (b_type === "number") {
            return Caml_primitive.caml_int_compare(a, b);
          }
          break;
      case "string" :
          if (b_type === "string") {
            return Caml_primitive.caml_string_compare(a, b);
          } else {
            return 1;
          }
      case "undefined" :
          return -1;
      default:
        
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
          if (b === null || b.tag === 256) {
            return 1;
          } else {
            return -1;
          }
        }
        if (b_type === "number") {
          if (a === null || a.tag === 256) {
            return -1;
          } else {
            return 1;
          }
        }
        if (a === null) {
          if (b.tag === 256) {
            return 1;
          } else {
            return -1;
          }
        }
        if (b === null) {
          if (a.tag === 256) {
            return -1;
          } else {
            return 1;
          }
        }
        var tag_a = a.tag | 0;
        var tag_b = b.tag | 0;
        if (tag_a === 250) {
          _a = a[0];
          continue ;
        }
        if (tag_b === 250) {
          _b = b[0];
          continue ;
        }
        if (tag_a === 256) {
          if (tag_b === 256) {
            return Caml_primitive.caml_int_compare(a[1], b[1]);
          } else {
            return -1;
          }
        }
        if (tag_a === 248) {
          return Caml_primitive.caml_int_compare(a[1], b[1]);
        }
        if (tag_a === 251) {
          throw {
                CamlExt: Caml_builtin_exceptions.invalid_argument,
                _1: "equal: abstract value"
              };
        }
        if (tag_a !== tag_b) {
          if (tag_a < tag_b) {
            return -1;
          } else {
            return 1;
          }
        }
        var len_a = a.length | 0;
        var len_b = b.length | 0;
        if (len_a === len_b) {
          if (Array.isArray(a)) {
            var _i = 0;
            while(true) {
              var i = _i;
              if (i === len_a) {
                return 0;
              }
              var res = caml_compare(a[i], b[i]);
              if (res !== 0) {
                return res;
              }
              _i = i + 1 | 0;
              continue ;
            };
          } else if ((a instanceof Date && b instanceof Date)) {
            return (a - b);
          } else {
            var min_key_lhs = {
              contents: undefined
            };
            var min_key_rhs = {
              contents: undefined
            };
            var do_key = function (param, key) {
              var min_key = param[2];
              var b = param[1];
              if (!(!b.hasOwnProperty(key) || caml_compare(param[0][key], b[key]) > 0)) {
                return ;
              }
              var mk = min_key.contents;
              if (mk !== undefined && key >= mk) {
                return ;
              } else {
                min_key.contents = key;
                return ;
              }
            };
            var partial_arg = /* tuple */[
              a,
              b,
              min_key_rhs
            ];
            var do_key_a = (function(partial_arg){
            return function do_key_a(param) {
              return do_key(partial_arg, param);
            }
            }(partial_arg));
            var partial_arg$1 = /* tuple */[
              b,
              a,
              min_key_lhs
            ];
            var do_key_b = (function(partial_arg$1){
            return function do_key_b(param) {
              return do_key(partial_arg$1, param);
            }
            }(partial_arg$1));
            for_in(a, do_key_a);
            for_in(b, do_key_b);
            var match = min_key_lhs.contents;
            var match$1 = min_key_rhs.contents;
            if (match !== undefined) {
              if (match$1 !== undefined) {
                return Caml_primitive.caml_string_compare(match, match$1);
              } else {
                return -1;
              }
            } else if (match$1 !== undefined) {
              return 1;
            } else {
              return 0;
            }
          }
        } else if (len_a < len_b) {
          var _i$1 = 0;
          while(true) {
            var i$1 = _i$1;
            if (i$1 === len_a) {
              return -1;
            }
            var res$1 = caml_compare(a[i$1], b[i$1]);
            if (res$1 !== 0) {
              return res$1;
            }
            _i$1 = i$1 + 1 | 0;
            continue ;
          };
        } else {
          var _i$2 = 0;
          while(true) {
            var i$2 = _i$2;
            if (i$2 === len_b) {
              return 1;
            }
            var res$2 = caml_compare(a[i$2], b[i$2]);
            if (res$2 !== 0) {
              return res$2;
            }
            _i$2 = i$2 + 1 | 0;
            continue ;
          };
        }
    }
  };
}

function caml_equal(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return true;
    }
    var a_type = typeof a;
    if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a === null) {
      return false;
    }
    var b_type = typeof b;
    if (a_type === "function" || b_type === "function") {
      throw {
            CamlExt: Caml_builtin_exceptions.invalid_argument,
            _1: "equal: functional value"
          };
    }
    if (b_type === "number" || b_type === "undefined" || b === null) {
      return false;
    }
    var tag_a = a.tag | 0;
    var tag_b = b.tag | 0;
    if (tag_a === 250) {
      _a = a[0];
      continue ;
    }
    if (tag_b === 250) {
      _b = b[0];
      continue ;
    }
    if (tag_a === 248) {
      return a[1] === b[1];
    }
    if (tag_a === 251) {
      throw {
            CamlExt: Caml_builtin_exceptions.invalid_argument,
            _1: "equal: abstract value"
          };
    }
    if (tag_a !== tag_b) {
      return false;
    }
    if (tag_a === 256) {
      return a[1] === b[1];
    }
    var len_a = a.length | 0;
    var len_b = b.length | 0;
    if (len_a === len_b) {
      if (Array.isArray(a)) {
        var _i = 0;
        while(true) {
          var i = _i;
          if (i === len_a) {
            return true;
          }
          if (!caml_equal(a[i], b[i])) {
            return false;
          }
          _i = i + 1 | 0;
          continue ;
        };
      } else if ((a instanceof Date && b instanceof Date)) {
        return !(a > b || a < b);
      } else {
        var result = {
          contents: true
        };
        var do_key_a = (function(b,result){
        return function do_key_a(key) {
          if (!b.hasOwnProperty(key)) {
            result.contents = false;
            return ;
          }
          
        }
        }(b,result));
        var do_key_b = (function(a,b,result){
        return function do_key_b(key) {
          if (!a.hasOwnProperty(key) || !caml_equal(b[key], a[key])) {
            result.contents = false;
            return ;
          }
          
        }
        }(a,b,result));
        for_in(a, do_key_a);
        if (result.contents) {
          for_in(b, do_key_b);
        }
        return result.contents;
      }
    } else {
      return false;
    }
  };
}

function caml_equal_null(x, y) {
  if (y !== null) {
    return caml_equal(x, y);
  } else {
    return x === y;
  }
}

function caml_equal_undefined(x, y) {
  if (y !== undefined) {
    return caml_equal(x, y);
  } else {
    return x === y;
  }
}

function caml_equal_nullable(x, y) {
  if (y == null) {
    return x === y;
  } else {
    return caml_equal(x, y);
  }
}

function caml_notequal(a, b) {
  return !caml_equal(a, b);
}

function caml_greaterequal(a, b) {
  return caml_compare(a, b) >= 0;
}

function caml_greaterthan(a, b) {
  return caml_compare(a, b) > 0;
}

function caml_lessequal(a, b) {
  return caml_compare(a, b) <= 0;
}

function caml_lessthan(a, b) {
  return caml_compare(a, b) < 0;
}

function caml_min(x, y) {
  if (caml_compare(x, y) <= 0) {
    return x;
  } else {
    return y;
  }
}

function caml_max(x, y) {
  if (caml_compare(x, y) >= 0) {
    return x;
  } else {
    return y;
  }
}

function caml_obj_set_tag(prim, prim$1) {
  prim.tag = prim$1;
  
}

export {
  caml_obj_block ,
  caml_obj_dup ,
  caml_obj_truncate ,
  caml_update_dummy ,
  caml_compare ,
  caml_equal ,
  caml_equal_null ,
  caml_equal_undefined ,
  caml_equal_nullable ,
  caml_notequal ,
  caml_greaterequal ,
  caml_greaterthan ,
  caml_lessthan ,
  caml_lessequal ,
  caml_min ,
  caml_max ,
  caml_obj_set_tag ,
  
}
/* No side effect */
