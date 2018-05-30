'use strict';

var Block = require("./block.js");
var Caml_primitive = require("./caml_primitive.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function caml_obj_block(tag, size) {
  var v = new Array(size);
  v.tag = tag;
  return v;
}

function caml_obj_dup(x) {
  var len = x.length | 0;
  var v = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    v[i] = x[i];
  }
  v.tag = x.tag | 0;
  return v;
}

function caml_obj_truncate(x, new_size) {
  var len = x.length | 0;
  if (new_size <= 0 || new_size > len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Obj.truncate"
        ];
  } else if (len !== new_size) {
    for(var i = new_size ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      x[i] = 0;
    }
    x.length = new_size;
    return /* () */0;
  } else {
    return 0;
  }
}

function caml_lazy_make_forward(x) {
  return Block.__(250, [x]);
}

function caml_update_dummy(x, y) {
  var len = y.length | 0;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    x[i] = y[i];
  }
  var y_tag = y.tag | 0;
  if (y_tag !== 0) {
    x.tag = y_tag;
    return /* () */0;
  } else {
    return 0;
  }
}

var for_in = function (o,foo){
        for (var x in o) { foo(x) }
      };

function caml_compare(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return 0;
    } else if (a === null) {
      return -1;
    } else if (b === null) {
      return 1;
    } else if (a === undefined) {
      return -1;
    } else if (b === undefined) {
      return 1;
    } else {
      var a_type = typeof a;
      var b_type = typeof b;
      if (a_type === "string") {
        return Caml_primitive.caml_string_compare(a, b);
      } else {
        var is_a_number = a_type === "number";
        var is_b_number = b_type === "number";
        if (is_a_number) {
          if (is_b_number) {
            return Caml_primitive.caml_int_compare(a, b);
          } else {
            return -1;
          }
        } else if (is_b_number) {
          return 1;
        } else if (a_type === "boolean") {
          var x = a;
          var y = b;
          if (x === y) {
            return 0;
          } else if (x < y) {
            return -1;
          } else {
            return 1;
          }
        } else if (a_type === "function" || b_type === "function") {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "compare: functional value"
              ];
        } else {
          var tag_a = a.tag | 0;
          var tag_b = b.tag | 0;
          if (tag_a === 250) {
            _a = a[0];
            continue ;
          } else if (tag_b === 250) {
            _b = b[0];
            continue ;
          } else if (tag_a === 248) {
            return Caml_primitive.caml_int_compare(a[1], b[1]);
          } else if (tag_a === 251) {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "equal: abstract value"
                ];
          } else if (tag_a !== tag_b) {
            if (tag_a < tag_b) {
              return -1;
            } else {
              return 1;
            }
          } else {
            var len_a = a.length | 0;
            var len_b = b.length | 0;
            if (len_a === len_b) {
              if (Array.isArray(a)) {
                var a$1 = a;
                var b$1 = b;
                var _i = 0;
                var same_length = len_a;
                while(true) {
                  var i = _i;
                  if (i === same_length) {
                    return 0;
                  } else {
                    var res = caml_compare(a$1[i], b$1[i]);
                    if (res !== 0) {
                      return res;
                    } else {
                      _i = i + 1 | 0;
                      continue ;
                    }
                  }
                };
              } else {
                var a$2 = a;
                var b$2 = b;
                var min_key_lhs = [/* None */0];
                var min_key_rhs = [/* None */0];
                var do_key = function (param, key) {
                  var min_key = param[2];
                  var b = param[1];
                  if (!b.hasOwnProperty(key) || caml_compare(param[0][key], b[key]) > 0) {
                    var match = min_key[0];
                    if (match) {
                      if (key < match[0]) {
                        min_key[0] = /* Some */[key];
                        return /* () */0;
                      } else {
                        return 0;
                      }
                    } else {
                      min_key[0] = /* Some */[key];
                      return /* () */0;
                    }
                  } else {
                    return 0;
                  }
                };
                var partial_arg = /* tuple */[
                  a$2,
                  b$2,
                  min_key_rhs
                ];
                var do_key_a = (function(partial_arg){
                return function do_key_a(param) {
                  return do_key(partial_arg, param);
                }
                }(partial_arg));
                var partial_arg$1 = /* tuple */[
                  b$2,
                  a$2,
                  min_key_lhs
                ];
                var do_key_b = (function(partial_arg$1){
                return function do_key_b(param) {
                  return do_key(partial_arg$1, param);
                }
                }(partial_arg$1));
                for_in(a$2, do_key_a);
                for_in(b$2, do_key_b);
                var match = min_key_lhs[0];
                var match$1 = min_key_rhs[0];
                if (match) {
                  if (match$1) {
                    return Caml_primitive.caml_string_compare(match[0], match$1[0]);
                  } else {
                    return -1;
                  }
                } else if (match$1) {
                  return 1;
                } else {
                  return 0;
                }
              }
            } else if (len_a < len_b) {
              var a$3 = a;
              var b$3 = b;
              var _i$1 = 0;
              var short_length = len_a;
              while(true) {
                var i$1 = _i$1;
                if (i$1 === short_length) {
                  return -1;
                } else {
                  var res$1 = caml_compare(a$3[i$1], b$3[i$1]);
                  if (res$1 !== 0) {
                    return res$1;
                  } else {
                    _i$1 = i$1 + 1 | 0;
                    continue ;
                  }
                }
              };
            } else {
              var a$4 = a;
              var b$4 = b;
              var _i$2 = 0;
              var short_length$1 = len_b;
              while(true) {
                var i$2 = _i$2;
                if (i$2 === short_length$1) {
                  return 1;
                } else {
                  var res$2 = caml_compare(a$4[i$2], b$4[i$2]);
                  if (res$2 !== 0) {
                    return res$2;
                  } else {
                    _i$2 = i$2 + 1 | 0;
                    continue ;
                  }
                }
              };
            }
          }
        }
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
    } else {
      var a_type = typeof a;
      if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a === null) {
        return false;
      } else {
        var b_type = typeof b;
        if (a_type === "function" || b_type === "function") {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "equal: functional value"
              ];
        } else if (b_type === "number" || b_type === "undefined" || b === null) {
          return false;
        } else {
          var tag_a = a.tag | 0;
          var tag_b = b.tag | 0;
          if (tag_a === 250) {
            _a = a[0];
            continue ;
          } else if (tag_b === 250) {
            _b = b[0];
            continue ;
          } else if (tag_a === 248) {
            return a[1] === b[1];
          } else if (tag_a === 251) {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "equal: abstract value"
                ];
          } else if (tag_a !== tag_b) {
            return false;
          } else {
            var len_a = a.length | 0;
            var len_b = b.length | 0;
            if (len_a === len_b) {
              if (Array.isArray(a)) {
                var a$1 = a;
                var b$1 = b;
                var _i = 0;
                var same_length = len_a;
                while(true) {
                  var i = _i;
                  if (i === same_length) {
                    return true;
                  } else if (caml_equal(a$1[i], b$1[i])) {
                    _i = i + 1 | 0;
                    continue ;
                  } else {
                    return false;
                  }
                };
              } else {
                var a$2 = a;
                var b$2 = b;
                var result = [true];
                var do_key_a = (function(b$2,result){
                return function do_key_a(key) {
                  if (b$2.hasOwnProperty(key)) {
                    return 0;
                  } else {
                    result[0] = false;
                    return /* () */0;
                  }
                }
                }(b$2,result));
                var do_key_b = (function(a$2,b$2,result){
                return function do_key_b(key) {
                  if (!a$2.hasOwnProperty(key) || !caml_equal(b$2[key], a$2[key])) {
                    result[0] = false;
                    return /* () */0;
                  } else {
                    return 0;
                  }
                }
                }(a$2,b$2,result));
                for_in(a$2, do_key_a);
                if (result[0]) {
                  for_in(b$2, do_key_b);
                }
                return result[0];
              }
            } else {
              return false;
            }
          }
        }
      }
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

exports.caml_obj_block = caml_obj_block;
exports.caml_obj_dup = caml_obj_dup;
exports.caml_obj_truncate = caml_obj_truncate;
exports.caml_lazy_make_forward = caml_lazy_make_forward;
exports.caml_update_dummy = caml_update_dummy;
exports.caml_compare = caml_compare;
exports.caml_equal = caml_equal;
exports.caml_equal_null = caml_equal_null;
exports.caml_equal_undefined = caml_equal_undefined;
exports.caml_equal_nullable = caml_equal_nullable;
exports.caml_notequal = caml_notequal;
exports.caml_greaterequal = caml_greaterequal;
exports.caml_greaterthan = caml_greaterthan;
exports.caml_lessthan = caml_lessthan;
exports.caml_lessequal = caml_lessequal;
exports.caml_min = caml_min;
exports.caml_max = caml_max;
/* No side effect */
