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

function caml_compare(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return 0;
    } else {
      var a_type = typeof a;
      var b_type = typeof b;
      if (a_type === "string") {
        return Caml_primitive.caml_string_compare(a, b);
      } else {
        var is_a_number = +(a_type === "number");
        var is_b_number = +(b_type === "number");
        if (is_a_number !== 0) {
          if (is_b_number !== 0) {
            return Caml_primitive.caml_int_compare(a, b);
          } else {
            return -1;
          }
        } else if (is_b_number !== 0) {
          return 1;
        } else if (a_type === "boolean" || a_type === "undefined" || a === null) {
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
            if (len_a === 0 && len_b === 0 && a.constructor === Object && b.constructor === Object) {
              var keys_a = Object.keys(a);
              var keys_b = Object.keys(b);
              keys_a.sort();
              keys_b.sort();
              var len_a$1 = keys_a.length;
              var len_b$1 = keys_b.length;
              var min_len = len_a$1 < len_b$1 ? len_a$1 : len_b$1;
              var default_res = len_a$1 - len_b$1 | 0;
              var a$1 = a;
              var keys_a$1 = keys_a;
              var b$1 = b;
              var keys_b$1 = keys_b;
              var _i = 0;
              var min_len$1 = min_len;
              var default_res$1 = default_res;
              while(true) {
                var i = _i;
                if (i === min_len$1) {
                  return default_res$1;
                } else {
                  var key_a = keys_a$1[i];
                  var key_b = keys_b$1[i];
                  var res = caml_compare(key_a, key_b);
                  if (res !== 0) {
                    return res;
                  } else {
                    var res$1 = caml_compare(a$1[key_a], b$1[key_b]);
                    if (res$1 !== 0) {
                      return res$1;
                    } else {
                      _i = i + 1 | 0;
                      continue ;
                    }
                  }
                }
              };
            } else if (len_a === len_b) {
              var a$2 = a;
              var b$2 = b;
              var _i$1 = 0;
              var same_length = len_a;
              while(true) {
                var i$1 = _i$1;
                if (i$1 === same_length) {
                  return 0;
                } else {
                  var res$2 = caml_compare(a$2[i$1], b$2[i$1]);
                  if (res$2 !== 0) {
                    return res$2;
                  } else {
                    _i$1 = i$1 + 1 | 0;
                    continue ;
                  }
                }
              };
            } else if (len_a < len_b) {
              var a$3 = a;
              var b$3 = b;
              var _i$2 = 0;
              var short_length = len_a;
              while(true) {
                var i$2 = _i$2;
                if (i$2 === short_length) {
                  return -1;
                } else {
                  var res$3 = caml_compare(a$3[i$2], b$3[i$2]);
                  if (res$3 !== 0) {
                    return res$3;
                  } else {
                    _i$2 = i$2 + 1 | 0;
                    continue ;
                  }
                }
              };
            } else {
              var a$4 = a;
              var b$4 = b;
              var _i$3 = 0;
              var short_length$1 = len_b;
              while(true) {
                var i$3 = _i$3;
                if (i$3 === short_length$1) {
                  return 1;
                } else {
                  var res$4 = caml_compare(a$4[i$3], b$4[i$3]);
                  if (res$4 !== 0) {
                    return res$4;
                  } else {
                    _i$3 = i$3 + 1 | 0;
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
      return /* true */1;
    } else {
      var a_type = typeof a;
      if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a === null) {
        return /* false */0;
      } else {
        var b_type = typeof b;
        if (a_type === "function" || b_type === "function") {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "equal: functional value"
              ];
        } else if (b_type === "number" || b_type === "undefined" || b === null) {
          return /* false */0;
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
            return +(a[1] === b[1]);
          } else if (tag_a === 251) {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "equal: abstract value"
                ];
          } else if (tag_a !== tag_b) {
            return /* false */0;
          } else {
            var len_a = a.length | 0;
            var len_b = b.length | 0;
            if (len_a === 0 && len_b === 0 && a.constructor === Object && b.constructor === Object) {
              var keys_a = Object.keys(a);
              var keys_b = Object.keys(b);
              var len_a$1 = keys_a.length;
              var len_b$1 = keys_b.length;
              if (len_a$1 === len_b$1) {
                keys_a.sort();
                keys_b.sort();
                var a$1 = a;
                var keys_a$1 = keys_a;
                var b$1 = b;
                var keys_b$1 = keys_b;
                var _i = 0;
                var length = len_a$1;
                while(true) {
                  var i = _i;
                  if (i === length) {
                    return /* true */1;
                  } else {
                    var key_a = keys_a$1[i];
                    var key_b = keys_b$1[i];
                    if (caml_equal(key_a, key_b) && caml_equal(a$1[key_a], b$1[key_b])) {
                      _i = i + 1 | 0;
                      continue ;
                    } else {
                      return /* false */0;
                    }
                  }
                };
              } else {
                return /* false */0;
              }
            } else if (len_a === len_b) {
              var a$2 = a;
              var b$2 = b;
              var _i$1 = 0;
              var same_length = len_a;
              while(true) {
                var i$1 = _i$1;
                if (i$1 === same_length) {
                  return /* true */1;
                } else if (caml_equal(a$2[i$1], b$2[i$1])) {
                  _i$1 = i$1 + 1 | 0;
                  continue ;
                } else {
                  return /* false */0;
                }
              };
            } else {
              return /* false */0;
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
    return +(x === y);
  }
}

function caml_equal_undefined(x, y) {
  if (y !== undefined) {
    return caml_equal(x, y);
  } else {
    return +(x === y);
  }
}

function caml_equal_nullable(x, y) {
  if (y == null) {
    return +(x === y);
  } else {
    return caml_equal(x, y);
  }
}

function caml_notequal(a, b) {
  return 1 - caml_equal(a, b);
}

function caml_greaterequal(a, b) {
  return +(caml_compare(a, b) >= 0);
}

function caml_greaterthan(a, b) {
  return +(caml_compare(a, b) > 0);
}

function caml_lessequal(a, b) {
  return +(caml_compare(a, b) <= 0);
}

function caml_lessthan(a, b) {
  return +(caml_compare(a, b) < 0);
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
