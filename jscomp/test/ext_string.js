'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Bytes                   = require("../../lib/js/bytes");
var Caml_exceptions         = require("../../lib/js/caml_exceptions");
var Caml_int32              = require("../../lib/js/caml_int32");
var Curry                   = require("../../lib/js/curry");
var Ext_bytes               = require("./ext_bytes");
var $$String                = require("../../lib/js/string");
var Caml_string             = require("../../lib/js/caml_string");
var List                    = require("../../lib/js/list");

function split_by($staropt$star, is_delim, str) {
  var keep_empty = $staropt$star ? $staropt$star[0] : /* false */0;
  var len = str.length;
  var _acc = /* [] */0;
  var _last_pos = len;
  var _pos = len - 1 | 0;
  while(true) {
    var pos = _pos;
    var last_pos = _last_pos;
    var acc = _acc;
    if (pos === -1) {
      if (last_pos === 0 && !keep_empty) {
        return acc;
      }
      else {
        return /* :: */[
                $$String.sub(str, 0, last_pos),
                acc
              ];
      }
    }
    else if (Curry._1(is_delim, Caml_string.get(str, pos))) {
      var new_len = (last_pos - pos | 0) - 1 | 0;
      if (new_len !== 0 || keep_empty) {
        var v = $$String.sub(str, pos + 1 | 0, new_len);
        _pos = pos - 1 | 0;
        _last_pos = pos;
        _acc = /* :: */[
          v,
          acc
        ];
        continue ;
        
      }
      else {
        _pos = pos - 1 | 0;
        _last_pos = pos;
        continue ;
        
      }
    }
    else {
      _pos = pos - 1 | 0;
      continue ;
      
    }
  };
}

function trim(s) {
  var i = 0;
  var j = s.length;
  while(function () {
        var u = s.charCodeAt(i);
        return +(i < j && (u === /* "\t" */9 || u === /* "\n" */10 || u === /* " " */32));
      }()) {
    i = i + 1 | 0;
  };
  var k = j - 1 | 0;
  while(function () {
        var u = s.charCodeAt(k);
        return +(k >= i && (u === /* "\t" */9 || u === /* "\n" */10 || u === /* " " */32));
      }()) {
    k = k - 1 | 0;
  };
  return $$String.sub(s, i, (k - i | 0) + 1 | 0);
}

function split(keep_empty, str, on) {
  if (str === "") {
    return /* [] */0;
  }
  else {
    return split_by(keep_empty, function (x) {
                return +(x === on);
              }, str);
  }
}

function quick_split_by_ws(str) {
  return split_by(/* Some */[/* false */0], function (x) {
              if (x === /* "\t" */9 || x === /* "\n" */10) {
                return /* true */1;
              }
              else {
                return +(x === /* " " */32);
              }
            }, str);
}

function starts_with(s, beg) {
  var beg_len = beg.length;
  var s_len = s.length;
  if (beg_len <= s_len) {
    var i = 0;
    while(i < beg_len && s[i] === beg[i]) {
      i = i + 1 | 0;
    };
    return +(i === beg_len);
  }
  else {
    return /* false */0;
  }
}

function ends_with_index(s, end_) {
  var s_finish = s.length - 1 | 0;
  var s_beg = end_.length - 1 | 0;
  if (s_beg > s_finish) {
    return -1;
  }
  else {
    var _j = s_finish;
    var _k = s_beg;
    while(true) {
      var k = _k;
      var j = _j;
      if (k < 0) {
        return j + 1 | 0;
      }
      else if (s[j] === end_[k]) {
        _k = k - 1 | 0;
        _j = j - 1 | 0;
        continue ;
        
      }
      else {
        return -1;
      }
    };
  }
}

function ends_with(s, end_) {
  return +(ends_with_index(s, end_) >= 0);
}

function ends_with_then_chop(s, beg) {
  var i = ends_with_index(s, beg);
  if (i >= 0) {
    return /* Some */[$$String.sub(s, 0, i)];
  }
  else {
    return /* None */0;
  }
}

function check_any_suffix_case(s, suffixes) {
  return List.exists(function (x) {
              return ends_with(s, x);
            }, suffixes);
}

function check_any_suffix_case_then_chop(s, suffixes) {
  var _suffixes = suffixes;
  while(true) {
    var suffixes$1 = _suffixes;
    if (suffixes$1) {
      var id = ends_with_index(s, suffixes$1[0]);
      if (id >= 0) {
        return /* Some */[$$String.sub(s, 0, id)];
      }
      else {
        _suffixes = suffixes$1[1];
        continue ;
        
      }
    }
    else {
      return /* None */0;
    }
  };
}

function escaped(s) {
  var needs_escape = function (_i) {
    while(true) {
      var i = _i;
      if (i >= s.length) {
        return /* false */0;
      }
      else {
        var match = s.charCodeAt(i);
        if (match >= 32) {
          var switcher = match - 34 | 0;
          if (switcher > 58 || switcher < 0) {
            if (switcher >= 93) {
              return /* true */1;
            }
            else {
              _i = i + 1 | 0;
              continue ;
              
            }
          }
          else if (switcher > 57 || switcher < 1) {
            return /* true */1;
          }
          else {
            _i = i + 1 | 0;
            continue ;
            
          }
        }
        else {
          return /* true */1;
        }
      }
    };
  };
  if (needs_escape(0)) {
    return Caml_string.bytes_to_string(Ext_bytes.escaped(Caml_string.bytes_of_string(s)));
  }
  else {
    return s;
  }
}

function unsafe_for_all_range(s, start, finish, p) {
  var s$1 = s;
  var _start = start;
  var finish$1 = finish;
  var p$1 = Curry.__1(p);
  while(true) {
    var start$1 = _start;
    if (start$1 > finish$1) {
      return /* true */1;
    }
    else if (p$1(s$1.charCodeAt(start$1))) {
      _start = start$1 + 1 | 0;
      continue ;
      
    }
    else {
      return /* false */0;
    }
  };
}

function for_all_range(s, start, finish, p) {
  var len = s.length;
  if (start < 0 || finish >= len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_string.for_all_range"
        ];
  }
  else {
    return unsafe_for_all_range(s, start, finish, p);
  }
}

function for_all(p, s) {
  return unsafe_for_all_range(s, 0, s.length - 1 | 0, p);
}

function is_empty(s) {
  return +(s.length === 0);
}

function repeat(n, s) {
  var len = s.length;
  var res = Caml_string.caml_create_string(Caml_int32.imul(n, len));
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    $$String.blit(s, 0, res, Caml_int32.imul(i, len), len);
  }
  return Bytes.to_string(res);
}

function unsafe_is_sub(sub, i, s, j, len) {
  if ((j + len | 0) <= s.length) {
    var _k = 0;
    while(true) {
      var k = _k;
      if (k === len) {
        return /* true */1;
      }
      else if (sub[i + k | 0] === s[j + k | 0]) {
        _k = k + 1 | 0;
        continue ;
        
      }
      else {
        return /* false */0;
      }
    };
  }
  else {
    return /* false */0;
  }
}

var Local_exit = Caml_exceptions.create("Ext_string.Local_exit");

function find($staropt$star, sub, s) {
  var start = $staropt$star ? $staropt$star[0] : 0;
  var n = sub.length;
  var i = start;
  try {
    while((i + n | 0) <= s.length) {
      if (unsafe_is_sub(sub, 0, s, i, n)) {
        throw Local_exit;
      }
      i = i + 1 | 0;
    };
    return -1;
  }
  catch (exn){
    if (exn === Local_exit) {
      return i;
    }
    else {
      throw exn;
    }
  }
}

function rfind(sub, s) {
  var n = sub.length;
  var i = s.length - n | 0;
  try {
    while(i >= 0) {
      if (unsafe_is_sub(sub, 0, s, i, n)) {
        throw Local_exit;
      }
      i = i - 1 | 0;
    };
    return -1;
  }
  catch (exn){
    if (exn === Local_exit) {
      return i;
    }
    else {
      throw exn;
    }
  }
}

function tail_from(s, x) {
  var len = s.length;
  if (x > len) {
    var s$1 = "Ext_string.tail_from " + (s + (" : " + x));
    throw [
          Caml_builtin_exceptions.invalid_argument,
          s$1
        ];
  }
  else {
    return $$String.sub(s, x, len - x | 0);
  }
}

function digits_of_str(s, offset, x) {
  var _i = 0;
  var _acc = 0;
  var s$1 = s;
  var x$1 = x;
  while(true) {
    var acc = _acc;
    var i = _i;
    if (i >= x$1) {
      return acc;
    }
    else {
      _acc = (Caml_int32.imul(10, acc) + Caml_string.get(s$1, offset + i | 0) | 0) - 48 | 0;
      _i = i + 1 | 0;
      continue ;
      
    }
  };
}

function starts_with_and_number(s, offset, beg) {
  var beg_len = beg.length;
  var s_len = s.length;
  var finish_delim = offset + beg_len | 0;
  if (finish_delim > s_len) {
    return -1;
  }
  else {
    var i = offset;
    while(i < finish_delim && s[i] === beg[i - offset | 0]) {
      i = i + 1 | 0;
    };
    if (i === finish_delim) {
      return digits_of_str(s, finish_delim, 2);
    }
    else {
      return -1;
    }
  }
}

function equal(x, y) {
  return +(x === y);
}

function unsafe_concat_with_length(len, sep, l) {
  if (l) {
    var hd = l[0];
    var r = Caml_string.caml_create_string(len);
    var hd_len = hd.length;
    var sep_len = sep.length;
    Caml_string.caml_blit_string(hd, 0, r, 0, hd_len);
    var pos = [hd_len];
    List.iter(function (s) {
          var s_len = s.length;
          Caml_string.caml_blit_string(sep, 0, r, pos[0], sep_len);
          pos[0] = pos[0] + sep_len | 0;
          Caml_string.caml_blit_string(s, 0, r, pos[0], s_len);
          pos[0] = pos[0] + s_len | 0;
          return /* () */0;
        }, l[1]);
    return Caml_string.bytes_to_string(r);
  }
  else {
    return "";
  }
}

function rindex_rec(s, _i, c) {
  while(true) {
    var i = _i;
    if (i < 0) {
      return i;
    }
    else if (s.charCodeAt(i) === c) {
      return i;
    }
    else {
      _i = i - 1 | 0;
      continue ;
      
    }
  };
}

function rindex_rec_opt(s, _i, c) {
  while(true) {
    var i = _i;
    if (i < 0) {
      return /* None */0;
    }
    else if (s.charCodeAt(i) === c) {
      return /* Some */[i];
    }
    else {
      _i = i - 1 | 0;
      continue ;
      
    }
  };
}

function rindex_neg(s, c) {
  return rindex_rec(s, s.length - 1 | 0, c);
}

function rindex_opt(s, c) {
  return rindex_rec_opt(s, s.length - 1 | 0, c);
}

function is_valid_module_file(s) {
  var len = s.length;
  if (len > 0) {
    var match = s.charCodeAt(0);
    var exit = 0;
    if (match >= 91) {
      if (match > 122 || match < 97) {
        return /* false */0;
      }
      else {
        exit = 1;
      }
    }
    else if (match >= 65) {
      exit = 1;
    }
    else {
      return /* false */0;
    }
    if (exit === 1) {
      return unsafe_for_all_range(s, 1, len - 1 | 0, function (x) {
                  if (x >= 65) {
                    var switcher = x - 91 | 0;
                    if (switcher > 5 || switcher < 0) {
                      if (switcher >= 32) {
                        return /* false */0;
                      }
                      else {
                        return /* true */1;
                      }
                    }
                    else if (switcher !== 4) {
                      return /* false */0;
                    }
                    else {
                      return /* true */1;
                    }
                  }
                  else if (x >= 48) {
                    if (x >= 58) {
                      return /* false */0;
                    }
                    else {
                      return /* true */1;
                    }
                  }
                  else if (x !== 39) {
                    return /* false */0;
                  }
                  else {
                    return /* true */1;
                  }
                });
    }
    
  }
  else {
    return /* false */0;
  }
}

function is_valid_source_name(name) {
  var match = check_any_suffix_case_then_chop(name, /* :: */[
        ".ml",
        /* :: */[
          ".re",
          /* :: */[
            ".mli",
            /* :: */[
              ".mll",
              /* :: */[
                ".rei",
                /* [] */0
              ]
            ]
          ]
        ]
      ]);
  if (match) {
    return is_valid_module_file(match[0]);
  }
  else {
    return /* false */0;
  }
}

function unsafe_no_char(x, ch, _i, len) {
  while(true) {
    var i = _i;
    if (i > len) {
      return /* true */1;
    }
    else if (x.charCodeAt(i) !== ch) {
      _i = i + 1 | 0;
      continue ;
      
    }
    else {
      return /* false */0;
    }
  };
}

function no_char(x, ch, i, len) {
  var str_len = x.length;
  if (i < 0 || i >= str_len || len >= str_len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_string.no_char"
        ];
  }
  else {
    return unsafe_no_char(x, ch, i, len);
  }
}

function no_slash(x) {
  return unsafe_no_char(x, /* "/" */47, 0, x.length - 1 | 0);
}

function replace_slash_backward(x) {
  var len = x.length;
  if (unsafe_no_char(x, /* "/" */47, 0, len - 1 | 0)) {
    return x;
  }
  else {
    return $$String.map(function (x) {
                if (x !== 47) {
                  return x;
                }
                else {
                  return /* "\\" */92;
                }
              }, x);
  }
}

function replace_backward_slash(x) {
  var len = x.length;
  if (unsafe_no_char(x, /* "\\" */92, 0, len - 1 | 0)) {
    return x;
  }
  else {
    return $$String.map(function (x) {
                if (x !== 92) {
                  return x;
                }
                else {
                  return /* "/" */47;
                }
              }, x);
  }
}

var check_suffix_case = ends_with;

var check_suffix_case_then_chop = ends_with_then_chop;

var empty = "";

exports.split_by                        = split_by;
exports.trim                            = trim;
exports.split                           = split;
exports.quick_split_by_ws               = quick_split_by_ws;
exports.starts_with                     = starts_with;
exports.ends_with_index                 = ends_with_index;
exports.ends_with                       = ends_with;
exports.ends_with_then_chop             = ends_with_then_chop;
exports.check_suffix_case               = check_suffix_case;
exports.check_suffix_case_then_chop     = check_suffix_case_then_chop;
exports.check_any_suffix_case           = check_any_suffix_case;
exports.check_any_suffix_case_then_chop = check_any_suffix_case_then_chop;
exports.escaped                         = escaped;
exports.unsafe_for_all_range            = unsafe_for_all_range;
exports.for_all_range                   = for_all_range;
exports.for_all                         = for_all;
exports.is_empty                        = is_empty;
exports.repeat                          = repeat;
exports.unsafe_is_sub                   = unsafe_is_sub;
exports.Local_exit                      = Local_exit;
exports.find                            = find;
exports.rfind                           = rfind;
exports.tail_from                       = tail_from;
exports.digits_of_str                   = digits_of_str;
exports.starts_with_and_number          = starts_with_and_number;
exports.equal                           = equal;
exports.unsafe_concat_with_length       = unsafe_concat_with_length;
exports.rindex_rec                      = rindex_rec;
exports.rindex_rec_opt                  = rindex_rec_opt;
exports.rindex_neg                      = rindex_neg;
exports.rindex_opt                      = rindex_opt;
exports.is_valid_module_file            = is_valid_module_file;
exports.is_valid_source_name            = is_valid_source_name;
exports.unsafe_no_char                  = unsafe_no_char;
exports.no_char                         = no_char;
exports.no_slash                        = no_slash;
exports.replace_slash_backward          = replace_slash_backward;
exports.replace_backward_slash          = replace_backward_slash;
exports.empty                           = empty;
/* No side effect */
