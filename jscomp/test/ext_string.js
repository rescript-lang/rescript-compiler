// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Bytes       = require("../stdlib/bytes");
var Ext_bytes   = require("./ext_bytes");
var $$String    = require("../stdlib/string");
var Caml_string = require("../runtime/caml_string");

function split_by($staropt$star, is_delim, str) {
  var keep_empty = $staropt$star ? $staropt$star[1] : /* false */0;
  var len = str.length;
  var _acc = /* [] */0;
  var _last_pos = len;
  var _pos = len - 1;
  while(true) {
    var pos = _pos;
    var last_pos = _last_pos;
    var acc = _acc;
    if (pos === -1) {
      return [
              /* :: */0,
              $$String.sub(str, 0, last_pos),
              acc
            ];
    }
    else {
      if (is_delim(str.charCodeAt(pos))) {
        var new_len = last_pos - pos - 1;
        if (new_len !== 0 || keep_empty) {
          var v = $$String.sub(str, pos + 1, new_len);
          _pos = pos - 1;
          _last_pos = pos;
          _acc = [
            /* :: */0,
            v,
            acc
          ];
        }
        else {
          _pos = pos - 1;
          _last_pos = pos;
        }
      }
      else {
        _pos = pos - 1;
      }
    }
  };
}

function split(keep_empty, str, on) {
  return split_by(keep_empty, function (x) {
              return +(x === on);
            }, str);
}

function starts_with(s, beg) {
  var beg_len = beg.length;
  var s_len = s.length;
  var i = 0;
  while(i < beg_len && s[i] === beg[i]) {
    ++ i;
  };
  return +(beg_len <= s_len && i === beg_len);
}

function ends_with(s, beg) {
  var s_finish = s.length - 1;
  var s_beg = beg.length - 1;
  if (s_beg > s_finish) {
    return /* false */0;
  }
  else {
    var _j = s_finish;
    var _k = s_beg;
    while(true) {
      var k = _k;
      var j = _j;
      if (k < 0) {
        return /* true */1;
      }
      else {
        if (s[j] === beg[k]) {
          _k = k - 1;
          _j = j - 1;
        }
        else {
          return /* false */0;
        }
      }
    };
  }
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
          var switcher = -34 + match;
          if (!(58 < (switcher >>> 0))) {
            if (56 < (-1 + switcher >>> 0)) {
              return /* true */1;
            }
            else {
              _i = i + 1;
            }
          }
          else {
            if (switcher >= 93) {
              return /* true */1;
            }
            else {
              _i = i + 1;
            }
          }
        }
        else {
          if (match >= 11) {
            match !== 13;
            return /* true */1;
          }
          else {
            match >= 8;
            return /* true */1;
          }
        }
      }
    };
  };
  if (needs_escape(0)) {
    return Bytes.unsafe_to_string(Ext_bytes.escaped(Bytes.unsafe_of_string(s)));
  }
  else {
    return s;
  }
}

function for_all(p, s) {
  var len = s.length;
  var aux = function (i) {
    if (i >= len) {
      return /* true */1;
    }
    else {
      return +(p(s.charCodeAt(i)) && aux(i + 1));
    }
  };
  return aux(0);
}

function is_empty(s) {
  return +(s.length === 0);
}

function repeat(n, s) {
  var len = s.length;
  var res = Caml_string.caml_create_string(n * len);
  for(var i = 0 ,i_finish = -1 + n; i<= i_finish; ++i){
    $$String.blit(s, 0, res, i * len, len);
  }
  return Bytes.to_string(res);
}

exports.split_by    = split_by;
exports.split       = split;
exports.starts_with = starts_with;
exports.ends_with   = ends_with;
exports.escaped     = escaped;
exports.for_all     = for_all;
exports.is_empty    = is_empty;
exports.repeat      = repeat;
/* No side effect */
