// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var $$String = require("../stdlib/string");

function split_by($staropt$star, is_delim, str) {
  var keep_empty = $staropt$star ? $staropt$star[1] : /* false */0;
  var len = str.length;
  var loop = function (_acc, _last_pos, _pos) {
    while(/* true */1) {
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
  };
  return loop(/* [] */0, len, len - 1);
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
    var aux = function (_j, _k) {
      while(/* true */1) {
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
    };
    return aux(s_finish, s_beg);
  }
}

exports.split_by = split_by;
exports.split = split;
exports.starts_with = starts_with;
exports.ends_with = ends_with;
/* No side effect */
