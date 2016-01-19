// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var $$String = require("../stdlib/string");

function generic_basename(is_dir_sep, current_dir_name, name) {
  var find_end = function (_n) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      else {
        if (is_dir_sep(name, n)) {
          _n = n - 1;
        }
        else {
          return find_beg(n, n + 1);
        }
      }
    };
  };
  var find_beg = function (_n, p) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, p);
      }
      else {
        if (is_dir_sep(name, n)) {
          return $$String.sub(name, n + 1, p - n - 1);
        }
        else {
          _n = n - 1;
        }
      }
    };
  };
  if (name === "") {
    return current_dir_name;
  }
  else {
    return find_end(name.length - 1);
  }
}

exports.generic_basename = generic_basename;
/* No side effect */
