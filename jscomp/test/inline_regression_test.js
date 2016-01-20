// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var $$String = require("../stdlib/string");

function generic_basename(is_dir_sep, current_dir_name, name) {
  if (name === "") {
    return current_dir_name;
  }
  else {
    var _n = name.length - 1;
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
          var _n$1 = n;
          var p = n + 1;
          while(/* true */1) {
            var n$1 = _n$1;
            if (n$1 < 0) {
              return $$String.sub(name, 0, p);
            }
            else {
              if (is_dir_sep(name, n$1)) {
                return $$String.sub(name, n$1 + 1, p - n$1 - 1);
              }
              else {
                _n$1 = n$1 - 1;
              }
            }
          };
        }
      }
    };
  }
}

exports.generic_basename = generic_basename;
/* No side effect */
