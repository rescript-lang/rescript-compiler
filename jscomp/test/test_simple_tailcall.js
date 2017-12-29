'use strict';


function tailcall() {
  while(true) {
    continue ;
    
  };
}

function non_length(x) {
  if (x) {
    return 1 + non_length(x[1]) | 0;
  } else {
    return 0;
  }
}

function length(_acc, _x) {
  while(true) {
    var x = _x;
    var acc = _acc;
    if (x) {
      var tl = x[1];
      if (tl) {
        return 1 + length(acc + 1 | 0, tl[1]) | 0;
      } else {
        _x = tl;
        _acc = acc + 1 | 0;
        continue ;
        
      }
    } else {
      return acc;
    }
  };
}

exports.tailcall = tailcall;
exports.non_length = non_length;
exports.length = length;
/* No side effect */
