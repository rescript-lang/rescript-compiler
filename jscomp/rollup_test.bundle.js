(function () {
'use strict';

/* No side effect */

/* No side effect */

/* No side effect */

var invalid_argument = /* tuple */[
  "Invalid_argument",
  -3
];


/* No side effect */

/* No side effect */

/* stdin Not a pure module */

/* imul Not a pure module */

/* repeat Not a pure module */

/* two_ptr_32_dbl Not a pure module */

/* No side effect */

/* float_of_string Not a pure module */

/* No side effect */

/* No side effect */

/* No side effect */

function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (param) {
      _param = param[1];
      _len = len + 1 | 0;
      continue ;
      
    }
    else {
      return len;
    }
  }
}


/* No side effect */

console.log(length([1,2,3]));

}());
