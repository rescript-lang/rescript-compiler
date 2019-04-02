'use strict';


function indexOf(s, searchValue) {
  var value = s.indexOf(searchValue);
  if (value !== -1) {
    return value;
  }
  
}

exports.indexOf = indexOf;
/* No side effect */
