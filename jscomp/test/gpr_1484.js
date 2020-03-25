'use strict';


function test(x) {
  x.nodeValue = null;
  
}

exports.test = test;
/* No side effect */
