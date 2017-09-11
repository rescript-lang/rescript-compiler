'use strict';


function say(x, y) {
  return x + y | 0;
}

exports.say = say;
/* No side effect */
