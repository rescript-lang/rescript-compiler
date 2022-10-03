'use strict';


var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

exports.then = then;
exports.$$catch = $$catch;
/* No side effect */
