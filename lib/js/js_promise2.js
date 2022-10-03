'use strict';


var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var then_ = then;

exports.then = then;
exports.$$catch = $$catch;
exports.then_ = then_;
/* No side effect */
