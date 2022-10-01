'use strict';


var then_ = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

exports.then_ = then_;
exports.$$catch = $$catch;
/* No side effect */
