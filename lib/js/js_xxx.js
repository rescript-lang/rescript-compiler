'use strict';

var Js_promise = require("./js_promise.js");

var then = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var $$catch = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var then_ = Js_promise.then_;

exports.then_ = then_;
exports.then = then;
exports.$$catch = $$catch;
/* No side effect */
