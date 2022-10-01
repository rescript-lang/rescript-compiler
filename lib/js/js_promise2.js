'use strict';

var Js_promise = require("./js_promise.js");

var then_ = (function(p, cont) {
    Promise.resolve(p).then(cont)
  });

var catch_ = (function(p, cont) {
      Promise.resolve(p).catch(cont)
    });

var $$catch = Js_promise.$$catch;

exports.$$catch = $$catch;
exports.then_ = then_;
exports.catch_ = catch_;
/* No side effect */
