'use strict';

var Curry = require("./curry.js");

function then_(arg1, obj) {
  return obj.then(Curry.__1(arg1));
}

function $$catch(arg1, obj) {
  return obj.catch(Curry.__1(arg1));
}

exports.then_ = then_;
exports.$$catch = $$catch;
/* No side effect */
