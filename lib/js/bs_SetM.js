'use strict';

var Bs_internalSet = require("./bs_internalSet.js");

function add(m, e) {
  var dict = m.dict;
  var oldRoot = m.root;
  var newRoot = Bs_internalSet.addMutate(dict[/* cmp */0], oldRoot, e);
  if (newRoot !== oldRoot) {
    m.root = newRoot;
  }
  return m;
}

exports.add = add;
/* No side effect */
