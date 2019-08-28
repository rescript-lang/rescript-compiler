'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

var u = /* record */{
  contents: 0
};

var v = Pervasives.incr(u);

exports.v = v;
/* v Not a pure module */
