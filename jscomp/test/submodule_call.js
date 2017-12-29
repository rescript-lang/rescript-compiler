'use strict';

var Curry = require("../../lib/js/curry.js");
var Submodule = require("./submodule.js");

var a0 = Submodule.A0[/* a0 */0](1, 2);

var a1 = Curry._2(Submodule.A0[/* A1 */1][/* a1 */0], 1, 2);

var a2 = Curry._2(Submodule.A0[/* A1 */1][/* A2 */1][/* a2 */0], 1, 2);

var a3 = Curry._2(Submodule.A0[/* A1 */1][/* A2 */1][/* A3 */1][/* a3 */0], 1, 2);

var a4 = Curry._2(Submodule.A0[/* A1 */1][/* A2 */1][/* A3 */1][/* A4 */1][/* a4 */0], 1, 2);

exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
/* a0 Not a pure module */
