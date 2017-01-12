'use strict';


var u = {
  y: 3
};

var vv = {
  x: 3
};

var v = ice_cream3("vanilla", 3);

var int_expect = {
  x: 0
};

var int_expect2 = {
  x: 0
};

var mk_ice = {
  flavour: "vanilla",
  num: 3
};

var my_ice2 = {
  flavour: "vanilla",
  num: 1
};

var my_ice3 = {
  num: 2
};

var v_mk4 = {
  y: 3
};

var v_mk5 = {
  y: 3
};

var v_mk6 = {
  y: 3
};

var v_mk6_1 = {
  x: /* () */0,
  y: 3
};

var mk_u = mk(0);

again("a", 3);

again2("a", 3);

again3(3);

var side_effect = [0];

again4(/* () */0, /* () */0);

again4(/* () */0, /* () */0);

side_effect[0] = side_effect[0] + 1 | 0;

again4(/* () */0, /* () */0);

side_effect[0] = side_effect[0] + 1 | 0;

side_effect[0] = side_effect[0] - 1 | 0;

again4(/* () */0, /* () */0);

exports.u           = u;
exports.vv          = vv;
exports.v           = v;
exports.int_expect  = int_expect;
exports.int_expect2 = int_expect2;
exports.mk_ice      = mk_ice;
exports.my_ice2     = my_ice2;
exports.my_ice3     = my_ice3;
exports.v_mk4       = v_mk4;
exports.v_mk5       = v_mk5;
exports.v_mk6       = v_mk6;
exports.v_mk6_1     = v_mk6_1;
exports.mk_u        = mk_u;
exports.side_effect = side_effect;
/* u Not a pure module */
