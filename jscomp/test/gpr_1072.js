'use strict';


var u = {
  y: 3
};

var v_ice_cream3_0 = {
  flavor: "vanilla",
  num: 3
};

var v_ice_cream3_1 = {
  hd: {
    flavor: "x",
    num: 3
  },
  tl: {
    hd: {
      flavor: "vanilla",
      num: 3
    },
    tl: /* [] */0
  }
};

var v_ice_cream3 = {
  hd: v_ice_cream3_0,
  tl: v_ice_cream3_1
};

var v_ice_cream4_0 = {
  flavor: "vanilla",
  num: 3
};

var v_ice_cream4_1 = {
  hd: {
    flavor: "x",
    num: 3
  },
  tl: /* [] */0
};

var v_ice_cream4 = {
  hd: v_ice_cream4_0,
  tl: v_ice_cream4_1
};

var vv = {
  x: 3
};

var int_expect = {
  x: 0
};

var int_expect2 = {
  x: 0
};

var int_expects_0 = {};

var int_expects_1 = {
  hd: {
    x: 2
  },
  tl: {
    hd: {
      x: 3
    },
    tl: /* [] */0
  }
};

var int_expects = {
  hd: int_expects_0,
  tl: int_expects_1
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
  x: undefined,
  y: 3
};

var v_mk6 = {
  y: 3
};

var v_mk6_1 = {
  x: undefined,
  y: 3
};

var mk_u = {
  x: 0
};

var v_mk7_0 = {
  y: 3
};

var v_mk7_1 = {
  hd: {
    y: 2
  },
  tl: {
    hd: {
      y: 2
    },
    tl: /* [] */0
  }
};

var v_mk7 = {
  hd: v_mk7_0,
  tl: v_mk7_1
};

again("a", 3);

again(undefined, 3);

again(undefined, 3);

again(undefined, 3);

again2("a", 3);

again3(3);

again3(2);

var side_effect = {
  contents: 0
};

again4(undefined, undefined, 166);

again4(undefined, undefined, 167);

again4(undefined, undefined, 168);

again4(undefined, undefined, 169);

again4(undefined, undefined, 170);

again4((side_effect.contents = side_effect.contents + 1 | 0, undefined), undefined, 171);

again4((side_effect.contents = side_effect.contents + 1 | 0, undefined), (side_effect.contents = side_effect.contents - 1 | 0, undefined), 172);

again4(undefined, (side_effect.contents = side_effect.contents - 1 | 0, undefined), 173);

again4((side_effect.contents = side_effect.contents + 1 | 0, undefined), undefined, 174);

exports.u = u;
exports.v_ice_cream3 = v_ice_cream3;
exports.v_ice_cream4 = v_ice_cream4;
exports.vv = vv;
exports.int_expect = int_expect;
exports.int_expect2 = int_expect2;
exports.int_expects = int_expects;
exports.mk_ice = mk_ice;
exports.my_ice2 = my_ice2;
exports.my_ice3 = my_ice3;
exports.v_mk4 = v_mk4;
exports.v_mk5 = v_mk5;
exports.v_mk6 = v_mk6;
exports.v_mk6_1 = v_mk6_1;
exports.mk_u = mk_u;
exports.v_mk7 = v_mk7;
exports.side_effect = side_effect;
/*  Not a pure module */
