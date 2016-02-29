// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_sys                = require("../runtime/caml_sys");
var Pervasives              = require("./pervasives");
var Nativeint               = require("./nativeint");
var Int32                   = require("./int32");
var Digest                  = require("./digest");
var Int64                   = require("./int64");
var Caml_array              = require("../runtime/caml_array");
var $$Array                 = require("./array");
var Caml_curry              = require("../runtime/caml_curry");

function new_state() {
  return /* record */[
          Caml_array.caml_make_vect(55, 0),
          0
        ];
}

function assign(st1, st2) {
  $$Array.blit(st2[0], 0, st1[0], 0, 55);
  st1[1] = st2[1];
  return /* () */0;
}

function full_init(s, seed) {
  var combine = function (accu, x) {
    return Digest.string(accu + x);
  };
  var extract = function (d) {
    return d.charCodeAt(0) + (d.charCodeAt(1) << 8) + (d.charCodeAt(2) << 16) + (d.charCodeAt(3) << 24);
  };
  var seed$1 = seed.length ? seed : /* int array */[0];
  var l = seed$1.length;
  for(var i = 0; i<= 54; ++i){
    s[0][i] = i;
  }
  var accu = "x";
  for(var i$1 = 0 ,i_finish = 54 + Pervasives.max(55, l); i$1<= i_finish; ++i$1){
    var j = i$1 % 55;
    var k = i$1 % l;
    accu = combine(accu, seed$1[k]);
    s[0][j] = (s[0][j] ^ extract(accu)) & 1073741823;
  }
  s[1] = 0;
  return /* () */0;
}

function make(seed) {
  var result = new_state(/* () */0);
  full_init(result, seed);
  return result;
}

function make_self_init() {
  return make(Caml_sys.caml_sys_random_seed(/* () */0));
}

function copy(s) {
  var result = new_state(/* () */0);
  assign(result, s);
  return result;
}

function bits(s) {
  s[1] = (s[1] + 1) % 55;
  var curval = s[0][s[1]];
  var newval = s[0][(s[1] + 24) % 55] + (curval ^ (curval >>> 25) & 31);
  var newval30 = newval & 1073741823;
  s[0][s[1]] = newval30;
  return newval30;
}

function $$int(s, bound) {
  if (bound > 1073741823 || bound <= 0) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Random.int"
        ];
  }
  else {
    var s$1 = s;
    var n = bound;
    while(true) {
      var r = bits(s$1);
      var v = r % n;
      if (r - v > 1073741823 - n + 1) {
        continue ;
        
      }
      else {
        return v;
      }
    };
  }
}

function int32(s, bound) {
  if (bound <= 0) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Random.int32"
        ];
  }
  else {
    var s$1 = s;
    var n = bound;
    while(true) {
      var b1 = bits(s$1);
      var b2 = ((bits(s$1) & 1) << 30);
      var r = b1 | b2;
      var v = r % n;
      if ((r - v | 0) > ((Int32.max_int - n | 0) + 1 | 0)) {
        continue ;
        
      }
      else {
        return v;
      }
    };
  }
}

function int64(s, bound) {
  if (bound <= 0) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Random.int64"
        ];
  }
  else {
    var s$1 = s;
    var n = bound;
    while(true) {
      var b1 = bits(s$1);
      var b2 = (bits(s$1) << 30);
      var b3 = ((bits(s$1) & 7) << 60);
      var r = b1 | b2 | b3;
      var v = r % n;
      if ((r - v | 0) > ((Int64.max_int - n | 0) + 1 | 0)) {
        continue ;
        
      }
      else {
        return v;
      }
    };
  }
}

var nativeint = Nativeint.size === 32 ? function (s, bound) {
    return int32(s, bound);
  } : function (s, bound) {
    return int64(s, bound);
  };

function rawfloat(s) {
  var scale = 1073741824.0;
  var r1 = bits(s);
  var r2 = bits(s);
  return (r1 / scale + r2) / scale;
}

function $$float(s, bound) {
  return rawfloat(s) * bound;
}

function bool(s) {
  return +((bits(s) & 1) === 0);
}

var $$default = /* record */[
  /* array */[
    987910699,
    495797812,
    364182224,
    414272206,
    318284740,
    990407751,
    383018966,
    270373319,
    840823159,
    24560019,
    536292337,
    512266505,
    189156120,
    730249596,
    143776328,
    51606627,
    140166561,
    366354223,
    1003410265,
    700563762,
    981890670,
    913149062,
    526082594,
    1021425055,
    784300257,
    667753350,
    630144451,
    949649812,
    48546892,
    415514493,
    258888527,
    511570777,
    89983870,
    283659902,
    308386020,
    242688715,
    482270760,
    865188196,
    1027664170,
    207196989,
    193777847,
    619708188,
    671350186,
    149669678,
    257044018,
    87658204,
    558145612,
    183450813,
    28133145,
    901332182,
    710253903,
    510646120,
    652377910,
    409934019,
    801085050
  ],
  0
];

function bits$1() {
  return bits($$default);
}

function $$int$1(bound) {
  return $$int($$default, bound);
}

function int32$1(bound) {
  return int32($$default, bound);
}

function nativeint$1(bound) {
  return Caml_curry.app2(nativeint, $$default, bound);
}

function int64$1(bound) {
  return int64($$default, bound);
}

function $$float$1(scale) {
  return rawfloat($$default) * scale;
}

function bool$1() {
  return bool($$default);
}

function full_init$1(seed) {
  return full_init($$default, seed);
}

function init(seed) {
  return full_init($$default, /* int array */[seed]);
}

function self_init() {
  return full_init$1(Caml_sys.caml_sys_random_seed(/* () */0));
}

function get_state() {
  return copy($$default);
}

function set_state(s) {
  return assign($$default, s);
}

var State = [
  make,
  make_self_init,
  copy,
  bits,
  $$int,
  int32,
  nativeint,
  int64,
  $$float,
  bool
];

exports.init      = init;
exports.full_init = full_init$1;
exports.self_init = self_init;
exports.bits      = bits$1;
exports.$$int     = $$int$1;
exports.int32     = int32$1;
exports.nativeint = nativeint$1;
exports.int64     = int64$1;
exports.$$float   = $$float$1;
exports.bool      = bool$1;
exports.State     = State;
exports.get_state = get_state;
exports.set_state = set_state;
/* No side effect */
