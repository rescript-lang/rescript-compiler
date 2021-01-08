

import * as $$Array from "./array.mjs";
import * as Int32 from "./int32.mjs";
import * as Int64 from "./int64.mjs";
import * as Digest from "./digest.mjs";
import * as Caml_array from "./caml_array.mjs";
import * as Caml_int64 from "./caml_int64.mjs";
import * as Caml_string from "./caml_string.mjs";

function random_seed(param) {
  return [(Math.floor(Math.random()*0x7fffffff))];
}

function assign(st1, st2) {
  $$Array.blit(st2.st, 0, st1.st, 0, 55);
  st1.idx = st2.idx;
  
}

function full_init(s, seed) {
  var combine = function (accu, x) {
    return Digest.string(accu + String(x));
  };
  var extract = function (d) {
    return ((Caml_string.get(d, 0) + (Caml_string.get(d, 1) << 8) | 0) + (Caml_string.get(d, 2) << 16) | 0) + (Caml_string.get(d, 3) << 24) | 0;
  };
  var seed$1 = seed.length === 0 ? [0] : seed;
  var l = seed$1.length;
  for(var i = 0; i <= 54; ++i){
    Caml_array.set(s.st, i, i);
  }
  var accu = "x";
  for(var i$1 = 0 ,i_finish = 54 + (
      55 > l ? 55 : l
    ) | 0; i$1 <= i_finish; ++i$1){
    var j = i$1 % 55;
    var k = i$1 % l;
    accu = combine(accu, Caml_array.get(seed$1, k));
    Caml_array.set(s.st, j, (Caml_array.get(s.st, j) ^ extract(accu)) & 1073741823);
  }
  s.idx = 0;
  
}

function make(seed) {
  var result = {
    st: Caml_array.caml_make_vect(55, 0),
    idx: 0
  };
  full_init(result, seed);
  return result;
}

function make_self_init(param) {
  return make(random_seed(undefined));
}

function copy(s) {
  var result = {
    st: Caml_array.caml_make_vect(55, 0),
    idx: 0
  };
  assign(result, s);
  return result;
}

function bits(s) {
  s.idx = (s.idx + 1 | 0) % 55;
  var curval = Caml_array.get(s.st, s.idx);
  var newval = Caml_array.get(s.st, (s.idx + 24 | 0) % 55) + (curval ^ (curval >>> 25) & 31) | 0;
  var newval30 = newval & 1073741823;
  Caml_array.set(s.st, s.idx, newval30);
  return newval30;
}

function $$int(s, bound) {
  if (bound > 1073741823 || bound <= 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Random.int",
          Error: new Error()
        };
  }
  while(true) {
    var r = bits(s);
    var v = r % bound;
    if ((r - v | 0) <= ((1073741823 - bound | 0) + 1 | 0)) {
      return v;
    }
    continue ;
  };
}

function int32(s, bound) {
  if (bound <= 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Random.int32",
          Error: new Error()
        };
  }
  while(true) {
    var b1 = bits(s);
    var b2 = ((bits(s) & 1) << 30);
    var r = b1 | b2;
    var v = r % bound;
    if ((r - v | 0) <= ((Int32.max_int - bound | 0) + 1 | 0)) {
      return v;
    }
    continue ;
  };
}

function int64(s, bound) {
  if (Caml_int64.le(bound, Caml_int64.zero)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Random.int64",
          Error: new Error()
        };
  }
  while(true) {
    var b1 = Caml_int64.of_int32(bits(s));
    var b2 = Caml_int64.lsl_(Caml_int64.of_int32(bits(s)), 30);
    var b3 = Caml_int64.lsl_(Caml_int64.of_int32(bits(s) & 7), 60);
    var r = Caml_int64.or_(b1, Caml_int64.or_(b2, b3));
    var v = Caml_int64.mod_(r, bound);
    if (!Caml_int64.gt(Caml_int64.sub(r, v), Caml_int64.add(Caml_int64.sub(Int64.max_int, bound), Caml_int64.one))) {
      return v;
    }
    continue ;
  };
}

function rawfloat(s) {
  var r1 = bits(s);
  var r2 = bits(s);
  return (r1 / 1073741824.0 + r2) / 1073741824.0;
}

function $$float(s, bound) {
  return rawfloat(s) * bound;
}

function bool(s) {
  return (bits(s) & 1) === 0;
}

var $$default = {
  st: [
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
  idx: 0
};

function bits$1(param) {
  return bits($$default);
}

function $$int$1(bound) {
  return $$int($$default, bound);
}

function int32$1(bound) {
  return int32($$default, bound);
}

function int64$1(bound) {
  return int64($$default, bound);
}

function $$float$1(scale) {
  return rawfloat($$default) * scale;
}

function bool$1(param) {
  return bool($$default);
}

function full_init$1(seed) {
  return full_init($$default, seed);
}

function init(seed) {
  return full_init($$default, [seed]);
}

function self_init(param) {
  return full_init$1(random_seed(undefined));
}

function get_state(param) {
  return copy($$default);
}

function set_state(s) {
  return assign($$default, s);
}

var State = {
  make: make,
  make_self_init: make_self_init,
  copy: copy,
  bits: bits,
  $$int: $$int,
  int32: int32,
  int64: int64,
  $$float: $$float,
  bool: bool
};

export {
  init ,
  full_init$1 as full_init,
  self_init ,
  bits$1 as bits,
  $$int$1 as $$int,
  int32$1 as int32,
  int64$1 as int64,
  $$float$1 as $$float,
  bool$1 as bool,
  State ,
  get_state ,
  set_state ,
  
}
/* No side effect */
