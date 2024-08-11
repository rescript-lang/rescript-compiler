'use strict';

let Caml = require("./caml.js");
let $$Array = require("./array.js");
let Int32 = require("./int32.js");
let Int64 = require("./int64.js");
let Digest = require("./digest.js");
let Caml_array = require("./caml_array.js");
let Caml_int64 = require("./caml_int64.js");
let Caml_string = require("./caml_string.js");

function random_seed() {
  return [(Math.floor(Math.random()*0x7fffffff))];
}

function assign(st1, st2) {
  $$Array.blit(st2.st, 0, st1.st, 0, 55);
  st1.idx = st2.idx;
}

function full_init(s, seed) {
  let combine = (accu, x) => {
    return Digest.string(accu + String(x));
  };
  let extract = (d) => {
    return ((Caml_string.get(d, 0) + (Caml_string.get(d, 1) << 8) | 0) + (Caml_string.get(d, 2) << 16) | 0) + (Caml_string.get(d, 3) << 24) | 0;
  };
  let seed$1 = seed.length === 0 ? [0] : seed;
  let l = seed$1.length;
  for (let i = 0; i <= 54; ++i) {
    Caml_array.set(s.st, i, i);
  }
  let accu = "x";
  for (let i$1 = 0, i_finish = 54 + (
      55 > l ? 55 : l
    ) | 0; i$1 <= i_finish; ++i$1) {
    let j = i$1 % 55;
    let k = i$1 % l;
    accu = combine(accu, Caml_array.get(seed$1, k));
    Caml_array.set(s.st, j, (Caml_array.get(s.st, j) ^ extract(accu)) & 1073741823);
  }
  s.idx = 0;
}

function make(seed) {
  let result = {
    st: Caml_array.make(55, 0),
    idx: 0
  };
  full_init(result, seed);
  return result;
}

function make_self_init() {
  return make(random_seed());
}

function copy(s) {
  let result = {
    st: Caml_array.make(55, 0),
    idx: 0
  };
  assign(result, s);
  return result;
}

function bits(s) {
  s.idx = (s.idx + 1 | 0) % 55;
  let curval = Caml_array.get(s.st, s.idx);
  let newval = Caml_array.get(s.st, (s.idx + 24 | 0) % 55) + (curval ^ (curval >>> 25) & 31) | 0;
  let newval30 = newval & 1073741823;
  Caml_array.set(s.st, s.idx, newval30);
  return newval30;
}

function int(s, bound) {
  if (bound > 1073741823 || bound <= 0) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int"
      }
    });
  }
  while (true) {
    let r = bits(s);
    let v = r % bound;
    if ((r - v | 0) <= ((1073741823 - bound | 0) + 1 | 0)) {
      return v;
    }
    continue;
  };
}

function int32(s, bound) {
  if (bound <= 0) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int32"
      }
    });
  }
  while (true) {
    let b1 = bits(s);
    let b2 = ((bits(s) & 1) << 30);
    let r = b1 | b2;
    let v = r % bound;
    if ((r - v | 0) <= ((Int32.max_int - bound | 0) + 1 | 0)) {
      return v;
    }
    continue;
  };
}

function int64(s, bound) {
  if (Caml.i64_le(bound, Caml_int64.zero)) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Random.int64"
      }
    });
  }
  while (true) {
    let b1 = Caml_int64.of_int32(bits(s));
    let b2 = Caml_int64.lsl_(Caml_int64.of_int32(bits(s)), 30);
    let b3 = Caml_int64.lsl_(Caml_int64.of_int32(bits(s) & 7), 60);
    let r = Caml_int64.or_(b1, Caml_int64.or_(b2, b3));
    let v = Caml_int64.mod_(r, bound);
    if (!Caml.i64_gt(Caml_int64.sub(r, v), Caml_int64.add(Caml_int64.sub(Int64.max_int, bound), Caml_int64.one))) {
      return v;
    }
    continue;
  };
}

function rawfloat(s) {
  let r1 = bits(s);
  let r2 = bits(s);
  return (r1 / 1073741824.0 + r2) / 1073741824.0;
}

function float(s, bound) {
  return rawfloat(s) * bound;
}

function bool(s) {
  return (bits(s) & 1) === 0;
}

let $$default = {
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

function bits$1() {
  return bits($$default);
}

function int$1(bound) {
  return int($$default, bound);
}

function int32$1(bound) {
  return int32($$default, bound);
}

function int64$1(bound) {
  return int64($$default, bound);
}

function float$1(scale) {
  return rawfloat($$default) * scale;
}

function bool$1() {
  return bool($$default);
}

function full_init$1(seed) {
  full_init($$default, seed);
}

function init(seed) {
  full_init($$default, [seed]);
}

function self_init() {
  full_init$1(random_seed());
}

function get_state() {
  return copy($$default);
}

function set_state(s) {
  assign($$default, s);
}

let State = {
  make: make,
  make_self_init: make_self_init,
  copy: copy,
  bits: bits,
  int: int,
  int32: int32,
  int64: int64,
  float: float,
  bool: bool
};

exports.init = init;
exports.full_init = full_init$1;
exports.self_init = self_init;
exports.bits = bits$1;
exports.int = int$1;
exports.int32 = int32$1;
exports.int64 = int64$1;
exports.float = float$1;
exports.bool = bool$1;
exports.State = State;
exports.get_state = get_state;
exports.set_state = set_state;
/* No side effect */
