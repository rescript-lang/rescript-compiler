


function sub(x, offset, len) {
  let result = new Array(len);
  let j = 0;
  let i = offset;
  while (j < len) {
    result[j] = x[i];
    j = j + 1 | 0;
    i = i + 1 | 0;
  };
  return result;
}

function len(_acc, _l) {
  while (true) {
    let l = _l;
    let acc = _acc;
    if (!l) {
      return acc;
    }
    _l = l.tl;
    _acc = l.hd.length + acc | 0;
    continue;
  };
}

function fill(arr, _i, _l) {
  while (true) {
    let l = _l;
    let i = _i;
    if (!l) {
      return;
    }
    let x = l.hd;
    let l$1 = x.length;
    let k = i;
    let j = 0;
    while (j < l$1) {
      arr[k] = x[j];
      k = k + 1 | 0;
      j = j + 1 | 0;
    };
    _l = l.tl;
    _i = k;
    continue;
  };
}

function concat(l) {
  let v = len(0, l);
  let result = new Array(v);
  fill(result, 0, l);
  return result;
}

function set(xs, index, newval) {
  if (index < 0 || index >= xs.length) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "index out of bounds"
      }
    });
  }
  xs[index] = newval;
}

function get(xs, index) {
  if (index < 0 || index >= xs.length) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "index out of bounds"
      }
    });
  }
  return xs[index];
}

function make(len, init) {
  let b = new Array(len);
  for (let i = 0; i < len; ++i) {
    b[i] = init;
  }
  return b;
}

function make_float(len) {
  let b = new Array(len);
  for (let i = 0; i < len; ++i) {
    b[i] = 0;
  }
  return b;
}

function blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for (let j = 0; j < len; ++j) {
      a2[j + i2 | 0] = a1[j + i1 | 0];
    }
    return;
  }
  for (let j$1 = len - 1 | 0; j$1 >= 0; --j$1) {
    a2[j$1 + i2 | 0] = a1[j$1 + i1 | 0];
  }
}

function dup(prim) {
  return prim.slice(0);
}

export {
  dup,
  sub,
  concat,
  make,
  make_float,
  blit,
  get,
  set,
}
/* No side effect */
