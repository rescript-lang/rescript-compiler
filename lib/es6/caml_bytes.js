


function set(s, i, ch) {
  if (i < 0 || i >= s.length) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "index out of bounds"
          }
        });
  }
  s[i] = ch;
}

function get(s, i) {
  if (i < 0 || i >= s.length) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "index out of bounds"
          }
        });
  }
  return s[i];
}

function create(len) {
  if (len < 0) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "String.create"
          }
        });
  }
  let result = new Array(len);
  for(let i = 0; i < len; ++i){
    result[i] = /* '\000' */0;
  }
  return result;
}

function bytes_compare_aux(s1, s2, _off, len, def) {
  while(true) {
    let off = _off;
    if (off >= len) {
      return def;
    }
    let a = s1[off];
    let b = s2[off];
    if (a > b) {
      return 1;
    }
    if (a < b) {
      return -1;
    }
    _off = off + 1 | 0;
    continue;
  };
}

function bytes_compare(s1, s2) {
  let len1 = s1.length;
  let len2 = s2.length;
  if (len1 === len2) {
    return bytes_compare_aux(s1, s2, 0, len1, 0);
  } else if (len1 < len2) {
    return bytes_compare_aux(s1, s2, 0, len1, -1);
  } else {
    return bytes_compare_aux(s1, s2, 0, len2, 1);
  }
}

function bytes_equal(s1, s2) {
  let len1 = s1.length;
  let len2 = s2.length;
  if (len1 === len2) {
    let _off = 0;
    while(true) {
      let off = _off;
      if (off === len1) {
        return true;
      }
      let a = s1[off];
      let b = s2[off];
      if (a !== b) {
        return false;
      }
      _off = off + 1 | 0;
      continue;
    };
  } else {
    return false;
  }
}

function bytes_greaterthan(s1, s2) {
  return bytes_compare(s1, s2) > 0;
}

function bytes_greaterequal(s1, s2) {
  return bytes_compare(s1, s2) >= 0;
}

function bytes_lessthan(s1, s2) {
  return bytes_compare(s1, s2) < 0;
}

function bytes_lessequal(s1, s2) {
  return bytes_compare(s1, s2) <= 0;
}

export {
  create,
  get,
  set,
  bytes_compare,
  bytes_greaterthan,
  bytes_greaterequal,
  bytes_lessthan,
  bytes_lessequal,
  bytes_equal,
}
/* No side effect */
