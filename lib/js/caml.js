'use strict';


function int_compare(x, y) {
  if (x < y) {
    return -1;
  } else if (x === y) {
    return 0;
  } else {
    return 1;
  }
}

function bool_compare(x, y) {
  if (x) {
    if (y) {
      return 0;
    } else {
      return 1;
    }
  } else if (y) {
    return -1;
  } else {
    return 0;
  }
}

function float_compare(x, y) {
  if (x === y) {
    return 0;
  } else if (x < y) {
    return -1;
  } else if (x > y || x === x) {
    return 1;
  } else if (y === y) {
    return -1;
  } else {
    return 0;
  }
}

function bigint_compare(x, y) {
  if (x < y) {
    return -1;
  } else if (x === y) {
    return 0;
  } else {
    return 1;
  }
}

function string_compare(s1, s2) {
  if (s1 === s2) {
    return 0;
  } else if (s1 < s2) {
    return -1;
  } else {
    return 1;
  }
}

function bool_min(x, y) {
  if (x) {
    return y;
  } else {
    return x;
  }
}

function int_min(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

function float_min(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

function string_min(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

function bool_max(x, y) {
  if (x) {
    return x;
  } else {
    return y;
  }
}

function int_max(x, y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

function float_max(x, y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

function string_max(x, y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

function i64_eq(x, y) {
  if (x[1] === y[1]) {
    return x[0] === y[0];
  } else {
    return false;
  }
}

function i64_ge(param, param$1) {
  var other_hi = param$1[0];
  var hi = param[0];
  if (hi > other_hi) {
    return true;
  } else if (hi < other_hi) {
    return false;
  } else {
    return param[1] >= param$1[1];
  }
}

function i64_neq(x, y) {
  return !i64_eq(x, y);
}

function i64_lt(x, y) {
  return !i64_ge(x, y);
}

function i64_gt(x, y) {
  if (x[0] > y[0]) {
    return true;
  } else if (x[0] < y[0]) {
    return false;
  } else {
    return x[1] > y[1];
  }
}

function i64_le(x, y) {
  return !i64_gt(x, y);
}

function i64_min(x, y) {
  if (i64_ge(x, y)) {
    return y;
  } else {
    return x;
  }
}

function i64_max(x, y) {
  if (i64_gt(x, y)) {
    return x;
  } else {
    return y;
  }
}

exports.int_compare = int_compare;
exports.bool_compare = bool_compare;
exports.float_compare = float_compare;
exports.bigint_compare = bigint_compare;
exports.string_compare = string_compare;
exports.bool_min = bool_min;
exports.int_min = int_min;
exports.float_min = float_min;
exports.string_min = string_min;
exports.bool_max = bool_max;
exports.int_max = int_max;
exports.float_max = float_max;
exports.string_max = string_max;
exports.i64_eq = i64_eq;
exports.i64_neq = i64_neq;
exports.i64_lt = i64_lt;
exports.i64_gt = i64_gt;
exports.i64_le = i64_le;
exports.i64_ge = i64_ge;
exports.i64_min = i64_min;
exports.i64_max = i64_max;
/* No side effect */
