


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

export {
  int_compare,
  bool_compare,
  float_compare,
  bigint_compare,
  string_compare,
  bool_min,
  int_min,
  float_min,
  string_min,
  bool_max,
  int_max,
  float_max,
  string_max,
}
/* No side effect */
