// Generated by ReScript, PLEASE EDIT WITH CARE


function f(_x, _y) {
  while (true) {
    let y = _y;
    let x = _x;
    _y = x;
    _x = y;
    continue;
  };
}

function f1(_x, _y, _z) {
  while (true) {
    let z = _z;
    let y = _y;
    let x = _x;
    console.log(z);
    _z = x;
    _y = z;
    _x = y;
    continue;
  };
}

function f2(x, _y) {
  while (true) {
    let y = _y;
    _y = y + 10 | 0;
    continue;
  };
}

function f3(_x, _y) {
  while (true) {
    let y = _y;
    let x = _x;
    _y = x + 10 | 0;
    _x = y;
    continue;
  };
}

function f4(_x, _y) {
  while (true) {
    let y = _y;
    let x = _x;
    _y = y + x | 0;
    _x = x + 10 | 0;
    continue;
  };
}

function f5(_x, _y, z) {
  while (true) {
    let y = _y;
    _y = z + 20 | 0;
    _x = y + 10 | 0;
    continue;
  };
}

function f6(b) {
  while (true) {
    if (!b) {
      return false;
    }
    continue;
  };
}

function f7(b) {
  while (true) {
    if (b) {
      return true;
    }
    continue;
  };
}

function f8(_x, _y) {
  while (true) {
    let y = _y;
    let x = _x;
    if (x > 10) {
      _y = y + 1 | 0;
      continue;
    }
    if (x < 5) {
      _x = x - 1 | 0;
      continue;
    }
    if (x <= 6) {
      return f8(x, y + 1 | 0) + f8(x - 1 | 0, y) | 0;
    }
    _x = x - 2 | 0;
    continue;
  };
}

export {
  f,
  f1,
  f2,
  f3,
  f4,
  f5,
  f6,
  f7,
  f8,
}
/* No side effect */
