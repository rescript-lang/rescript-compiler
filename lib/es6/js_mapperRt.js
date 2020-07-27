


function raiseWhenNotFound(x) {
  if (x == null) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  return x;
}

function fromInt(len, xs, $$enum) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === len) {
      return ;
    }
    var k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function fromIntAssert(len, xs, $$enum) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === len) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

export {
  raiseWhenNotFound ,
  fromInt ,
  fromIntAssert ,
  
}
/* No side effect */
