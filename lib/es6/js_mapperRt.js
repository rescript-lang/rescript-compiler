


function raiseWhenNotFound(x) {
  if (x == null) {
    throw new Error("Not_found", {
              cause: {
                RE_EXN_ID: "Not_found"
              }
            });
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
      throw new Error("Not_found", {
                cause: {
                  RE_EXN_ID: "Not_found"
                }
              });
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
