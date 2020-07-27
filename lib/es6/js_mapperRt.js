


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
    if (i >= len) {
      throw new Error("File \"js_mapperRt.ml\", line 38, characters 4-10");
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
  fromInt ,
  fromIntAssert ,
  
}
/* No side effect */
