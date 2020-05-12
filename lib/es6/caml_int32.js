


function div(x, y) {
  if (y === 0) {
    throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
  }
  return x / y | 0;
}

function mod_(x, y) {
  if (y === 0) {
    throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
  }
  return x % y;
}

var imul = (Math.imul || function (x,y) {
  y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; 
});

export {
  div ,
  mod_ ,
  imul ,
  
}
/* imul Not a pure module */
