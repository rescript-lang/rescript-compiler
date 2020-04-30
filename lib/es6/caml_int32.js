


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

function caml_bswap16(x) {
  return ((x & 255) << 8) | ((x & 65280) >>> 8);
}

function caml_int32_bswap(x) {
  return ((x & 255) << 24) | ((x & 65280) << 8) | ((x & 16711680) >>> 8) | ((x & 4278190080) >>> 24);
}

var imul = (Math.imul || function (x,y) {
  y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; 
});

var caml_nativeint_bswap = caml_int32_bswap;

export {
  div ,
  mod_ ,
  caml_bswap16 ,
  caml_int32_bswap ,
  caml_nativeint_bswap ,
  imul ,
  
}
/* imul Not a pure module */
