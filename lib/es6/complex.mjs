


var one = {
  re: 1.0,
  im: 0.0
};

function add(x, y) {
  return {
          re: x.re + y.re,
          im: x.im + y.im
        };
}

function sub(x, y) {
  return {
          re: x.re - y.re,
          im: x.im - y.im
        };
}

function neg(x) {
  return {
          re: -x.re,
          im: -x.im
        };
}

function conj(x) {
  return {
          re: x.re,
          im: -x.im
        };
}

function mul(x, y) {
  return {
          re: x.re * y.re - x.im * y.im,
          im: x.re * y.im + x.im * y.re
        };
}

function div(x, y) {
  if (Math.abs(y.re) >= Math.abs(y.im)) {
    var r = y.im / y.re;
    var d = y.re + r * y.im;
    return {
            re: (x.re + r * x.im) / d,
            im: (x.im - r * x.re) / d
          };
  }
  var r$1 = y.re / y.im;
  var d$1 = y.im + r$1 * y.re;
  return {
          re: (r$1 * x.re + x.im) / d$1,
          im: (r$1 * x.im - x.re) / d$1
        };
}

function inv(x) {
  return div(one, x);
}

function norm2(x) {
  return x.re * x.re + x.im * x.im;
}

function norm(x) {
  var r = Math.abs(x.re);
  var i = Math.abs(x.im);
  if (r === 0.0) {
    return i;
  }
  if (i === 0.0) {
    return r;
  }
  if (r >= i) {
    var q = i / r;
    return r * Math.sqrt(1.0 + q * q);
  }
  var q$1 = r / i;
  return i * Math.sqrt(1.0 + q$1 * q$1);
}

function arg(x) {
  return Math.atan2(x.im, x.re);
}

function polar(n, a) {
  return {
          re: Math.cos(a) * n,
          im: Math.sin(a) * n
        };
}

function sqrt(x) {
  if (x.re === 0.0 && x.im === 0.0) {
    return {
            re: 0.0,
            im: 0.0
          };
  }
  var r = Math.abs(x.re);
  var i = Math.abs(x.im);
  var w;
  if (r >= i) {
    var q = i / r;
    w = Math.sqrt(r) * Math.sqrt(0.5 * (1.0 + Math.sqrt(1.0 + q * q)));
  } else {
    var q$1 = r / i;
    w = Math.sqrt(i) * Math.sqrt(0.5 * (q$1 + Math.sqrt(1.0 + q$1 * q$1)));
  }
  if (x.re >= 0.0) {
    return {
            re: w,
            im: 0.5 * x.im / w
          };
  } else {
    return {
            re: 0.5 * i / w,
            im: x.im >= 0.0 ? w : -w
          };
  }
}

function exp(x) {
  var e = Math.exp(x.re);
  return {
          re: e * Math.cos(x.im),
          im: e * Math.sin(x.im)
        };
}

function log(x) {
  return {
          re: Math.log(norm(x)),
          im: Math.atan2(x.im, x.re)
        };
}

function pow(x, y) {
  return exp(mul(y, log(x)));
}

var zero = {
  re: 0.0,
  im: 0.0
};

var i = {
  re: 0.0,
  im: 1.0
};

export {
  zero ,
  one ,
  i ,
  neg ,
  conj ,
  add ,
  sub ,
  mul ,
  inv ,
  div ,
  sqrt ,
  norm2 ,
  norm ,
  arg ,
  polar ,
  exp ,
  log ,
  pow ,
  
}
/* No side effect */
