


function filterInPlace(p, a) {
  let i = 0;
  let j = 0;
  while(i < a.length) {
    let v = a[i];
    if (p(v)) {
      a[j] = v;
      j = j + 1 | 0;
    }
    i = i + 1 | 0;
  };
  a.splice(j);
}

function empty(a) {
  a.splice(0);
}

function pushBack(x, xs) {
  xs.push(x);
}

function memByRef(x, xs) {
  return xs.indexOf(x) >= 0;
}

function iter(f, xs) {
  for(let i = 0 ,i_finish = xs.length; i < i_finish; ++i){
    f(xs[i]);
  }
}

function iteri(f, a) {
  for(let i = 0 ,i_finish = a.length; i < i_finish; ++i){
    f(i, a[i]);
  }
}

function toList(a) {
  let _i = a.length - 1 | 0;
  let _res = /* [] */0;
  while(true) {
    let res = _res;
    let i = _i;
    if (i < 0) {
      return res;
    }
    _res = {
      hd: a[i],
      tl: res
    };
    _i = i - 1 | 0;
    continue;
  };
}

function init(n, f) {
  let v = new Array(n);
  for(let i = 0; i < n; ++i){
    v[i] = f(i);
  }
  return v;
}

function copy(x) {
  let len = x.length;
  let b = new Array(len);
  for(let i = 0; i < len; ++i){
    b[i] = x[i];
  }
  return b;
}

function map(f, a) {
  let l = a.length;
  let r = new Array(l);
  for(let i = 0; i < l; ++i){
    r[i] = f(a[i]);
  }
  return r;
}

function foldLeft(f, x, a) {
  let r = x;
  for(let i = 0 ,i_finish = a.length; i < i_finish; ++i){
    r = f(r, a[i]);
  }
  return r;
}

function foldRight(f, a, x) {
  let r = x;
  for(let i = a.length - 1 | 0; i >= 0; --i){
    r = f(a[i], r);
  }
  return r;
}

function mapi(f, a) {
  let l = a.length;
  if (l === 0) {
    return [];
  }
  let r = new Array(l);
  for(let i = 0; i < l; ++i){
    r[i] = f(i, a[i]);
  }
  return r;
}

function append(x, a) {
  return a.concat([x]);
}

export {
  filterInPlace,
  empty,
  pushBack,
  copy,
  memByRef,
  iter,
  iteri,
  toList,
  map,
  mapi,
  foldLeft,
  foldRight,
  init,
  append,
}
/* No side effect */
