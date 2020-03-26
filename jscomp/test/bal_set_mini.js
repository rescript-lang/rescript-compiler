'use strict';


function height(param) {
  if (param) {
    return param[3];
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          l,
          v,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, v, r) {
  var hl = height(l);
  var hr = height(r);
  if (hl > (hr + 2 | 0)) {
    if (!l) {
      return /* Empty */0;
    }
    var lr = l[2];
    var lv = l[1];
    var ll = l[0];
    if (height(ll) >= height(lr)) {
      return create(ll, lv, create(lr, v, r));
    } else if (lr) {
      return create(create(ll, lv, lr[0]), lr[1], create(lr[2], v, r));
    } else {
      return /* Empty */0;
    }
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (!r) {
    return /* Empty */0;
  }
  var rr = r[2];
  var rv = r[1];
  var rl = r[0];
  if (height(rr) >= height(rl)) {
    return create(create(l, v, rl), rv, rr);
  } else if (rl) {
    return create(create(l, v, rl[0]), rl[1], create(rl[2], rv, rr));
  } else {
    return /* Empty */0;
  }
}

function compare_int(x, y) {
  if (x > y) {
    return 1;
  } else if (x === y) {
    return 0;
  } else {
    return -1;
  }
}

function add(x, t) {
  if (!t) {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
  }
  var r = t[2];
  var v = t[1];
  var l = t[0];
  var c = compare_int(x, v);
  if (c === 0) {
    return t;
  } else if (c < 0) {
    return bal(add(x, l), v, r);
  } else {
    return bal(l, v, add(x, r));
  }
}

function min_elt(_def, _param) {
  while(true) {
    var param = _param;
    var def = _def;
    if (!param) {
      return def;
    }
    var l = param[0];
    if (!l) {
      return param[1];
    }
    _param = l;
    _def = param[1];
    continue ;
  };
}

function remove_min_elt(l, v, r) {
  if (l) {
    return bal(remove_min_elt(l[0], l[1], l[2]), v, r);
  } else {
    return r;
  }
}

function internal_merge(l, r) {
  if (!l) {
    return r;
  }
  if (!r) {
    return l;
  }
  var rv = r[1];
  return bal(l, min_elt(rv, r), remove_min_elt(r[0], rv, r[2]));
}

function remove(x, tree) {
  if (!tree) {
    return /* Empty */0;
  }
  var r = tree[2];
  var v = tree[1];
  var l = tree[0];
  var c = compare_int(x, v);
  if (c === 0) {
    return internal_merge(l, r);
  } else if (c < 0) {
    return bal(remove(x, l), v, r);
  } else {
    return bal(l, v, remove(x, r));
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = compare_int(x, param[1]);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param[0] : param[2];
    continue ;
  };
}

var v = /* Empty */0;

for(var i = 0; i <= 100000; ++i){
  v = add(i, v);
}

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  if (!mem(i$1, v)) {
    console.log("impossible");
  }
  
}

for(var i$2 = 0; i$2 <= 100000; ++i$2){
  v = remove(i$2, v);
}

var match = v;

if (match) {
  console.log("impossible");
}

exports.height = height;
exports.create = create;
exports.bal = bal;
exports.compare_int = compare_int;
exports.add = add;
exports.min_elt = min_elt;
exports.remove_min_elt = remove_min_elt;
exports.internal_merge = internal_merge;
exports.remove = remove;
exports.mem = mem;
/*  Not a pure module */
