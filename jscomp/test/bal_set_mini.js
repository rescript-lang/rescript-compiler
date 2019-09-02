'use strict';


function height(param) {
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = height(l);
  var hr = height(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: v,
          Arg2: r,
          Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, v, r) {
  var hl = height(l);
  var hr = height(r);
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, lr.Arg0), lr.Arg1, create(lr.Arg2, v, r));
      } else {
        return "Empty";
      }
    } else {
      return "Empty";
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create(create(l, v, rl.Arg0), rl.Arg1, create(rl.Arg2, rv, rr));
      } else {
        return "Empty";
      }
    } else {
      return "Empty";
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
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
  if (t !== "Empty") {
    var r = t.Arg2;
    var v = t.Arg1;
    var l = t.Arg0;
    var c = compare_int(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  }
}

function min_elt(_def, _param) {
  while(true) {
    var param = _param;
    var def = _def;
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        _param = l;
        _def = param.Arg1;
        continue ;
      } else {
        return param.Arg1;
      }
    } else {
      return def;
    }
  };
}

function remove_min_elt(l, v, r) {
  if (l !== "Empty") {
    return bal(remove_min_elt(l.Arg0, l.Arg1, l.Arg2), v, r);
  } else {
    return r;
  }
}

function internal_merge(l, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      var rv = r.Arg1;
      return bal(l, min_elt(rv, r), remove_min_elt(r.Arg0, rv, r.Arg2));
    } else {
      return l;
    }
  } else {
    return r;
  }
}

function remove(x, tree) {
  if (tree !== "Empty") {
    var r = tree.Arg2;
    var v = tree.Arg1;
    var l = tree.Arg0;
    var c = compare_int(x, v);
    if (c === 0) {
      return internal_merge(l, r);
    } else if (c < 0) {
      return bal(remove(x, l), v, r);
    } else {
      return bal(l, v, remove(x, r));
    }
  } else {
    return "Empty";
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = compare_int(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg2;
        continue ;
      }
    } else {
      return false;
    }
  };
}

var v = "Empty";

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

if (match !== "Empty") {
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
