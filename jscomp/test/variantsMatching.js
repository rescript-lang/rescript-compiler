'use strict';


function toEnum(x) {
  switch (x) {
    case "A" :
        return 0;
    case "B" :
        return 1;
    case "C" :
        return 2;
    case "D" :
        return 3;
    case "E" :
        return 4;
    
  }
}

function toString(x) {
  switch (x) {
    case "A" :
        return "A";
    case "B" :
        return "B";
    case "C" :
        return "C";
    case "D" :
        return "D";
    case "E" :
        return "E";
    
  }
}

function bar(x) {
  switch (x) {
    case "A" :
    case "E" :
        return 10;
    default:
      return 0;
  }
}

function and_(x, y) {
  if (x === "True" && y === "True") {
    return "True";
  } else {
    return "False";
  }
}

function id(x) {
  if (x === "True") {
    return "True";
  } else {
    return "False";
  }
}

function not_(x) {
  if (x === "True") {
    return "False";
  } else {
    return "True";
  }
}

function st(state) {
  if (typeof state !== "object") {
    return 0;
  } else {
    return 23;
  }
}

function showToJs(x) {
  if (typeof x !== "object" && x === "No") {
    return false;
  } else {
    return true;
  }
}

function third(l) {
  if (!l) {
    return false;
  }
  if (l.hd !== 1) {
    return false;
  }
  var match = l.tl;
  if (!match) {
    return false;
  }
  if (match.hd !== 2) {
    return false;
  }
  var match$1 = match.tl;
  if (match$1 && !(match$1.hd !== 3 || match$1.tl)) {
    return true;
  } else {
    return false;
  }
}

function third2(l) {
  if (typeof l !== "object") {
    return false;
  }
  if (l._0 !== 1) {
    return false;
  }
  var match = l._1;
  if (typeof match !== "object") {
    return false;
  }
  if (match._0 !== 2) {
    return false;
  }
  var match$1 = match._1;
  if (typeof match$1 !== "object") {
    return false;
  }
  if (match$1._0 !== 3) {
    return false;
  }
  var tmp = match$1._1;
  if (typeof tmp !== "object") {
    return true;
  } else {
    return false;
  }
}

function foo(x) {
  if (typeof x !== "object") {
    switch (x) {
      case "dd" :
          return 1;
      case 12 :
          return 2;
      case "C" :
          return 3;
      
    }
  } else {
    switch (x.TAG) {
      case "qq" :
          return 4;
      case 42 :
          return 5;
      case "F" :
          return 6;
      
    }
  }
}

var CustomizeTags_d = {
  TAG: "qq",
  _0: 42
};

var CustomizeTags_e = {
  TAG: 42,
  _0: 0
};

var CustomizeTags = {
  foo: foo,
  a: "dd",
  b: 12,
  c: "C",
  d: CustomizeTags_d,
  e: CustomizeTags_e
};

exports.toEnum = toEnum;
exports.toString = toString;
exports.bar = bar;
exports.and_ = and_;
exports.id = id;
exports.not_ = not_;
exports.st = st;
exports.showToJs = showToJs;
exports.third = third;
exports.third2 = third2;
exports.CustomizeTags = CustomizeTags;
/* No side effect */
