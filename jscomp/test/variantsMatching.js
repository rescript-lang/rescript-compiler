'use strict';


function foo(x) {
  switch (x) {
    case /* A */0 :
        return "A";
    case /* B */1 :
        return "B";
    case /* C */2 :
        return "C";
    case /* D */3 :
        return "D";
    case /* E */4 :
        return "E";
    
  }
}

function bar(x) {
  switch (x) {
    case /* A */0 :
    case /* E */4 :
        return 10;
    default:
      return 0;
  }
}

function and_(x, y) {
  if (x === /* True */0) {
    return y;
  } else {
    return /* False */1;
  }
}

function id(x) {
  return x;
}

function not_(x) {
  if (x === /* True */0) {
    return /* False */1;
  } else {
    return /* True */0;
  }
}

function st(state) {
  if (/* tag */typeof state === "number") {
    return 0;
  } else {
    return 23;
  }
}

function showToJs(x) {
  if (/* tag */typeof x === "number" && x === /* No */0) {
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
  if (/* tag */typeof l === "number") {
    return false;
  }
  if (l._0 !== 1) {
    return false;
  }
  var match = l._1;
  if (/* tag */typeof match === "number") {
    return false;
  }
  if (match._0 !== 2) {
    return false;
  }
  var match$1 = match._1;
  if (/* tag */typeof match$1 === "number") {
    return false;
  }
  if (match$1._0 !== 3) {
    return false;
  }
  var tmp = match$1._1;
  if (/* tag */typeof tmp === "number") {
    return true;
  } else {
    return false;
  }
}

exports.foo = foo;
exports.bar = bar;
exports.and_ = and_;
exports.id = id;
exports.not_ = not_;
exports.st = st;
exports.showToJs = showToJs;
exports.third = third;
exports.third2 = third2;
/* No side effect */
