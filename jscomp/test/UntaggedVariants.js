'use strict';


function classify(x) {
  if (typeof x !== "string" && typeof x !== "number") {
    return "A";
  } else if (typeof x === "number") {
    return "An integer";
  } else {
    return "A string" + x;
  }
}

function classify2(x) {
  if (typeof x === "string") {
    return "A string" + x;
  } else {
    return "A float";
  }
}

function cls(x) {
  if (typeof x !== "object") {
    if (x === "One") {
      return "one";
    } else {
      return "two";
    }
  } else {
    return "object" + x.y;
  }
}

var ListWithTuples = {};

var ListWithObjects = {};

function tuplesToObjects(l) {
  if (l === undefined) {
    return null;
  } else {
    return {
            hd: l[0],
            tl: tuplesToObjects(l[1])
          };
  }
}

var l1 = [
  1,
  [
    2,
    [
      3,
      undefined
    ]
  ]
];

var l2 = tuplesToObjects(l1);

console.log("l1", l1);

console.log("l2", l2);

function isTrue(x) {
  if (typeof x !== "object") {
    return true;
  } else {
    return x.flag;
  }
}

var Truthy = {
  isTrue: isTrue
};

var i = 42;

var i2 = 42.5;

var s = "abc";

var s2 = "abc";

var w = {
  x: 10,
  y: ""
};

exports.i = i;
exports.i2 = i2;
exports.s = s;
exports.s2 = s2;
exports.classify = classify;
exports.classify2 = classify2;
exports.w = w;
exports.cls = cls;
exports.ListWithTuples = ListWithTuples;
exports.ListWithObjects = ListWithObjects;
exports.tuplesToObjects = tuplesToObjects;
exports.l1 = l1;
exports.l2 = l2;
exports.Truthy = Truthy;
/* l2 Not a pure module */
