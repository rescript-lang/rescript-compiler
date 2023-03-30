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

var s = "abc";

var w = {
  x: 10,
  y: ""
};

exports.i = i;
exports.s = s;
exports.classify = classify;
exports.w = w;
exports.cls = cls;
exports.ListWithTuples = ListWithTuples;
exports.ListWithObjects = ListWithObjects;
exports.tuplesToObjects = tuplesToObjects;
exports.l1 = l1;
exports.l2 = l2;
exports.Truthy = Truthy;
/* l2 Not a pure module */
