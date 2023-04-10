'use strict';


function classify(x) {
  if (x === "A" && typeof x !== "number") {
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

function classify$1(x) {
  if (typeof x !== "object") {
    if (x === null) {
      return "null";
    } else {
      return "undefined";
    }
  } else {
    return "object" + x.name;
  }
}

var TwoObjects = {
  classify: classify$1
};

function classify$2(x) {
  if (x === "A" || x === "B") {
    if (x === "A") {
      return "a";
    } else {
      return "b";
    }
  }
  console.log(x);
  return "Unknown";
}

var Unknown = {
  classify: classify$2
};

function classify$3(x) {
  if (typeof x !== "object" && typeof x !== "number" && (x === "C" || x === "B" || x === "A" || x === "D")) {
    switch (x) {
      case "A" :
          return "a";
      case "B" :
          return "b";
      case "C" :
          return "c";
      case "D" :
          return "d";
      
    }
  } else {
    switch (typeof x) {
      case "string" :
          return "string";
      case "number" :
          return "int";
      case "object" :
          return "Object" + x.name;
      
    }
  }
}

var MultipleBlocks = {
  classify: classify$3
};

function classify$4(x) {
  switch (typeof x) {
    case "string" :
        return "string";
    case "number" :
        return "int";
    case "object" :
        return "Object" + x.name;
    
  }
}

var OnlyBlocks = {
  classify: classify$4
};

function classify$5(x) {
  if (Array.isArray(x)) {
    return "array";
  }
  switch (typeof x) {
    case "string" :
        return "string";
    case "number" :
        return "int";
    case "object" :
        return "Object" + x.name;
    
  }
}

var WithArray = {
  classify: classify$5
};

function classify$6(x) {
  if (!(x instanceof Array) && typeof x !== "object" && typeof x !== "number" && typeof x !== "string") {
    switch (x) {
      case false :
          return "JSONFalse";
      case true :
          return "JSONTrue";
      case null :
          return "JSONNull";
      
    }
  } else {
    if (Array.isArray(x)) {
      return {
              TAG: "JSONArray",
              _0: x
            };
    }
    switch (typeof x) {
      case "string" :
          return {
                  TAG: "JSONString",
                  _0: x
                };
      case "number" :
          return {
                  TAG: "JSONNumber",
                  _0: x
                };
      case "object" :
          return {
                  TAG: "JSONObject",
                  _0: x
                };
      
    }
  }
}

var Json = {
  classify: classify$6
};

function check(s, y) {
  if (s === "B") {
    return 42;
  }
  var x = s[0];
  if (x === "B") {
    return 42;
  }
  var tmp = s[1];
  if (tmp === "B" && x !== y) {
    return 41;
  } else {
    return 42;
  }
}

var TrickyNested = {
  check: check
};

function checkEnum(e) {
  if (!(e === "Two" || e === "One" || e === "Three")) {
    return "Something else..." + e;
  }
  switch (e) {
    case "One" :
        return "One!";
    case "Two" :
        return "Two";
    case "Three" :
        return "Threeeee";
    
  }
}

var Overlap = {
  checkEnum: checkEnum
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
exports.TwoObjects = TwoObjects;
exports.Unknown = Unknown;
exports.MultipleBlocks = MultipleBlocks;
exports.OnlyBlocks = OnlyBlocks;
exports.WithArray = WithArray;
exports.Json = Json;
exports.TrickyNested = TrickyNested;
exports.Overlap = Overlap;
/* l2 Not a pure module */
