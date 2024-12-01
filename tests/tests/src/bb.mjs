// Generated by ReScript, PLEASE EDIT WITH CARE


function f(x) {
  if (x === "b") {
    return "b";
  } else if (x === "c") {
    return "c";
  } else {
    return "a";
  }
}

function ff(x) {
  switch (x) {
    case "a" :
      return "a";
    case "b" :
      return "b";
    case "c" :
      return "c";
    default:
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bb.res",
          13,
          9
        ],
        Error: new Error()
      };
  }
}

function test(x) {
  let match;
  switch (x) {
    case "a" :
      match = "a";
      break;
    case "b" :
      match = "b";
      break;
    case "c" :
      match = "c";
      break;
    default:
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bb.res",
          21,
          9
        ],
        Error: new Error()
      };
  }
  if (match === "b") {
    return "b";
  } else if (match === "c") {
    return "c";
  } else {
    return "a";
  }
}

let test_poly = "a";

let c = f("a");

let d = f("b");

let e = f("c");

export {
  f,
  ff,
  test,
  test_poly,
  c,
  d,
  e,
}
/* c Not a pure module */
