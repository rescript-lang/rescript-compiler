// Generated by ReScript, PLEASE EDIT WITH CARE


function f(x) {
  switch (x) {
    case "aaaabb" :
      return 0;
    case "bbbb" :
      return 1;
    default:
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "test_string.res",
          7,
          17
        ],
        Error: new Error()
      };
  }
}

function a(x) {
  return "helloworldhello" + x;
}

function b(y, x) {
  return y + ("helloworldhello" + x);
}

function c(x, y) {
  return x + "hellohiuhi" + y;
}

let v = "xx".length;

function h(s) {
  return s.codePointAt(0) === /* 'a' */97;
}

let $$String;

export {
  $$String,
  f,
  a,
  b,
  c,
  v,
  h,
}
/* No side effect */
