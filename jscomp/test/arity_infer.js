// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function f0(x) {
  let tmp;
  if (x > 3) {
    tmp = x => x + 1 | 0;
  } else {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  return tmp(3);
}

function f1(x) {
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
  return undefined(x);
}

function f3(x) {
  let tmp;
  switch (x) {
    case 0 :
      tmp = x => x + 1 | 0;
      break;
    case 1 :
      tmp = x => x + 2 | 0;
      break;
    case 2 :
      tmp = x => x + 3 | 0;
      break;
    case 3 :
      tmp = x => x + 4 | 0;
      break;
    default:
      throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
  }
  return tmp(3);
}

exports.f0 = f0;
exports.f1 = f1;
exports.f3 = f3;
/* No side effect */
