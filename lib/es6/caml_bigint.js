


function div(x, y) {
  if (y === 0n) {
    throw new Error("Division_by_zero", {
      cause: {
        RE_EXN_ID: "Division_by_zero"
      }
    });
  }
  return x / y;
}

function mod_(x, y) {
  if (y === 0n) {
    throw new Error("Division_by_zero", {
      cause: {
        RE_EXN_ID: "Division_by_zero"
      }
    });
  }
  return x % y;
}

export {
  div,
  mod_,
}
/* No side effect */
