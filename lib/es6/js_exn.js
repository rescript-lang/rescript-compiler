


function raiseError(str) {
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: str
        }
      });
}

let $$Error = "JsError";

export {
  $$Error,
  raiseError,
}
/* No side effect */
