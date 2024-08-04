'use strict';


function raiseError(str) {
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: str
        }
      });
}

let $$Error = "JsError";

exports.$$Error = $$Error;
exports.raiseError = raiseError;
/* No side effect */
