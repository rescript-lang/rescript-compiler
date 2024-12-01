// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Nodeassert from "node:assert";

function ok(loc, a) {
  Nodeassert.ok(a, loc);
}

function eq(loc, a, b) {
  Nodeassert.deepStrictEqual(a, b, loc);
}

function $$throw(loc, f) {
  Nodeassert.throws(f, undefined, loc);
}

export {
  ok,
  eq,
  $$throw,
}
/* node:assert Not a pure module */
