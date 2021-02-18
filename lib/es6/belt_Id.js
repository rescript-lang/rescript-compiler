

import * as Curry from "./curry.js";

function MakeComparableU(M) {
  return M;
}

function MakeComparable(M) {
  var cmp = M.cmp;
  var cmp$1 = Curry.__2(cmp);
  return {
          cmp: cmp$1
        };
}

function comparableU(cmp) {
  return {
          cmp: cmp
        };
}

function comparable(cmp) {
  var cmp$1 = Curry.__2(cmp);
  return {
          cmp: cmp$1
        };
}

function MakeHashableU(M) {
  return M;
}

function MakeHashable(M) {
  var hash = M.hash;
  var hash$1 = Curry.__1(hash);
  var eq = M.eq;
  var eq$1 = Curry.__2(eq);
  return {
          hash: hash$1,
          eq: eq$1
        };
}

function hashableU(hash, eq) {
  return {
          hash: hash,
          eq: eq
        };
}

function hashable(hash, eq) {
  var hash$1 = Curry.__1(hash);
  var eq$1 = Curry.__2(eq);
  return {
          hash: hash$1,
          eq: eq$1
        };
}

export {
  MakeComparableU ,
  MakeComparable ,
  comparableU ,
  comparable ,
  MakeHashableU ,
  MakeHashable ,
  hashableU ,
  hashable ,
  
}
/* No side effect */
