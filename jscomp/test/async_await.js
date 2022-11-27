'use strict';


function next(n) {
  return n + 1 | 0;
}

async function useNext(param) {
  return 4;
}

function Make(I) {
  var get = async function (key) {
    return await I.get(key);
  };
  return {
          get: get
        };
}

exports.next = next;
exports.useNext = useNext;
exports.Make = Make;
/* No side effect */
