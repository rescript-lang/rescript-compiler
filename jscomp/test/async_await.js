'use strict';


async function withAnnotations1(param) {
  return 3;
}

async function withAnnotations2(param) {
  return 3;
}

async function withAnnotations3(param) {
  return 3;
}

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

exports.withAnnotations1 = withAnnotations1;
exports.withAnnotations2 = withAnnotations2;
exports.withAnnotations3 = withAnnotations3;
exports.next = next;
exports.useNext = useNext;
exports.Make = Make;
/* No side effect */
