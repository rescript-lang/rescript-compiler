'use strict';


async function willBeInlined(param) {
  return 3;
}

var inlined = willBeInlined(undefined);

function wrapSomethingAsync(param) {
  ((async function (param) {
          var test = await Promise.resolve("Test");
          console.log(test);
        })(777));
}

exports.willBeInlined = willBeInlined;
exports.inlined = inlined;
exports.wrapSomethingAsync = wrapSomethingAsync;
/* inlined Not a pure module */
