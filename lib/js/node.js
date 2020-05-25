'use strict';


function test(x) {
  if (typeof x === "string") {
    return [
            /* String */0,
            x
          ];
  } else {
    return [
            /* Buffer */1,
            x
          ];
  }
}

var Path;

var Fs;

var Process;

var Module;

var $$Buffer;

var Child_process;

exports.Path = Path;
exports.Fs = Fs;
exports.Process = Process;
exports.Module = Module;
exports.$$Buffer = $$Buffer;
exports.Child_process = Child_process;
exports.test = test;
/* No side effect */
