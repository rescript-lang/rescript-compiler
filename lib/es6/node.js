


function test(x) {
  if (typeof x === "string") {
    return [
            "String",
            x
          ];
  } else {
    return [
            "Buffer",
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

export {
  Path ,
  Fs ,
  Process ,
  Module ,
  $$Buffer ,
  Child_process ,
  test ,
}
/* No side effect */
