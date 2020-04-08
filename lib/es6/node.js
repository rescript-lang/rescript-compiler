


function test(x) {
  if (typeof x === "string") {
    return /* tuple */[
            /* String */0,
            x
          ];
  } else {
    return /* tuple */[
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
