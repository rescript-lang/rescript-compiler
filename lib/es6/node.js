


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

var Path = /* alias */0;

var Fs = /* alias */0;

var Process = /* alias */0;

var Module = /* alias */0;

var $$Buffer = /* alias */0;

var Child_process = /* alias */0;

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
