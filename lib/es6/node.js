'use strict';


function test(x) {
  if (typeof x === "string") {
    return /* tuple */[
            /* String */0,
            x
          ];
  }
  else {
    return /* tuple */[
            /* Buffer */1,
            x
          ];
  }
}

var Path = 0;

var Fs = 0;

var Process = 0;

var Module = 0;

var Buffer = 0;

var Child_process = 0;

export {
  Path          ,
  Fs            ,
  Process       ,
  Module        ,
  Buffer        ,
  Child_process ,
  test          ,
  
}
/* No side effect */
