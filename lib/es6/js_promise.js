

import * as Curry from "./curry.js";

function then_(arg1, arg0) {
  return arg0.then(Curry.__1(arg1));
}

function $$catch(arg1, arg0) {
  return arg0.catch(Curry.__1(arg1));
}

export {
  then_ ,
  $$catch ,
  
}
/* No side effect */
