

import Curry from "./curry.js";

function then_(arg1, obj) {
  return obj.then(Curry.__1(arg1));
}

function $$catch(arg1, obj) {
  return obj.catch(Curry.__1(arg1));
}

export default {
  then_ ,
  $$catch ,
}
/* No side effect */
