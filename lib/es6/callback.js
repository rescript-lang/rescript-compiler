

import * as Obj from "./obj.js";

function register(name, v) {
  return /* () */0;
}

function register_exception(name, exn) {
  (exn.tag | 0) === Obj.object_tag ? exn : exn[0];
  return /* () */0;
}

export {
  register ,
  register_exception ,
  
}
/* No side effect */
