'use strict';

import * as Obj from "./obj";

function register(_, _$1) {
  return /* () */0;
}

function register_exception(_, exn) {
  (exn.tag | 0) === Obj.object_tag ? exn : exn[0];
  return /* () */0;
}

export{
  register           ,
  register_exception ,
  
}
/* No side effect */
