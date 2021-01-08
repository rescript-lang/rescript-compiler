

import * as Js_dict from "./js_dict.mjs";
import * as Process from "process";

function putEnvVar(key, $$var) {
  Process.env[key] = $$var;
  
}

function deleteEnvVar(s) {
  return Js_dict.unsafeDeleteKey(Process.env, s);
}

export {
  putEnvVar ,
  deleteEnvVar ,
  
}
/* process Not a pure module */
