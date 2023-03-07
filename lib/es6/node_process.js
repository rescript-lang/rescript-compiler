

import Js_dict from "./js_dict.js";
import Process from "process";

function putEnvVar(key, $$var) {
  Process.env[key] = $$var;
}

function deleteEnvVar(s) {
  Js_dict.unsafeDeleteKey(Process.env, s);
}

export default {
  putEnvVar ,
  deleteEnvVar ,
}
/* process Not a pure module */
