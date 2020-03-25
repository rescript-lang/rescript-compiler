'use strict';

var Js_dict = require("./js_dict.js");
var Process = require("process");

function putEnvVar(key, $$var) {
  Process.env[key] = $$var;
  
}

function deleteEnvVar(s) {
  return Js_dict.unsafeDeleteKey(Process.env, s);
}

exports.putEnvVar = putEnvVar;
exports.deleteEnvVar = deleteEnvVar;
/* process Not a pure module */
