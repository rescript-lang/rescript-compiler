'use strict';

var Child_process = require("child_process");

function caml_sys_system_command(cmd) {
  var match = Child_process.spawnSync(cmd, ( {"shell": true, "stdio": "inherit"} )).status;
  if (match !== null) {
    return match;
  } else {
    return 127;
  }
}

exports.caml_sys_system_command = caml_sys_system_command;
/* child_process Not a pure module */
