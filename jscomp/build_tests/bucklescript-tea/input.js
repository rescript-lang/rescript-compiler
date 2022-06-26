var p = require("child_process");
const { assert } = require("console");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe

var o = p.spawnSync(rescript_exe);

console.log(o.stderr + "");
console.log("-----");
console.log(o.stdout + "");
assert(o.status === 0)
