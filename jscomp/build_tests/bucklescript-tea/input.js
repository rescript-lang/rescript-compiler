var p = require("child_process");
const { assert } = require("console");

var o = p.spawnSync(`rescript`);

console.log(o.stderr + "");
console.log("-----");
console.log(o.stdout + "");
assert(o.status === 0)