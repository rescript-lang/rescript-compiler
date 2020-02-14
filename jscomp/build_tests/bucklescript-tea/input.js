var p = require("child_process");

var o = p.spawnSync(`bsb`);

console.log(o.stderr + "");
console.log("-----");
console.log(o.stdout + "");
if (o.error) {
  throw o.error;
}
