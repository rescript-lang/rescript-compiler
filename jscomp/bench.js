//@ts-check
var cp = require("child_process");

var output = cp.execSync(`ninja -t commands test/ocaml_typedtree_test.cmj`, {
  encoding: "utf8"
});

var toDO = output
  .split("\n")
  .filter(x => x.trim())
  .pop()
  .replace(".exe", ".darwin");

console.log(cp.execSync(`time ${toDO}`, { encoding: "utf8" }));

/*
  real    0m3.658s
user    0m3.562s
sys     0m0.089s
 */
