//@ts-check
var cp = require("child_process");
var path = require("path");
var assert = require("assert");
var output = cp.execSync(`bsb -- -t targets`, {
  encoding: "utf8",
  cwd: __dirname
});

var cmjTargets = output
  .split("\n")
  .filter(Boolean)
  .map(x => x.split(":")[0])
  .filter(x => x.endsWith(".cmj"))
  .map(x => path.basename(x))
  .sort();

assert.deepEqual(cmjTargets, ["demo.cmj", "hello00.cmj","hexll.cmj"]);


