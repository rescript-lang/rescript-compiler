var child_process = require("child_process");
var assert = require("assert");

var out = child_process.spawnSync(`rescript`, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
