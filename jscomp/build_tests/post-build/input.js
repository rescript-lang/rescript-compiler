var child_process = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");

var out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
