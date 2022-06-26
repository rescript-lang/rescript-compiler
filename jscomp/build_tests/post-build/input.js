var child_process = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
