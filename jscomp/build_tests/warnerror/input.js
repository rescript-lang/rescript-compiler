var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var out = cp.spawnSync(`${rescript_exe} build`, {
  encoding: "utf-8",
  shell: true,
});
if (out.error) {
  throw out.error;
}
if (out.status !== 0) {
  assert.fail(out.stderr + "\n" + out.stdout);
}
