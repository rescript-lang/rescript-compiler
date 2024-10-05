var child_process = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

var out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
