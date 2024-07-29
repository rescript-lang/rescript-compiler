const child_process = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

const out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
