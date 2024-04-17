const child_process = require("child_process");
const assert = require("assert");
const rescript_exe = require("../../../scripts/bin_path").rescript_exe;

const out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });
assert.match(
  out.stderr,
  /deprecated: Option "es6-global" is deprecated\. Use "esmodule" instead\./
);
