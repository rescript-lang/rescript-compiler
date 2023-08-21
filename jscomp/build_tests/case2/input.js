var p = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
var o = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });

assert.equal(
  o.stderr,
  `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/X vs src/x\n`
);
