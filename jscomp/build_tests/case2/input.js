var p = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
var o = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });

if (
  ![
    `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/X vs src/x\n`,
    // On linux files are parsed in different order
    `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/x vs src/X\n`,
  ].includes(o.stderr)
) {
  assert.fail(o.stderr);
}
