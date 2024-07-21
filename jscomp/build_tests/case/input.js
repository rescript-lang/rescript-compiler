const p = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
const o = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });

if (
  ![
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/demo vs src/Demo\n",
    // On linux files are parsed in different order
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/Demo vs src/demo\n",
  ].includes(o.stderr)
) {
  assert.fail(o.stderr);
}
