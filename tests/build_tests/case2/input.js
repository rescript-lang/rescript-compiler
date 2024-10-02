var p = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");
var { normalizeNewlines } = require("../utils.js");

var o = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });

if (
  ![
    `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/X vs src/x\n`,
    // Windows: path separator
    `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src\\X vs src\\x\n`,
    // Linux: files are parsed in different order
    `Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/x vs src/X\n`,
  ].includes(normalizeNewlines(o.stderr))
) {
  assert.fail(o.stderr);
}
