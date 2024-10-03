var p = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");
try {
  var output = p.spawnSync(`${rescript_exe} build -regen`, {
    shell: true,
    encoding: "utf8",
  });

  assert.ok(output.stderr.match(/reserved package name/));
} finally {
}
