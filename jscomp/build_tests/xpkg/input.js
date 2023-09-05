var p = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
try {
  var output = p.spawnSync(`${rescript_exe} build -regen`, {
    shell: true,
    encoding: "utf8",
  });

  assert.ok(output.stderr.match(/reserved package name/));
} finally {
}
