var child_process = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var output = child_process.spawnSync(
  `${rescript_exe} -with-deps && ${rescript_exe} build`,
  { cwd: __dirname, shell: true, encoding: "utf8" }
);

var assert = require("assert");
assert.ok(output.stderr.match(/not an existing module/));
