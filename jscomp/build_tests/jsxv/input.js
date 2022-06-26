//@ts-check

var assert = require("assert");

var p = require("child_process");

var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var output = p.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
  shell: true,
});

assert.ok(output.stderr.match(/File ".*bsconfig.json", line 10/));
