var child_process = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

child_process.execSync(`${rescript_exe} clean && ${rescript_exe} build`, {
  cwd: __dirname,
});

var x = require("./src/demo.bs.js");
assert.equal(x.v, 42);
