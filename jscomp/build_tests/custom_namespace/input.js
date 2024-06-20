var child_process = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");

child_process.execSync(`${rescript_exe} clean && ${rescript_exe} build`, {
  cwd: __dirname,
});

var x = require("./src/demo.bs.js");
assert.equal(x.v, 42);
