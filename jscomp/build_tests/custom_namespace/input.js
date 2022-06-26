var child_process = require("child_process");
var fs = require("fs");
var path = require("path");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

child_process.execSync(`${rescript_exe} clean -with-deps && ${rescript_exe} build`, {
  cwd: __dirname,
  stdio: [0, 1, 2],
});

var x = require("./src/demo.bs.js");
assert.equal(x.v, 42);

var merlin = fs.readFileSync(path.join(__dirname, ".merlin"), "utf8");
assert.ok(merlin.includes("-open Foo_bar"));
