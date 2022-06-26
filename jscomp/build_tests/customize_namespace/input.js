//@ts-check

var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
cp.execSync(`${rescript_exe} build`, { cwd: __dirname, encoding: "utf8" });

assert.equal(require("./src/demo.bs").v, 5);
