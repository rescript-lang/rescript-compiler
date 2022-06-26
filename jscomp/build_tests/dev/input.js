//@ts-check
var assert = require("assert");
var path = require("path");
var p = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe
p.execSync(`${rescript_exe} build`, { cwd: __dirname, shell: true, encoding: "utf8" });
var u = require("./examples/test.js");
assert.equal(path.basename(u.v), "demo.mldemo.ml");
