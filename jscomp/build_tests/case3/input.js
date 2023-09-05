//@ts-check

var p = require("child_process");
var fs = require("fs");
var path = require("path");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
p.spawnSync(`${rescript_exe} clean && ${rescript_exe} build`, {
  encoding: "utf8",
  cwd: __dirname,
});

var o = fs.readFileSync(path.join(__dirname, "src", "hello.bs.js"), "ascii");
assert.ok(/HelloGen\.f/.test(o));
