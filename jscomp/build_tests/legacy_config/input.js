var p = require("child_process");
var assert = require("assert");
var fs = require("fs");
var path = require("path");
var {rescript_exe} = require("../../../scripts/bin_path");

var out = p.spawnSync(rescript_exe, { cwd: __dirname });

// console.log(out);
// TODO: test deprecation warning
