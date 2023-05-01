var p = require("child_process");
var assert = require("assert");
var fs = require("fs");
var path = require("path");
var {rescript_exe} = require("../../../scripts/bin_path");

var {stdout} = p.execSync(rescript_exe, { cwd: __dirname });

// TODO: test deprecation warning
