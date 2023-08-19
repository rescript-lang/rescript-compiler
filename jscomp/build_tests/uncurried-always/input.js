//@ts-check
const cp = require("child_process");
const assert = require("assert");
const fs = require('fs')
const path = require('path')
var rescript_exe = require("../../../scripts/bin_path").rescript_exe

cp.execSync(`${rescript_exe} clean`, { cwd: __dirname, });
cp.execSync(`${rescript_exe}`, { cwd: __dirname, });
