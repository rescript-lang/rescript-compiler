//@ts-check
const cp = require("child_process");
const assert = require("assert");
const fs = require('fs')
const path = require('path')

var output = cp.spawnSync(`../node_modules/.bin/rescript`, { encoding: "utf8", shell: true });

assert(/is dangling/.test(output.stdout));

var compilerLogFile = path.join(__dirname, 'lib', 'bs', '.compiler.log');
var compilerLog = fs.readFileSync(compilerLogFile, 'utf8');
assert(/is dangling/.test(compilerLog));