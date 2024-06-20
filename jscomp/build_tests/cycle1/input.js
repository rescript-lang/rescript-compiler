//@ts-check
const cp = require("child_process");
const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { rescript_exe } = require("#cli/bin_path");

cp.execSync(`${rescript_exe} clean`, { cwd: __dirname });

var output = cp.spawnSync(rescript_exe, { encoding: "utf8", shell: true });

assert(/is dangling/.test(output.stdout));

var compilerLogFile = path.join(__dirname, "lib", "bs", ".compiler.log");
var compilerLog = fs.readFileSync(compilerLogFile, "utf8");
assert(/is dangling/.test(compilerLog));
