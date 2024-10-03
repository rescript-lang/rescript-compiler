//@ts-check
const cp = require("child_process");
const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { rescript_exe } = require("#cli/bin_path");

const output = cp.spawnSync(rescript_exe, { encoding: "utf8", shell: true });
assert(/M is exported twice/.test(output.stdout));

const compilerLogFile = path.join(__dirname, "lib", "bs", ".compiler.log");
const compilerLog = fs.readFileSync(compilerLogFile, "utf8");
assert(/M is exported twice/.test(compilerLog));
