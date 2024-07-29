//@ts-check
const child_process = require("node:child_process");
const { rescript_exe } = require("#cli/bin_path");

console.log(child_process.execSync(rescript_exe, { encoding: "utf8" }));

const fs = require("node:fs");
const path = require("node:path");
const content = `${fs.readFileSync(path.join(__dirname, "lib", "bs", ".sourcedirs.json"))}`;

const assert = require("node:assert");

assert(JSON.parse(content).dirs.some(x => x.includes("📕annotation")));
