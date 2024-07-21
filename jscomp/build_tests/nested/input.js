const p = require("node:child_process");
const assert = require("node:assert");
const fs = require("node:fs");
const path = require("node:path");
const { rescript_exe } = require("#cli/bin_path");
p.execSync(rescript_exe, { cwd: __dirname });

const content = fs.readFileSync(path.join(__dirname, "src", "demo.js"), "utf8");

assert.ok(content.match(/A00_a1_main/g).length === 3);
assert.ok(content.match(/B00_b1_main/g).length === 3);
assert.ok(content.match(/A0_main/g).length === 2);
assert.ok(content.match(/a0_main/g).length === 1);
assert.ok(content.match(/B0_main/g).length === 2);
assert.ok(content.match(/b0_main/g).length === 1);

assert.ok(require("./src/demo.js").v === 4, "nested");
