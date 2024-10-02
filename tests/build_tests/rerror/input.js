// @ts-check
var child_process = require("child_process");
var assert = require("assert");
var os = require("os");
var { rescript_exe } = require("#cli/bin_path");

child_process.spawnSync(`${rescript_exe} clean`, {
  cwd: __dirname,
  encoding: "utf8",
});

var o = child_process.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
  shell: true,
});

// verify the output is in reason syntax
var u = o.stdout.match(/=>/g);

var lines = o.stdout
  .split(/\r?\n/)
  .map(x => x.trim())
  .filter(Boolean);

var test = false;
for (var i = 0; i < lines.length; ++i) {
  if (lines[i] === "We've found a bug for you!") {
    console.log(`line ${i} found`);
    assert.ok(/src[\\/]demo.res:1:21-23/.test(lines[i + 1]));
    test = true;
  }
}
assert.ok(test);
assert.ok(u.length === 2);
