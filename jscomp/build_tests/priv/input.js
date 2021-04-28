var child_process = require("child_process");

var output = child_process.spawnSync(
  `rescript clean -with-deps && rescript build`,
  { cwd: __dirname, shell: true, encoding: "utf8" }
);

var assert = require("assert");
assert.ok(output.stderr.match(/not an existing module/));
