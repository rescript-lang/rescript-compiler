var p = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path.js");

var o = p.spawnSync(rescript_exe, ["build", "-warn-error", "+110"], {
  encoding: "utf8",
  cwd: __dirname,
});

var error_message = o.stdout
  .split("\n")
  .map(s => s.trim())
  .includes("Warning number 110 (configured as error)");

if (!error_message) {
  assert.fail(o.stdout);
}
