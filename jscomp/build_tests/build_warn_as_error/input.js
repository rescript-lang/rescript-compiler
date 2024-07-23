var p = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var o1 = p.spawnSync(rescript_exe, ["build"], {
  encoding: "utf8",
  cwd: __dirname,
});

var first_message = o1.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s == "Warning number 110");

if (!first_message) {
  assert.fail(o1.stdout);
}

// Second build using -warn-error +110
var o2 = p.spawnSync(rescript_exe, ["build", "-warn-error", "+110"], {
  encoding: "utf8",
  cwd: __dirname,
});

var second_message = o2.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s == "Warning number 110 (configured as error)");

if (!second_message) {
  assert.fail(o2.stdout);
}

// Third build, without -warn-error +110
// The result should not be a warning as error
var o3 = p.spawnSync(rescript_exe, ["build"], {
  encoding: "utf8",
  cwd: __dirname,
});

var third_message = o3.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s == "Dependency Finished");

if (!third_message) {
  assert.fail(o3.stdout);
}

var cleanup = p.spawnSync(rescript_exe, ["clean"], {
  encoding: "utf8",
  cwd: __dirname,
});
