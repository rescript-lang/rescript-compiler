//@ts-check

var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var out = cp.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
});

if (out.stdout !== "") {
  assert.fail(out.stdout);
} else {
  assert.equal(
    out.stderr,
    [
      'File "bsconfig.json", line 1',
      "Error: package weird not found or built ",
      "- Did you install it?",
      "",
    ].join("\n")
  );
}
