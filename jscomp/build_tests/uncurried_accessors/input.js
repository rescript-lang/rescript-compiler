var p = require("child_process");
var assert = require("assert");

var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
var o = p.spawnSync(rescript_exe, {
  encoding: "utf8",
  cwd: __dirname,
});

if (o.status !== 0) {
  assert.fail(
    "Deriving accessors should work in both curried and uncurried mode \n" +
      o.stdout
  );
}
