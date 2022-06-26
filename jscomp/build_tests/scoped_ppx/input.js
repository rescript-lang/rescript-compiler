var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });

var output = cp.execSync(`${rescript_exe} build -- -t commands src/hello.ast`, {
  cwd: __dirname,
  encoding: "utf8",
});
assert(
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(
    output
  )
);
