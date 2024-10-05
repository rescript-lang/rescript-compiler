var cp = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });

var output = cp.execSync(`${rescript_exe} build -- -t commands src/hello.ast`, {
  cwd: __dirname,
  encoding: "utf8",
});

assert(
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(
    output,
  ),
);
