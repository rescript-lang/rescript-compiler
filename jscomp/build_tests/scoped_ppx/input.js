const cp = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });

const output = cp.execSync(
  `${rescript_exe} build -- -t commands src/hello.ast`,
  {
    cwd: __dirname,
    encoding: "utf8",
  },
);
assert(
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(
    output,
  ),
);
