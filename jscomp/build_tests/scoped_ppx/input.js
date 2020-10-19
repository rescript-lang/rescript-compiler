var cp = require("child_process");
var assert = require('assert')
cp.execSync(`bsb`, { cwd: __dirname, stdio: [0, 1, 2], encoding: "utf8" });

var output = cp.execSync(`bsb -- -t commands src/hello.ast`, {
  cwd: __dirname,
  encoding: "utf8"
});
assert(/-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(output))

