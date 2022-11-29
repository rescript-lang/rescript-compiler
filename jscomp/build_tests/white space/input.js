var r = require("rollup");
var path = require("path");
var assert = require("assert");
var p = require("child_process");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
try {
  p.execSync(`${rescript_exe} build`, {
    cwd: __dirname,
    encoding: "utf8",
  });
  r.rollup({
    input: path.join(__dirname, "yy.js"),
  })
    .then((bundle) => {
      return bundle.generate({
        format: "iife",
        name: "X",
      });
    })
    .then((output) => {
      // console.log(output.code)
      assert.ok(output.code.length < 1000, "bundled success");
      p.execSync(`${rescript_exe} clean -with-deps`);
    });
} finally {
}
