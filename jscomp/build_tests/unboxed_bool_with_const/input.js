//@ts-check

var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var out = cp.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
});

assert.equal(
  out.stdout.slice(out.stdout.indexOf("FAILED: src/Main.cmj")),
  `FAILED: src/Main.cmj

  We've found a bug for you!
  /Users/dzakh/code/DZakh-forks/rescript-compiler/jscomp/build_tests/unboxed_bool_with_const/src/Main.res:3:3-14

  1 │ @unboxed
  2 │ type t<'a> =
  3 │   | Bool(bool)
  4 │   | @as(false) False
  5 │   | @as(true) True

  This untagged variant definition is invalid: At most one case can be a boolean type.

FAILED: cannot make progress due to previous errors.
`
);
