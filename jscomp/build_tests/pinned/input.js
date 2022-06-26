var cp = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
function checkSpawnOut(out) {
  if (out.error) {
    throw out.error;
  }
  if (out.status !== 0) {
    assert.fail(out.stderr + "\n" + out.stdout);
  }
}

// Clean beforehand to force its dependency to be rebuilt
var out = cp.spawnSync(`${rescript_exe} clean`, {
  encoding: "utf-8",
  shell: true,
});
checkSpawnOut(out);

var out = cp.spawnSync(`${rescript_exe} build`, {
  encoding: "utf-8",
  shell: true,
});
checkSpawnOut(out);

// In pinned mode, its dependency has warnings
assert.ok(
  out.stdout.split("\n").some((x) => x.includes("Warning 32: unused value"))
);

var out2 = cp.spawnSync(
  `${rescript_exe} build -- -C node_modules/test/lib/bs/ -t targets`,
  {
    encoding: "utf-8",
    shell: true,
  }
);
checkSpawnOut(out2);

// In pinned mode, generators are running
assert.ok(out2.stdout.split("\n").some((x) => x.endsWith("test.ml")));
