const { spawnSync } = require("child_process");
const assert = require("assert");
const rescript_exe = require("../../../scripts/bin_path").rescript_exe;

const output = spawnSync(rescript_exe, { encoding: "utf8" });
assert(
  /^Warning: bsconfig.json is deprecated. Migrate it to rescript.json/.test(
    output.stdout
  )
);
