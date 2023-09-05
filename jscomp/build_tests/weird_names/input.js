var cp = require("child_process");
var assert = require("assert");
var path = require("path");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

var out = cp.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.stderr !== "") {
  assert.fail(out.stderr);
}

let files = [
  "_app.res",
  "[...params_max_3].res",
  "[...params].res",
  "[[...params]].res",
  "[slug_or_ID].res",
  "404.res",
  "utils.test.res",
];

for (let f of files) {
  let { name } = path.parse(f);
  let m = `./lib/js/src/${name}.js`;
  assert.deepEqual(require(m).a, 1);
}
