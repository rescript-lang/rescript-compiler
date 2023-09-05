var p = require("child_process");
var fs = require("fs");
var path = require("path");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

p.spawnSync(rescript_exe, [`clean`], {
  encoding: "utf8",
  cwd: __dirname,
});
p.spawnSync(rescript_exe, [`build`, `-install`], {
  encoding: "utf8",
  cwd: __dirname,
});

var fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists == false);

p.spawnSync(rescript_exe, {
  encoding: "utf8",
  cwd: __dirname,
});
p.spawnSync(rescript_exe, [`build`, `-install`], {
  encoding: "utf8",
  cwd: __dirname,
});

fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists);
