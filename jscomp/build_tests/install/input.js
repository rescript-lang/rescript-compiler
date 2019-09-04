var p = require("child_process");
var fs = require("fs");
var path = require("path");
var assert = require("assert");

p.spawnSync(`bsb`, [`-clean`], {
  encoding: "utf8",
  cwd: __dirname,
  stdio: [0, 1, 2]
});
p.spawnSync(`bsb`, [`-install`], {
  encoding: "utf8",
  cwd: __dirname,
  stdio: [0, 1, 2]
});

var fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists == false);

p.spawnSync(`bsb`, {
  encoding: "utf8",
  cwd: __dirname,
  stdio: [0, 1, 2]
});
p.spawnSync(`bsb` ,[`-install`], {
  encoding: "utf8",
  cwd: __dirname,
  stdio: [0, 1, 2]
});

fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists);