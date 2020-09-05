var child_process = require("child_process");
var assert = require("assert");
child_process.spawnSync(`bsb -clean-world`, {
  cwd: __dirname,
  encoding: "utf8",
  stdio: [0, 1, 2]
});
var o = child_process.spawnSync(`bsb `, {
  cwd: __dirname,
  encoding: "utf8",
  shell: true
});

// verify the output is in reason syntax
var u = o.stdout.match(/=>/g);

var lines = o.stdout
  .split("\n")
  .map(x => x.trim())
  .filter(Boolean);
// console.log(`lines: \n${lines}`)
// console.log(lines[4])
var test = false;
for (var i = 0; i < lines.length; ++i) {
  if (lines[i] === "We've found a bug for you!") {
    console.log(`line ${i} found`);
    assert.ok(/src\/demo.re:3:23-25/.test(lines[i + 1]));
    test = true;
  }
}
assert.ok(test);
assert.ok(u.length === 2);
