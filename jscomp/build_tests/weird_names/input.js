var cp = require("child_process");
var assert = require("assert");
var path = require("path");

var out = cp.spawnSync(`bsb`, { encoding: "utf8" });

console.log(out.stdout);
if(out.stderr !== ""){
  assert.fail(out.stderr)
}


let files = [
  "_app.res",
  "[...params_max_3].res",
  "[...params].res",
  "[[...params]].res",
  "[slug_or_ID].res",
  "404.res",
];

for (let f of files) {
  let { name } = path.parse(f);
  let m = `./lib/js/src/${name}.js`;
  //   console.log(m);
  assert.deepEqual(require(m).a,  1);
}
