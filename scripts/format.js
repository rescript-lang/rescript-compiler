//@ts-check
var fs = require("fs");
var path = require("path");
var child_process = require("child_process");
/**
 *
 * @param {string} str
 * @returns
 */
function hasConditionalCompilation(str) {
  return str.includes("#if");
}

var sourceDirs = [
  "stubs",
  "ext",
  "common",
  //   "js_parser",
  "frontend",
  "depends",
  "core",
  //   "super_errors",
  "outcome_printer",
  "bsb",
  "bsb_helper",
  //   "ounit",
  //   "napkin",
  //   "ounit_tests",
  "main",
];
var skips = ["dataset", "ident"];
// ident comes from upstream
for (let dir of sourceDirs) {
  let dir1 = path.join(__dirname, "..", "jscomp", dir);
  let files = fs.readdirSync(dir1);
  for (let file of files) {
    if (
      (file.endsWith(".ml") || file.endsWith(".mli")) &&
      !skips.some(x => file.includes(x))
    ) {
      let source = path.join(dir1, file);
      let content = fs.readFileSync(source, "utf-8");
      if (hasConditionalCompilation(content)) {
        // console.log(`${source} has conditional compilation`);
      } else {
        var command = `ocamlformat -i ${source}`;
        // console.log(`running ${command}`);
        var out = child_process.spawnSync(command, {
          encoding: "utf-8",
          shell: true,
        });
        if (out.stderr) {
          console.log(out.stderr);
        }
      }
    }
  }
}
