#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");
var opt = path.join(__dirname, "..", "native", "4.06.1", "bin", "ocamlopt.opt");

// TOOD: share with ninja.js
// check compiler code base
var sourceDirs = [
  "stubs",
  "ext",
  "common",
  "js_parser",
  "syntax",
  "depends",
  "core",
  "super_errors",
  "outcome_printer",
  "bsb",
  "bsb_helper",
  "ounit",
  "ounit_tests",
  "main"
];
var fileMap = new Map();
for (let dir of sourceDirs) {
  let ydir = path.join("jscomp", dir);
  let xdir = path.join(__dirname, "..", ydir);
  for (let file of fs.readdirSync(xdir, "utf8")) {
    let p = path.parse(file);
    if (p.ext === ".ml" || p.ext === ".mli") {
      fileMap.set(p.base, ydir);
    }
  }
}

/**
 * 
 * @param {string} file 
 */
function check(file) {
  var output = cp.spawnSync(
    `${opt} -c  -opaque -linscan -I +compiler-libs -I 4.06.1 -w a+32 4.06.1/${file}.mli 4.06.1/${file}.ml`,
    { cwd: path.join(__dirname, "..", "lib"), encoding: "utf8", shell: true }
  );

  // debugger

  var result = output.stderr.replace(/File "(.*)"/g, (file, p1) => {
    let query = fileMap.get(p1);
    if (!query) {
      return `Unkonwn file ${p1}`;
    }
    return `File "${query}/${p1}"`;
  });

  console.log(result);
}

// 
console.log(process.argv)
check(process.argv[2])