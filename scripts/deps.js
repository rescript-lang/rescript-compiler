#!/usr/bin/node
//@ts-check

var fs = require("fs");
var path = require("path");
var content = fs.readFileSync(
  path.join(__dirname, "..", "lib", "4.06.1", "unstable", "bspack.ml.d"),
  "utf8"
);

var files = content
  .split("\n")
  .filter(Boolean)
  .map((x) =>
    x
      .split(":")
      .map((x) => x.trim())
      .filter(Boolean)
      .at(-1)
  );

for (let file of files) {
  let { base, dir } = path.parse(file);
  // console.log(base, dir);
  if (dir.includes("ml") && !base.includes('rescript')) {
    continue;
  }
  let src = path.join(__dirname, "..", "jscomp", file);
  let dest = path.join(__dirname, "..", "..", "rescript-pack", base);
  console.log(`copy ${src} -> ${dest}`);
  fs.copyFileSync(src, dest);
}
