#!/usr/bin/env node
//@ts-check
var p = require("child_process");
var path = require("path");
var fs = require("fs");
var assert = require("assert");
var root = path.join(__dirname, "..");

var output = p.spawnSync(`npm pack --dry-run`, {
  cwd: root,
  encoding: "utf8",
  shell: true,
  // stdio: [0, 1, 2]
});

/**
 *
 * @param {string} output
 */
function parseOutput(output) {
  var publishedFiles = output
    .slice(
      output.indexOf("Tarball Contents"),
      output.lastIndexOf("npm notice === Tarball Details ===")
    )
    .split("\n")
    .map((x) => x.split(" ").filter(Boolean))
    .filter((x) => x[0] === "npm" && x[1] === "notice")
    .map((x) => x[x.length - 1]);
  return publishedFiles;
}

var packedFiles = parseOutput(output.stderr);

/**
 *
 * @param {string[]} files
 */
function stat(files) {
  /**
   * @type Map<string,Set<string> >
   */
  var map = new Map();
  for (let f of files) {
    let p = path.parse(f);
    if (map.get(p.dir) === undefined) {
      map.set(p.dir, new Set([p.base]));
    } else {
      map.get(p.dir).add(p.base);
    }
  }
  // for (let [k, v] of map) {
  //   console.log(`dir: ${k} \t=>\t ${v.size}`);
  // }
  return map;
}

/**
 *
 * @param {Map<string, Set<string> >} map
 */
function check(map) {
  // we don't need check artifacts any more
  // since it's already snapshot

  // make sure the remote and current are on the same commit
  var currentBranch = (p.execSync(`git rev-parse --abbrev-ref HEAD`) + "").trim();
  var command = `git fetch origin && git diff ${currentBranch} origin/${currentBranch}`;
  console.log(`Running '${command}'`);
  var remoteDiffs = p.execSync(command) + "";
  if (remoteDiffs) {
    console.warn(`diffs with remote`);
    console.log(remoteDiffs);
  } else {
    console.log(`remote diffs looking good`);
  }
}

var map = stat(packedFiles);
/**
 *
 * @param {Map<string,Set<string> >} map
 */
function toJSON(map) {
  var o = {};
  var keys = [...map.keys()].sort();
  for (let k of keys) {
    // @ts-ignore
    o[k] = [...map.get(k)];
  }
  return o;
}

fs.writeFileSync(
  path.join(__dirname, "..", "jscomp", "artifacts.json"),
  JSON.stringify(toJSON(map), undefined, 2),
  "utf8"
);

if (!process.argv.includes("-nocheck")) {
  check(map);
}

var output = p.spawnSync(`git diff jscomp/artifacts.json`, {
  cwd: root,
  encoding: "utf8",
  shell: true,
});

assert(output.status === 0);
console.log("The diff of artifacts", output.stdout);
