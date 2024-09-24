#!/usr/bin/env node
//@ts-check

const path = require("path");
const fs = require("fs");
const assert = require("assert");

const package_config = require(path.join(__dirname, "..", "package.json"));
const bsVersion = fs.readFileSync(
  path.join(__dirname, "..", "jscomp", "common", "bs_version.ml"),
  "utf-8",
);

/**
 * @param {string} bsVersion
 * @param {string} version
 */
function verifyVersion(bsVersion, version) {
  try {
    let [major, minor] = bsVersion
      .split("\n")
      .find(x => x.startsWith("let version = "))
      .split("=")[1]
      .trim()
      .slice(1, -1)
      .split(".");
    let [specifiedMajor, specifiedMinor] = version.split(".");
    console.log(
      `Version check: package.json: ${specifiedMajor}.${specifiedMinor} vs ABI: ${major}.${minor}`,
    );
    return major === specifiedMajor && minor === specifiedMinor;
  } catch (e) {
    return false;
  }
}

/**
 *
 * @param {string} src
 * @param {(file:string)=>boolean} filter
 * @param {string} dest
 */
function installDirBy(src, dest, filter) {
  fs.readdir(src, function (err, files) {
    if (err === null) {
      files.forEach(function (file) {
        if (filter(file)) {
          var x = path.join(src, file);
          var y = path.join(dest, file);
          // console.log(x, '----->', y )
          fs.copyFile(x, y, err => assert.equal(err, null));
        }
      });
    } else {
      throw err;
    }
  });
}

function populateLibDir() {
  const root_dir = path.join(__dirname, "..");
  const lib_dir = path.join(root_dir, "lib");
  const jscomp_dir = path.join(root_dir, "jscomp");
  const runtime_dir = path.join(jscomp_dir, "runtime");
  const others_dir = path.join(jscomp_dir, "others");
  const ocaml_dir = path.join(lib_dir, "ocaml");

  if (!fs.existsSync(ocaml_dir)) {
    fs.mkdirSync(ocaml_dir);
  }
  // for merlin or other IDE
  var installed_suffixes = [
    ".ml",
    ".mli",
    ".res",
    ".resi",
    ".cmi",
    ".cmj",
    ".cmt",
    ".cmti",
  ];
  installDirBy(runtime_dir, ocaml_dir, function (file) {
    var y = path.parse(file);
    return installed_suffixes.includes(y.ext);
  });
  installDirBy(others_dir, ocaml_dir, file => {
    var y = path.parse(file);
    if (y.ext === ".cmi") {
      return !y.base.match(/Belt_internal/i);
    }
    return installed_suffixes.includes(y.ext) && !y.name.endsWith(".cppo");
  });
}

assert(verifyVersion(bsVersion, package_config.version));

populateLibDir();
