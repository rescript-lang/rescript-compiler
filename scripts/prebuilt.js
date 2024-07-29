#!/usr/bin/env node

// @ts-check

const path = require("node:path");
const fs = require("node:fs");
const assert = require("node:assert");
const semver = require("semver");
const { compilerVersionFile } = require("#internal/paths");
const { packageJson } = require("#internal/meta");

/**
 * @param {semver.SemVer} bsVersion
 * @param {semver.SemVer} version
 */
function verifyVersion(bsVersion, version) {
  const { major, minor } = bsVersion;
  const { major: specifiedMajor, minor: specifiedMinor } = version;
  console.log(
    `Version check: package.json: ${specifiedMajor}.${specifiedMinor} vs ABI: ${major}.${minor}`,
  );
  return major === specifiedMajor && minor === specifiedMinor;
}

/**
 *
 * @param {string} src
 * @param {(file:string)=>boolean} filter
 * @param {string} dest
 */
function installDirBy(src, dest, filter) {
  fs.readdir(src, (err, files) => {
    if (err === null) {
      for (const file of files) {
        if (filter(file)) {
          const x = path.join(src, file);
          const y = path.join(dest, file);
          fs.copyFile(x, y, err => assert.equal(err, null));
        }
      }
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
  const stdlib_dir = path.join(jscomp_dir, "stdlib-406");

  if (!fs.existsSync(ocaml_dir)) {
    fs.mkdirSync(ocaml_dir);
  }

  installDirBy(runtime_dir, ocaml_dir, file => {
    const y = path.parse(file);
    return y.name === "js";
  });

  // for merlin or other IDE
  const installed_suffixes = [
    ".ml",
    ".mli",
    ".res",
    ".resi",
    ".cmi",
    ".cmj",
    ".cmt",
    ".cmti",
  ];
  installDirBy(others_dir, ocaml_dir, file => {
    const y = path.parse(file);
    if (y.ext === ".cmi") {
      return !y.base.match(/Belt_internal/i);
    }
    return installed_suffixes.includes(y.ext) && !y.name.endsWith(".cppo");
  });
  installDirBy(stdlib_dir, ocaml_dir, file => {
    const y = path.parse(file);
    return installed_suffixes.includes(y.ext);
  });
}

const bsVersionPattern = /let version = "(?<version>.*)"/m;
const bsVersionFileContent = fs.readFileSync(compilerVersionFile, "utf-8");
const bsVersionMatch = bsVersionFileContent.match(bsVersionPattern)?.groups;
assert.ok(bsVersionMatch, "Failed to parse the compiler version file");

const bsVersion = semver.parse(bsVersionMatch.version);
assert.ok(bsVersion, "Failed to parse the compiler version file");

const version = semver.parse(packageJson.version);
assert.ok(version, "Failed to parse the version of the package.json");

assert.ok(
  verifyVersion(bsVersion, version),
  "Please bump versions to be matched",
);

populateLibDir();
