#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var { is_windows } = require("./config.js");

var root = path.join(__dirname, "..");
var root_config = { cwd: root, stdio: [0, 1, 2], encoding: "utf8" };

var ocamlVersion = require("./buildocaml.js").getVersionPrefix();
var fs = require("fs");
var package_config = require(path.join(__dirname, "..", "package.json"));
var bsVersion = fs.readFileSync(
  path.join(__dirname, "..", "jscomp", "common", "bs_version.ml"),
  "utf-8"
);
/**
 *
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
      `package.json ${specifiedMajor}.${specifiedMinor} vs ABI : ${major}.${minor}`
    );
    return major === specifiedMajor && minor === specifiedMinor;
  } catch (e) {
    return false;
  }
}
var assert = require("assert");

assert(verifyVersion(bsVersion, package_config.version));

function isHostPlatform() {
  return process.platform === "darwin" || process.platform === "linux";
}
function rebuild() {
  cp.execSync(`node ${path.join(__dirname, "ninja.js")} cleanbuild`, {
    cwd: __dirname,
    stdio: [0, 1, 2],
  });
}

var use_env_compiler = process.argv.includes("-use-env-compiler");
var bin_path = require("./bin_path").absolutePath;

function buildCompiler() {
  var prebuilt = "prebuilt.ninja";
  var content = require("./ninjaFactory.js").libNinja({
    ocamlopt: use_env_compiler
      ? `ocamlopt.opt`
      : `../native/${ocamlVersion}/bin/ocamlopt.opt`,
    INCL: "4.06.1",
    isWin: is_windows,
  });

  fs.writeFileSync(path.join(root, "lib", prebuilt), content, "ascii");
  // This is for ninja access
  process.env.PATH = `${bin_path}${path.delimiter}${process.env.PATH}`;
  let ninjaPath = `ninja.exe`;
  console.log(`start build`);
  cp.execSync(
    `${ninjaPath} -C lib -f ${prebuilt} -v -t clean && ${ninjaPath} -v -C lib -f ${prebuilt}`,
    root_config
  );
  console.log("compiler built done");
}
if (!is_windows) {
  if (!process.argv.includes("-no-clean")) {
    require("./pack").updateThemes();
    rebuild();
    require("./ninja.js").updateRelease();
  }
}

function createNinjaTar() {
  if (isHostPlatform()) {
    require("./installUtils.js").install();
    console.log(`status in ninja submodule:`);
    cp.execSync(`git -C ninja status -uno`, { cwd: root, stdio: [0, 1, 2] });
    cp.execSync(
      `git -C ninja archive --format=tar.gz HEAD -o ../vendor/ninja.tar.gz`,
      { cwd: root, stdio: [0, 1, 2] }
    );
  }
}

createNinjaTar();
buildCompiler();
