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
      .find((x) => x.startsWith("let version = "))
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

function buildCompiler() {
  // for 4.02.3 it relies on OCAMLLIB to find stdlib path
  // for 4.06.1 OCAMLLIB is another PATH
  // delete process.env.OCAMLLIB
  var prebuilt = "prebuilt.ninja";
  var content = require("./ninjaFactory.js").libNinja({
    ocamlopt: is_windows
      ? `ocamlopt.opt.exe`
      : `../native/${ocamlVersion}/bin/ocamlopt.opt`,
    INCL: ocamlVersion,
    isWin: is_windows,
  });

  fs.writeFileSync(path.join(root, "lib", prebuilt), content, "ascii");
  process.env.PATH = `${path.join(__dirname, "..", process.platform)}${
    path.delimiter
  }${process.env.PATH}`;
  let ninjaPath = `ninja.exe`;
  cp.execSync(
    `${ninjaPath} -C lib -f ${prebuilt} -v -t clean && ${ninjaPath} -v -C lib -f ${prebuilt}`,
    root_config
  );
}
if (!is_windows) {
  if (!process.argv.includes("-noclean")) {
    require("./pack").updateThemes();
    rebuild();
  }

  require("./ninja.js").updateRelease();
}

function createOCamlTar() {
  if (isHostPlatform()) {
    require("./installUtils.js").install();
    console.log(`status in ocaml submodule:`);
    cp.execSync(`git -C ocaml status -uno`, { cwd: root, stdio: [0, 1, 2] });
    cp.execSync(
      `git  -C ocaml archive --format=tar.gz HEAD -o ../vendor/ocaml.tar.gz`,
      { cwd: root, stdio: [0, 1, 2] }
    );
    console.log(`status in ninja submodule:`);
    cp.execSync(`git -C ninja status -uno`, { cwd: root, stdio: [0, 1, 2] });
    cp.execSync(
      `git -C ninja archive --format=tar.gz HEAD -o ../vendor/ninja.tar.gz`,
      { cwd: root, stdio: [0, 1, 2] }
    );
  }
}

createOCamlTar();
buildCompiler();
