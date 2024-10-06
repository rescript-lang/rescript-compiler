#!/usr/bin/env node
//@ts-check

const path = require("path");
const fs = require("fs");
const assert = require("assert");

const package_config = require(path.join(__dirname, "..", "package.json"));
const bsVersion = fs.readFileSync(
  path.join(__dirname, "..", "compiler", "common", "bs_version.ml"),
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

assert(verifyVersion(bsVersion, package_config.version));
