#!/usr/bin/env node

// @ts-check

// This script creates the list of the files that go into the rescript npm package.
//
// In local dev, invoke it without any args after adding or removing files.
// It will then recreate the list and make sure that the exes for all platforms
// are in the list, even if not present locally.
//
// In CI, it is invoked with -check. It then recreates the list and verifies
// that it has no changes compared to the committed state.

const { spawnSync, execSync } = require("node:child_process");
const path = require("node:path");
const fs = require("node:fs");

const isCheckMode = process.argv.includes("-check");

const rootPath = path.join(__dirname, "..");
const fileListPath = path.join(rootPath, "packages", "artifacts.txt");

/**
 * @typedef {{
 *   path: string,
 *   size: number,
 *   mode: number,
 * }} PackOutputFile
 * @typedef {{
 *   files: PackOutputFile[],
 *   entryCount: number,
 *   bundled: unknown[],
 * }} PackOutputEntry
 * @typedef {[PackOutputEntry]} PackOutput
 * @type {PackOutput}
 */
const output = JSON.parse(
  spawnSync("npm pack --dry-run --json", {
    cwd: rootPath,
    encoding: "utf8",
    shell: true,
  }).stdout,
);

const [{ files }] = output;
let filePaths = files.map(file => file.path);

if (!isCheckMode) {
  filePaths = Array.from(new Set(filePaths.concat(getFilesAddedByCI())));
}

filePaths.sort();
fs.writeFileSync(fileListPath, filePaths.join("\n"));

if (isCheckMode) {
  execSync(`git diff --exit-code ${fileListPath}`, { stdio: "inherit" });
}

function getFilesAddedByCI() {
  const platforms = ["darwin", "darwinarm64", "linux", "linuxarm64", "win32"];
  const exes = [
    "bsb_helper.exe",
    "bsc.exe",
    "ninja.exe",
    "rescript.exe",
    "rewatch.exe",
  ];

  const files = ["ninja.COPYING"];

  for (const platform of platforms) {
    for (const exe of exes) {
      files.push(`${platform}/${exe}`);
    }
  }

  return files;
}
