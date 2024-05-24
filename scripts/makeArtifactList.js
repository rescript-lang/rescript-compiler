#!/usr/bin/env node

// This script creates the list of the files that go into the rescript npm package.
//
// In local dev, invoke it without any args after adding or removing files.
// It will then recreate the list and make sure that the exes for all platforms
// are in the list, even if not present locally.
//
// In CI, it is invoked with -check. It then recreates the list and verifies
// that it has no changes compared to the committed state.

const { spawnSync, execSync } = require("child_process");
const path = require("path");
const fs = require("fs");

const isCheckMode = process.argv.includes("-check");

const rootPath = path.join(__dirname, "..");
const fileListPath = path.join(rootPath, "packages", "artifacts.txt");

const output = spawnSync(`npm pack --dry-run`, {
  cwd: rootPath,
  encoding: "utf8",
  shell: true,
}).stderr;

let files = output
  .slice(
    output.indexOf("Tarball Contents"),
    output.lastIndexOf("npm notice === Tarball Details ===")
  )
  .split("\n")
  .map(x => x.split(" ").filter(Boolean))
  .filter(x => x[0] === "npm" && x[1] === "notice")
  .map(x => x[x.length - 1]);

if (!isCheckMode) {
  files = Array.from(new Set(files.concat(getFilesAddedByCI())));
}

files.sort();
fs.writeFileSync(fileListPath, files.join("\n"));

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

  for (let platform of platforms) {
    for (let exe of exes) {
      files.push(`${platform}/${exe}`);
    }
  }

  return files;
}
