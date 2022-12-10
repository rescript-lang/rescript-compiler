#!/usr/bin/env node
// Copy exes built by dune to platform bin dir

const path = require("path");
const fs = require("fs");
const child_process = require("child_process");

const { absolutePath: platformBinDir, ninja_exe } = require("./bin_path");
const ninjaDir = path.join(__dirname, "..", "ninja");
const duneBinDir = path.join(
  __dirname,
  "..",
  "_build",
  "install",
  "default",
  "bin"
);

if (!fs.existsSync(platformBinDir)) {
  fs.mkdirSync(platformBinDir);
}

function copyExe(dir, exe) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(dir, exe + ext);
  const dest = path.join(platformBinDir, exe + ".exe");

  // For some reason, the copy operation fails in Windows CI if the file already exists.
  if (process.platform === "win32" && fs.existsSync(dest)) {
    fs.rmSync(dest);
  }

  fs.copyFileSync(src, dest);

  if (process.platform !== "win32") {
    child_process.execSync(`strip ${dest}`);
  }
}

copyExe(duneBinDir, "rescript");
copyExe(duneBinDir, "bsc");
copyExe(duneBinDir, "bsb_helper");
copyExe(ninjaDir, "ninja");
