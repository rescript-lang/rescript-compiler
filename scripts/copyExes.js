#!/usr/bin/env node

// @ts-check

// Copy exes built by dune to platform bin dir

const path = require("node:path");
const fs = require("node:fs");
const child_process = require("node:child_process");
const { absolutePath: platformBinDir } = require("#cli/bin_path");
const { duneBinDir } = require("#internal/paths");

const ninjaDir = path.join(__dirname, "..", "ninja");
const rewatchDir = path.join(__dirname, "..", "rewatch");

fs.mkdirSync(platformBinDir, { recursive: true });

/**
 * @param {string} dir
 * @param {string} exe
 */
function copyExe(dir, exe) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(dir, exe + ext);
  const dest = path.join(platformBinDir, `${exe}.exe`);

  // For some reason, the copy operation fails in Windows CI if the file already exists.
  if (process.platform === "win32" && fs.existsSync(dest)) {
    fs.rmSync(dest);
  }

  fs.copyFileSync(src, dest);

  if (process.platform !== "win32") {
    child_process.execSync(`strip ${dest}`);
  }
}

if (process.argv.includes("-all") || process.argv.includes("-compiler")) {
  copyExe(duneBinDir, "rescript");
  copyExe(duneBinDir, "bsc");
  copyExe(duneBinDir, "bsb_helper");
}

if (process.argv.includes("-all") || process.argv.includes("-ninja")) {
  copyExe(ninjaDir, "ninja");
}

if (process.argv.includes("-all") || process.argv.includes("-rewatch")) {
  copyExe(rewatchDir, "rewatch");
}
