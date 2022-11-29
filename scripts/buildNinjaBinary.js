#!/usr/bin/env node

const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

const { absolutePath: platformBinDir, ninja_exe } = require("./bin_path");

const platform = process.platform;
const ninjaDir = path.join(__dirname, "..", "ninja");
const buildCommand = "python3 configure.py --bootstrap --verbose";

if (platform === "win32") {
  // On Windows, the build uses the MSVC compiler which needs to be on the path.
  child_process.execSync(buildCommand, { cwd: ninjaDir });
} else {
  if (process.platform === "darwin") {
    process.env["CXXFLAGS"] = "-flto";
  }
  child_process.execSync(buildCommand, { stdio: [0, 1, 2], cwd: ninjaDir });
  child_process.execSync(`strip ninja`, { stdio: [0, 1, 2], cwd: ninjaDir });
}

// Copy exe to platform bin dir
if (!fs.existsSync(platformBinDir)) {
  fs.mkdirSync(platformBinDir);
}

const ext = process.platform === "win32" ? ".exe" : "";
fs.copyFileSync(path.join(ninjaDir, "ninja" + ext), ninja_exe);
