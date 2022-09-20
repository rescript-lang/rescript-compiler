#!/usr/bin/env node

const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

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

const { absolutePath, ninja_exe } = require("./bin_path");
const src = path.join(ninjaDir, `ninja${platform === "win32" ? ".exe" : ""}`);

if (!fs.existsSync(absolutePath)) {
  fs.mkdirSync(absolutePath);
}

fs.copyFileSync(src, ninja_exe);
