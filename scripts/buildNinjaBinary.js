#!/usr/bin/env node

const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

const platform = process.platform;
const ninjaDir = path.join(__dirname, "..", "ninja");
const buildCommand = "python configure.py --bootstrap --verbose";

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

const src = path.join(ninjaDir, `ninja${platform === "win32" ? ".exe" : ""}`);

const dst =
  platform === "darwin" && process.arch === "arm64"
    ? path.join(__dirname, "..", process.platform + process.arch, `ninja.exe`)
    : path.join(__dirname, "..", platform, `ninja.exe`);

fs.copyFileSync(src, dst);
