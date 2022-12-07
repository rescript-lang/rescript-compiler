#!/usr/bin/env node
// Copy exes built by dune to platform bin dir

const path = require("path");
const fs = require("fs");
const child_process = require("child_process");

const platformBinDir = require("./bin_path").absolutePath;
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

function copyExe(exe) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(duneBinDir, exe + ext);
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

copyExe("rescript");
copyExe("bsc");
copyExe("bsb_helper");
