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
  const dest = path.join(platformBinDir, exe + ".exe");

  const ext = process.platform === "win32" ? ".exe" : "";
  fs.copyFileSync(path.join(duneBinDir, exe + ext), dest);

  if (process.platform !== "win32") {
    child_process.execSync(`strip ${dest}`);
  }
}

copyExe("rescript");
copyExe("bsc");
copyExe("bsb_helper");
