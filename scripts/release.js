#!/usr/bin/env node

// @ts-check

// used as a unit of releasing

const path = require("node:path");
const cp = require("node:child_process");
const { projectDir, compilerRootDir } = require("#internal/paths");

cp.execSync("git clean -dfx stubs ext common syntax depends core bsb main .", {
  cwd: compilerRootDir,
  encoding: "utf8",
  stdio: [0, 1, 2],
});
cp.execSync("ninja -t clean -g && ninja", {
  cwd: compilerRootDir,
  encoding: "utf8",
  stdio: [0, 1, 2],
});
cp.execSync("ninja", { cwd: path.join(projectDir, "lib"), stdio: [0, 1, 2] });
cp.execSync("ninja -f release.ninja -t clean && ninja -f release.ninja", {
  cwd: compilerRootDir,
  stdio: [0, 1, 2],
});
