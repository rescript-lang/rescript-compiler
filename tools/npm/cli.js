#!/usr/bin/env node
//@ts-check
"use strict";

const child_process = require("child_process");
const { getBinaryPath } = require("./getBinaryPath");

const args = process.argv.slice(2);

const spawn = child_process.spawnSync(getBinaryPath(), args, {
  stdio: "inherit",
});

if (spawn.status != null) {
  process.exit(spawn.status);
}
