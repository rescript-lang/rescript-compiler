// @ts-check

const path = require("node:path");

/**
 * For compatibility reasons, if the architecture is x64, omit it from the bin directory name.
 * So we'll have "darwin", "linux" and "win32" for x64 arch,
 * but "darwinarm64" and "linuxarm64" for arm64 arch.
 * Also, we do not have Windows ARM binaries yet. But the x64 binaries do work on Windows 11 ARM.
 * So omit the architecture for Windows, too.
 */
const binDirName =
  process.arch === "x64" || process.platform === "win32"
    ? process.platform
    : process.platform + process.arch;

const binAbsolutePath = path.join(__dirname, "..", binDirName);

const bsc_exe = path.join(binAbsolutePath, "bsc.exe");

const ninja_exe = path.join(binAbsolutePath, "ninja.exe");

const rescript_exe = path.join(binAbsolutePath, "rescript.exe");

exports.dirName = binDirName;
exports.absolutePath = binAbsolutePath;
exports.bsc_exe = bsc_exe;
exports.ninja_exe = ninja_exe;
exports.rescript_exe = rescript_exe;
