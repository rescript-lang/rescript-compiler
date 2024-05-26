//@ts-check

var path = require("path");

/**
 * @type{string}
 *
 * For compatibility reasons, if the architecture is x64, omit it from the bin directory name.
 * So we'll have "darwin", "linux" and "win32" for x64 arch,
 * but "darwinarm64" and "linuxarm64" for arm64 arch.
 * Also, we do not have Windows ARM binaries yet. But the x64 binaries do work on Windows 11 ARM.
 * So omit the architecture for Windows, too.
 */
var binDirName =
  process.arch === "x64" || process.platform === "win32"
    ? process.platform
    : process.platform + process.arch;

/**
 *
 * @type{string}
 */
var binAbsolutePath = path.join(__dirname, "..", binDirName);

/**
 * @type{string}
 */
var bsc_exe = path.join(binAbsolutePath, "bsc.exe");

/**
 * @type{string}
 */
var ninja_exe = path.join(binAbsolutePath, "ninja.exe");

/**
 * @type{string}
 */
var rescript_exe = path.join(binAbsolutePath, "rescript.exe");

exports.dirName = binDirName;
exports.absolutePath = binAbsolutePath;
exports.bsc_exe = bsc_exe;
exports.ninja_exe = ninja_exe;
exports.rescript_exe = rescript_exe;
