//@ts-check

var path = require("path");

/**
 * @type{string}
 */
var binDirName =
  process.platform === "darwin" && process.arch === "arm64"
    ? process.platform + process.arch
    : process.platform;

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
