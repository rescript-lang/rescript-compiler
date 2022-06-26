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
var bin_path = path.join(__dirname, "..", binDirName);

/**
 * @type{string}
 */
var bsc_exe = path.join(bin_path, "bsc.exe");

/**
 * @type{string}
 */
var ninja_exe = path.join(bin_path, "ninja.exe");

/**
 * @type{string}
 */
var rescript_exe = path.join(bin_path, "rescript.exe");

exports.dirName = binDirName;
exports.absolutePath = bin_path;
exports.bsc_exe = bsc_exe;
exports.ninja_exe = ninja_exe;
exports.rescript_exe = rescript_exe;
