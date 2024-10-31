const path = require("path");

function getBinaryPath() {
  const platformArch =
    process.arch === "x64" ? process.platform : process.platform + process.arch;

  const binPath = path.join(
    __dirname,
    "..",
    "binaries",
    platformArch,
    "rescript-tools.exe",
  );
  return binPath;
}

module.exports = {
  getBinaryPath,
};
