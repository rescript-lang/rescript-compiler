var path = require("path");

var duneBinDir = path.join(
  __dirname,
  "..",
  "_build",
  "install",
  "default",
  "bin",
);

exports.duneBinDir = duneBinDir;
