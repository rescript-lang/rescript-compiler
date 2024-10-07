var path = require("path");

var duneBinDir = path.join(
  __dirname,
  "..",
  "compiler",
  "_build",
  "install",
  "default",
  "bin",
);

exports.duneBinDir = duneBinDir;
