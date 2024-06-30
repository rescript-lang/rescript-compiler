const fs = require("fs");
const os = require("os");

const { dirName: artifactDirName } = require("../../cli/bin_path.js");

// Pass artifactDirName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_OUTPUT,
  `dirname=${artifactDirName}${os.EOL}`,
);
