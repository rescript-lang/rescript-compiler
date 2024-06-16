const fs = require("fs");
const os = require("os");

const { dirName: artifactDirName } = require("rescript/bin_path");

// Pass artifactDirName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_dir_name=${artifactDirName}${os.EOL}`
);
