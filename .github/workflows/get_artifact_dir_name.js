const fs = require("fs");
const os = require("os");

const artifactDirName = require("../../cli/bin_path").dirName;

// Pass artifactDirName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_dir_name=${artifactDirName}${os.EOL}`
);
