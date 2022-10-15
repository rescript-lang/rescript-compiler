const fs = require("fs");
const os = require("os");

const artifactPath =
  process.platform === "darwin" && process.arch === "arm64"
    ? process.platform + process.arch
    : process.platform;

const artifactName = "binaries-" + artifactPath;

// Pass artifactPath and artifactName to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `artifact_path=${artifactPath}${os.EOL}artifact_name=${artifactName}${os.EOL}`
);
