const fs = require("fs");
const os = require("os");

const packageSpec = JSON.parse(fs.readFileSync("./package.json", "utf8"));
const { version } = packageSpec;

const commitHash = process.argv[2] || process.env.GITHUB_SHA;
const commitHashShort = commitHash.substring(0, 7);

fs.renameSync(
  `rescript-${version}.tgz`,
  `rescript-${version}-${commitHashShort}.tgz`
);
fs.renameSync(
  `packages/std/rescript-std-${version}.tgz`,
  `rescript-std-${version}-${commitHashShort}.tgz`
);

// Pass information to subsequent GitHub actions
fs.appendFileSync(
  process.env.GITHUB_ENV,
  `rescript_version=${version}-${commitHashShort}${os.EOL}`
);
