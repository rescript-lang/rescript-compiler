const fs = require("fs");

const packageSpec = JSON.parse(fs.readFileSync("./package.json", "utf8"));
const { name, version } = packageSpec;

const commitHash = process.argv[2] || process.env.GITHUB_SHA;
const commitHashShort = commitHash.substring(0, 7);

const packageName = `${name}-${version}.tgz`;
const artifactName = `${name}-${version}-${commitHashShort}.tgz`;

fs.renameSync(packageName, artifactName);

// Pass artifactPath and artifactName to subsequent GitHub actions
console.log(`::set-output name=artifact_path::${artifactName}`);
console.log(`::set-output name=artifact_name::${artifactName}`);
