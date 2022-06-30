const fs = require("fs");

const packageSpec = JSON.parse(fs.readFileSync("./package.json", "utf8"));
const { name, version } = packageSpec;

const commitHash = process.argv[2] || process.env.GITHUB_SHA;
const commitHashShort = commitHash.substring(0, 7);

const rescriptPackagePath = `${name}-${version}.tgz`;
const rescriptArtifactName = `${name}-${version}-${commitHashShort}.tgz`;

const stdlibPackagePath = `packages/std/rescript-std-${version}.tgz`;
const stdlibArtifactName = `rescript-std-${version}-${commitHashShort}.tgz`;

fs.renameSync(rescriptPackagePath, rescriptArtifactName);
fs.renameSync(stdlibPackagePath, stdlibArtifactName);

// Pass information to subsequent GitHub actions
console.log(`::set-output name=rescript_package::${rescriptArtifactName}`);
console.log(`::set-output name=stdlib_package::${stdlibArtifactName}`);
