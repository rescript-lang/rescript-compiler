const assert = require("assert");
const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

const expectedContent = `let a = (. b) => b\n`;
const filePath = path.join(__dirname, "src", "a.res");

fs.writeFileSync(filePath, expectedContent, "utf-8");

child_process.execSync(`../../../rescript format -all`, { cwd: __dirname });

const content = fs.readFileSync(filePath, "utf-8");

assert.equal(content, expectedContent);
