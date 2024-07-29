const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");

const { bsc_exe: bsc } = require("#cli/bin_path");

const expectedDir = path.join(__dirname, "expected");

const fixtures = fs
  .readdirSync(path.join(__dirname, "fixtures"))
  .filter(fileName => path.extname(fileName) === ".res");

// const runtime = path.join(__dirname, '..', '..', 'runtime')
const prefix = `${bsc} -w +A`;

const updateTests = process.argv[2] === "update";

/**
 * @param {string} output
 * @return {string}
 */
function postProcessErrorOutput(output) {
  return output
    .trimEnd()
    .replace(/\/[^ ]+?jscomp\/build_tests\/super_errors\//g, "/.../");
}

let doneTasksCount = 0;
let atLeastOneTaskFailed = false;

for (const fileName of fixtures) {
  const fullFilePath = path.join(__dirname, "fixtures", fileName);
  const command = `${prefix} -color always ${fullFilePath}`;
  console.log(`running ${command}`);
  child_process.exec(command, (err, stdout, stderr) => {
    doneTasksCount++;
    // careful of:
    // - warning test that actually succeeded in compiling (warning's still in stderr, so the code path is shared here)
    // - accidentally succeeding tests (not likely in this context),
    // actual, correctly erroring test case
    const actualErrorOutput = postProcessErrorOutput(stderr.toString());
    const expectedFilePath = path.join(expectedDir, `${fileName}.expected`);
    if (updateTests) {
      fs.writeFileSync(expectedFilePath, actualErrorOutput);
    } else {
      const expectedErrorOutput = postProcessErrorOutput(
        fs.readFileSync(expectedFilePath, { encoding: "utf-8" }),
      );
      if (expectedErrorOutput !== actualErrorOutput) {
        console.error(
          `The old and new error output for the test ${fullFilePath} aren't the same`,
        );
        console.error("\n=== Old:");
        console.error(expectedErrorOutput);
        console.error("\n=== New:");
        console.error(actualErrorOutput);
        atLeastOneTaskFailed = true;
      }

      if (doneTasksCount === fixtures.length && atLeastOneTaskFailed) {
        process.exit(1);
      }
    }
  });
}
