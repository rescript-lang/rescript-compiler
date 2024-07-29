//@ts-check
const arg = require("./rescript_arg.js");
const format_usage = `Usage: rescript format <options> [files]

\`rescript format\` formats the current directory
`;
const child_process = require("node:child_process");
const util = require("node:util");
const asyncExecFile = util.promisify(child_process.execFile);
const path = require("node:path");
const fs = require("node:fs");
const asyncFs = fs.promises;

/**
 * @type {arg.stringref}
 */
const stdin = { val: "" };

/**
 * @type {arg.boolref}
 */
const format = { val: false };

/**
 * @type {arg.boolref}
 */
const check = { val: false };

/**
 * @type{arg.specs}
 */
const specs = [
  [
    "-stdin",
    { kind: "String", data: { kind: "String_set", data: stdin } },
    `[.res|.resi] Read the code from stdin and print
the formatted code to stdout in ReScript syntax`,
  ],
  [
    "-all",
    { kind: "Unit", data: { kind: "Unit_set", data: format } },
    "Format the whole project ",
  ],
  [
    "-check",
    { kind: "Unit", data: { kind: "Unit_set", data: check } },
    "Check formatting for file or the whole project. Use `-all` to check the whole project",
  ],
];
const formattedStdExtensions = [".res", ".resi"];
const formattedFileExtensions = [".res", ".resi"];

/**
 *
 * @param {string[]} extensions
 */
function hasExtension(extensions) {
  /**
   * @param {string} x
   */
  const pred = x => extensions.some(ext => x.endsWith(ext));
  return pred;
}
async function readStdin() {
  const stream = process.stdin;
  const chunks = [];
  for await (const chunk of stream) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

/**
 * @param {string[]} files
 * @param {string} bsc_exe
 * @param {(x: string) => boolean} isSupportedFile
 * @param {boolean} checkFormatting
 */
async function formatFiles(files, bsc_exe, isSupportedFile, checkFormatting) {
  let incorrectlyFormattedFiles = 0;
  try {
    const _promises = await Promise.all(
      files.map(async file => {
        if (isSupportedFile(file)) {
          const flags = checkFormatting
            ? ["-format", file]
            : ["-o", file, "-format", file];
          const { stdout } = await asyncExecFile(bsc_exe, flags);
          if (check.val) {
            const original = await asyncFs.readFile(file, "utf-8");
            if (original !== stdout) {
              console.error("[format check]", file);
              incorrectlyFormattedFiles++;
            }
          }
        }
        return null;
      }),
    );
  } catch (err) {
    console.error(err);
    process.exit(2);
  }
  if (incorrectlyFormattedFiles > 0) {
    if (incorrectlyFormattedFiles === 1) {
      console.error("The file listed above needs formatting");
    } else {
      console.error(
        `The ${incorrectlyFormattedFiles} files listed above need formatting`,
      );
    }
    process.exit(3);
  }
}

/**
 * @param {string[]} argv
 * @param {string} rescript_exe
 * @param {string} bsc_exe
 */
async function main(argv, rescript_exe, bsc_exe) {
  const isSupportedFile = hasExtension(formattedFileExtensions);
  const isSupportedStd = hasExtension(formattedStdExtensions);

  try {
    /**
     * @type {string[]}
     */
    let files = [];
    arg.parse_exn(format_usage, argv, specs, xs => {
      files = xs;
    });

    const format_project = format.val;
    const use_stdin = stdin.val;

    // Only -check arg
    // Require: -all or path to a file
    if (check.val && !format_project && files.length === 0) {
      console.error(
        "format check require path to a file or use `-all` to check the whole project",
      );
      process.exit(2);
    }

    if (format_project) {
      if (use_stdin || files.length !== 0) {
        console.error("format -all can not be in use with other flags");
        process.exit(2);
      }
      // -all
      // TODO: check the rest arguments
      const output = child_process.spawnSync(
        rescript_exe,
        ["info", "-list-files"],
        {
          encoding: "utf-8",
        },
      );
      if (output.status !== 0) {
        console.error(output.stdout);
        console.error(output.stderr);
        process.exit(2);
      }
      files = output.stdout.split("\n").map(x => x.trim());
      await formatFiles(files, bsc_exe, isSupportedFile, check.val);
    } else if (use_stdin) {
      if (check.val) {
        console.error("format -stdin cannot be used with -check flag");
        process.exit(2);
      }
      if (isSupportedStd(use_stdin)) {
        const crypto = require("node:crypto");
        const os = require("node:os");
        const filename = path.join(
          os.tmpdir(),
          `rescript_${crypto.randomBytes(8).toString("hex")}${path.parse(use_stdin).base}`,
        );
        (async () => {
          const content = await readStdin();
          const fd = fs.openSync(filename, "wx", 0o600); // Avoid overwriting existing file
          fs.writeFileSync(fd, content, "utf8");
          fs.closeSync(fd);
          process.addListener("exit", () => fs.unlinkSync(filename));
          child_process.execFile(
            bsc_exe,
            ["-format", filename],
            (error, stdout, stderr) => {
              if (error === null) {
                process.stdout.write(stdout);
              } else {
                console.error(stderr);
                process.exit(2);
              }
            },
          );
        })();
      } else {
        console.error(`Unsupported extension ${use_stdin}`);
        console.error(`Supported extensions: ${formattedStdExtensions} `);
        process.exit(2);
      }
    } else {
      if (files.length === 0) {
        // none of argumets set
        // format the current directory
        files = fs.readdirSync(process.cwd()).filter(isSupportedFile);
      }

      for (const file of files) {
        if (!isSupportedStd(file)) {
          console.error(`Don't know what do with ${file}`);
          console.error(`Supported extensions: ${formattedFileExtensions}`);
          process.exit(2);
        }
      }
      await formatFiles(files, bsc_exe, isSupportedFile, check.val);
    }
  } catch (e) {
    if (e instanceof arg.ArgError) {
      console.error(e.message);
      process.exit(2);
    } else {
      throw e;
    }
  }
}
exports.main = main;
