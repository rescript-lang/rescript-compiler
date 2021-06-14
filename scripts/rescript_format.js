//@ts-check
var arg = require("./rescript_arg.js");
var child_process = require("child_process");
var path = require("path");
var fs = require("fs").promises;
var crypto = require("crypto");
var os = require("os");

var supportedInputExtensions = [".res", ".resi", ".ml", ".mli", ".re", ".rei"];

var usage = `rescript format

Automatically format code in ReScript syntax

Commands:
  rescript format               Format all .res(i) files in the current directory
  rescript format -all          Format all .res(i) files in the whole project
  rescript format <file>...     Format the specified files
                                Files with .res(i) syntax are formatted in place
                                Files with other supported extensions are printed to stdout in ReScript syntax
  rescript format -stdin <ext>  Read code from stdin and print the formatted code to stdout in ReScript syntax
                                The syntax of the input must be specified by passing an extension (like .res)
  rescript format -h            Show this help
`

/**
 * @param {string[]} extensions
 * @returns {(file: string) => boolean}
 */
function hasExtension(extensions) {
  return (file) => extensions.some(ext => file.endsWith(ext));
}

var isSupportedInputFile = hasExtension(supportedInputExtensions);
var isReScriptFile = hasExtension([".res", ".resi"]);

class UsageError extends Error { }

/**
 * @param {string[]} argv
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
function main(argv, bsb_exe, bsc_exe) {
  format(argv, bsb_exe, bsc_exe)
    .catch(err => {
      if (err instanceof arg.ArgError) {
        console.error(err.message);
      } else if (err instanceof UsageError) {
        console.error(`Error: ${err.message}`);
        console.error();
        console.error(usage);
      } else {
        console.error(err);
      }
      process.exit(2);
    });
}

/**
 * @param {string[]} argv
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
async function format(argv, bsb_exe, bsc_exe) {
  /**
   * @type {arg.stringref}
   */
  var optStdin = { val: undefined };
  /**
   * @type {arg.boolref}
   */
  var optAll = { val: undefined };
  /**
   * @type{arg.specs}
   */
  var specs = [
    [
      "-stdin",
      { kind: "String", data: { kind: "String_set", data: optStdin } },
      `[.res|.resi|.ml|.mli|.re|.rei] Read the code from stdin and print
the formatted code to stdout in ReScript syntax`,
    ],
    //  ml|mli
    [
      "-all",
      { kind: "Unit", data: { kind: "Unit_set", data: optAll } },
      "Format the whole project",
    ],
  ];

  /**
   * @type {string[]}
   */
  var files = [];
  // Exist when -h is passed
  arg.parse_exn(usage, argv, specs, (xs) => {
    files = xs;
  })

  if (optAll.val) {
    if (optStdin.val || files.length != 0) {
      throw new UsageError("-all does not accept other arguments");
    }
    return formatProject(bsb_exe, bsc_exe);
  } else if (optStdin.val) {
    return formatStdin(bsc_exe, optStdin.val);
  } else if (files.length == 0) {
    return formatDirectory(bsc_exe, process.cwd());
  } else {
    return formatFiles(bsc_exe, files);
  }
}

/**
 * rescript format -all
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
async function formatProject(bsb_exe, bsc_exe) {
  var output = child_process.spawnSync(bsb_exe, ["info", "-list-files"], {
    encoding: "utf-8",
  });
  if (output.status !== 0) {
    console.error(output.stdout);
    console.error(output.stderr);
    process.exit(2);
  }
  return Promise.all(
    output.stdout
      .split("\n")
      .map(x => x.trim())
      .filter(isReScriptFile)
      .map(async file => formatFileInPlace(bsc_exe, file))
  );
}

/**
 * @param {string} bsc_exe
 * @param {string} file
 */
async function formatFileInPlace(bsc_exe, file) {
  return new Promise((resolve, reject) => {
    child_process.execFile(
      bsc_exe,
      ["-o", file, "-format", file],
      (error, _stdout, stderr) => {
        if (error === null) {
          resolve();
        } else {
          reject(stderr);
        }
      }
    )
  });
}

/**
 * @param {string} bsc_exe
 * @param {string} file
 */
async function getFormattedFile(bsc_exe, file) {
  return new Promise((resolve, reject) => {
    child_process.execFile(
      bsc_exe,
      ["-format", file],
      (error, stdout, stderr) => {
        if (error === null) {
          resolve(stdout);
        } else {
          reject(stderr);
        }
      }
    )
  });
}

/**
 * rescript format <file>...
 * @param {string} bsc_exe
 * @param {string[]} files
 */
async function formatFiles(bsc_exe, files) {
  var invalid;
  if (invalid = files.find(file => !isSupportedInputFile(file))) {
    throw new UsageError(`unsupported input file: ${invalid}`);
  }
  return Promise.all(
    files.map(async file => {
      // This is surprising behaviour. Either multiple files should be formatted
      // in place, or a single file should be printed to stdout.
      if (isReScriptFile(file)) {
        return formatFileInPlace(bsc_exe, file);
      } else {
        return getFormattedFile(bsc_exe, file)
          .then(formatted => process.stdout.write(formatted));
      }
    })
  )
}

async function readStdin() {
  var stream = process.stdin;
  const chunks = [];
  for await (const chunk of stream) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

/**
 * rescript format -stdin <ext>
 * @param {string} bsc_exe
 * @param {string} extension
 */
async function formatStdin(bsc_exe, extension) {
  if (!supportedInputExtensions.includes(extension)) {
    throw new UsageError(`unsupported extension: ${extension}`);
  }

  var content = await readStdin();

  // Generate a random filename and try to create it. Fails if the file already exists.
  // TODO: Retry a few times if we pick a filename that already exists?
  var filename = path.join(
    os.tmpdir(),
    "rescript_" + crypto.randomBytes(8).toString("hex") + extension
  );
  var fd = await fs.open(filename, 'wx', 0o600);
  process.addListener('exit', async () => fs.unlink(filename));

  await fd.writeFile(content, "utf-8");
  await fd.close();

  return getFormattedFile(bsc_exe, filename)
    .then(formatted => process.stdout.write(formatted));
}

/**
 * rescript format
 * @param {string} bsc_exe
 * @param {string} directory
 */
async function formatDirectory(bsc_exe, directory) {
  var files = await fs.readdir(directory);
  return Promise.all(
    files
      .filter(isReScriptFile)
      .map(async file => formatFileInPlace(bsc_exe, file))
  );
}

exports.main = main;
