//@ts-check
var child_process = require("child_process");
var path = require("path");
var fs = require("fs/promises");
var crypto = require("crypto");
var os = require("os");

var supportedInputExtensions = [".res", ".resi", ".ml", ".mli", ".re", ".rei"];

function usage() {
  console.error("rescript format");
  console.error("");
  console.error("Automatically format code in ReScript syntax");
  console.error("");
  console.error("Commands");
  console.error("  rescript format               Format all .res(i) files in the current directory");
  console.error("  rescript format -all          Format all .res(i) files in the whole project");
  console.error("  rescript format <file>...     Format the specified files");
  console.error("                                Files with .res(i) syntax are formatted in place");
  console.error("                                Files with other supported extensions are printed to stdout in ReScript syntax");
  console.error("  rescript format -stdin <ext>  Read code from stdin and print the formatted code to stdout in ReScript syntax");
  console.error("                                The syntax of the input must be specified by passing an extension (like .res)");
  console.error("  rescript format -h            Show this help");
  console.error("");
  console.error("Supported inputs");
  console.error("  Formatting in place:  .res, .resi");
  console.error("  Printing to stdout:   " + supportedInputExtensions.join(", "));
}

/**
 * @param {string[]} extensions
 * @returns {(file: string) => boolean}
 */
function hasExtension(extensions) {
  return (file) => extensions.some(ext => file.endsWith(ext));
}

var isSupportedInputFile = hasExtension(supportedInputExtensions);
var isReScriptFile = hasExtension([".res", ".resi"]);

class ArgumentError extends Error { }

/**
 * @param {string[]} argv
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
function main(argv, bsb_exe, bsc_exe) {
  format(argv, bsb_exe, bsc_exe)
    .catch(err => {
      if (err instanceof ArgumentError) {
        console.error(`Error: ${err.message}`);
        console.error();
        usage();
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
  var files = [];
  var arg;
  loop:
  while ((arg = argv.shift()) !== undefined) {
    switch (arg) {
      case "--":
        files.push(...argv);
        break loop;
      case "-all":
        if (argv.length != 0 || files.length != 0) {
          throw new ArgumentError("-all does not accept other arguments");
        }
        return formatProject(bsb_exe, bsc_exe);
      case "-stdin":
        if (argv.length != 1 || files.length != 0) {
          throw new ArgumentError("-stdin requires an extension argument");
        }
        return formatStdin(bsc_exe, argv[0]);
      case "-h":
      case "-help":
      case "--help":
        return usage();
      default:
        if (arg.startsWith("-")) {
          throw new ArgumentError(`unrecognized option: ${arg}`);
        }
        files.push(arg);
    }
  }
  if (files.length > 0) {
    return formatFiles(bsc_exe, files);
  } else {
    return formatDirectory(bsc_exe, process.cwd());
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
    throw new ArgumentError(`unsupported input file: ${invalid}`);
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
    throw new ArgumentError(`unsupported extension: ${extension}`);
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
