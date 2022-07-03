//@ts-check
var arg = require("./rescript_arg.js");
var format_usage = `Usage: rescript format <options> [files]

\`rescript format\` formats the current directory
`;
var child_process = require("child_process");
var path = require("path");
var fs = require("fs");
/**
 * @type {arg.stringref}
 */
var stdin = { val: undefined };

/**
 * @type {arg.boolref}
 */
var format = { val: undefined };

/**
 * @type{arg.specs}
 */
var specs = [
  [
    "-stdin",
    { kind: "String", data: { kind: "String_set", data: stdin } },
    `[.res|.resi|.ml|.mli] Read the code from stdin and print
the formatted code to stdout in ReScript syntax`,
  ],
  //  ml|mli
  [
    "-all",
    { kind: "Unit", data: { kind: "Unit_set", data: format } },
    "Format the whole project ",
  ],
];
var formattedStdExtensions = [".res", ".resi", ".ml", ".mli"];
var formattedFileExtensions = [".res", ".resi"];

/**
 *
 * @param {string[]} extensions
 */
function hasExtension(extensions) {
  /**
   * @param {string} x
   */
  var pred = x => extensions.some(ext => x.endsWith(ext));
  return pred;
}
async function readStdin() {
  var stream = process.stdin;
  const chunks = [];
  for await (const chunk of stream) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

/**
 * @param {string[]} argv
 * @param {string} rescript_exe
 * @param {string} bsc_exe
 */
function main(argv, rescript_exe, bsc_exe) {
  var isSupportedFile = hasExtension(formattedFileExtensions);
  var isSupportedStd = hasExtension(formattedStdExtensions);

  try {
    /**
     * @type {string[]}
     */
    var files = [];
    arg.parse_exn(format_usage, argv, specs, xs => {
      files = xs;
    });

    var format_project = format.val;
    var use_stdin = stdin.val;
    if (format_project) {
      if (use_stdin || files.length !== 0) {
        console.error("format -all can not be in use with other flags");
        process.exit(2);
      }
      // -all
      // TODO: check the rest arguments
      var output = child_process.spawnSync(
        rescript_exe,
        ["info", "-list-files"],
        {
          encoding: "utf-8",
        }
      );
      if (output.status !== 0) {
        console.error(output.stdout);
        console.error(output.stderr);
        process.exit(2);
      }
      files = output.stdout.split("\n").map(x => x.trim());
      var hasError = false;
      for (let arg of files) {
        if (isSupportedFile(arg)) {
          // console.log(`processing ${arg}`);
          child_process.execFile(
            bsc_exe,
            ["-o", arg, "-format", arg],
            (error, _stdout, stderr) => {
              if (error !== null) {
                console.error(stderr);
                hasError = true;
              }
            }
          );
        }
      }
      if (hasError) {
        process.exit(2);
      }
    } else if (use_stdin) {
      if (isSupportedStd(use_stdin)) {
        var crypto = require("crypto");
        var os = require("os");
        var filename = path.join(
          os.tmpdir(),
          "rescript_" +
            crypto.randomBytes(8).toString("hex") +
            path.parse(use_stdin).base
        );
        (async function () {
          var content = await readStdin();
          var fd = fs.openSync(filename, "wx", 0o600); // Avoid overwriting existing file
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
            }
          );
        })();
      } else {
        console.error(`Unsupported exetnsion ${use_stdin}`);
        console.error(`Supported extensions: ${formattedStdExtensions} `);
        process.exit(2);
      }
    } else {
      if (files.length === 0) {
        // none of argumets set
        // format the current directory
        files = fs.readdirSync(process.cwd()).filter(isSupportedFile);
      }

      for (let i = 0; i < files.length; ++i) {
        let file = files[i];
        if (!isSupportedStd(file)) {
          console.error(`Don't know what do with ${file}`);
          console.error(`Supported extensions: ${formattedFileExtensions}`);
          process.exit(2);
        }
      }
      var hasError = false;
      files.forEach(file => {
        var write = isSupportedFile(file);
        var flags = write ? ["-o", file, "-format", file] : ["-format", file];
        child_process.execFile(bsc_exe, flags, (error, stdout, stderr) => {
          if (error === null) {
            if (!write) {
              process.stdout.write(stdout);
            }
          } else {
            console.error(stderr);
            hasError = true;
          }
        });
      });
      if (hasError) {
        process.exit(2);
      }
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
