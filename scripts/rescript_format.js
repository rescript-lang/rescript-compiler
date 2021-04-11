//@ts-check
var arg = require("./rescript_arg.js");
var format_usage = `Usage: rescript format <options> [files]
rescript format -- it format the current directory
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
    `[.res|.resi|.ml|.mli|.re|.rei] Read the format code from stdin and print the formatted code
in the stdout (in rescript syntax)`,
  ],
  //  ml|mli
  [
    "-all",
    { kind: "Unit", data: { kind: "Unit_set", data: format } },
    "Formatting the whole project ",
  ],
];

async function readStdin() {
  var stream = process.stdin;
  const chunks = [];
  for await (const chunk of stream) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

/**
 * @param {string[]} argv
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
function main(argv, bsb_exe, bsc_exe) {
  try {
    /**
     * @type {string[]}
     */
    var files = [];
    arg.parse_exn(format_usage, argv, specs, (xs) => {
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
      var output = child_process.spawnSync(bsb_exe, ["info", "-list-files"], {
        encoding: "utf-8",
      });
      if (output.status !== 0) {
        console.error(output.stdout);
        console.error(output.stderr);
        process.exit(2);
      }
      files = output.stdout.split("\n").map((x) => x.trim());
      for (let arg of files) {
        if (arg.endsWith(".res") || arg.endsWith(".resi")) {
          // console.log(`processing ${arg}`);
          child_process.execFile(
            bsc_exe,
            ["-o", arg, "-format", arg],
            (error, stdout, stderr) => {
              if (error === null) {
                // todo
              } else {
                // todo error handling
                console.log(stderr);
              }
            }
          );
        }
      }
    } else if (use_stdin) {
      if (
        use_stdin.endsWith(".res") ||
        use_stdin.endsWith(".resi") ||
        use_stdin.endsWith(".ml") ||
        use_stdin.endsWith(".mli") ||
        use_stdin.endsWith(".re") ||
        use_stdin.endsWith(".rei")
      ) {
        var crypto = require("crypto");
        var os = require("os");
        var filename = path.join(
          os.tmpdir(),
          "rescript_" + crypto.randomBytes(8).toString("hex") + use_stdin
        );
        (async function () {
          var content = await readStdin();
          fs.writeFileSync(filename, content, "utf8");
          child_process.execFile(
            bsc_exe,
            ["-format", filename],
            (error, stdout, stderr) => {
              if (error === null) {
                console.log(stdout);
              } else {
                console.log(stderr);
                process.exit(2);
              }
            }
          );
        })();
      }
    } else {
      if (files.length === 0) {
        // none of argumets set
        // format the current directory
        files = fs
          .readdirSync(process.cwd())
          .filter((x) => x.endsWith(".res") || x.endsWith(".resi"));
      }
      for (let i = 0; i < files.length; ++i) {
        let file = files[i];
        if (!(file.endsWith(".res") || file.endsWith(".resi"))) {
          console.error(`don't know what do with ${file}`);
          process.exit(2);
        }
      }
      files.forEach((file) => {
        child_process.execFile(
          bsc_exe,
          ["-o", file, "-format", file],
          (error, stdout, stderr) => {
            if (error === null) {
            } else {
              console.log(stderr);
            }
          }
        );
      });
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
