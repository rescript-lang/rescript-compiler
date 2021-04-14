//@ts-check
var arg = require("./rescript_arg.js");
var format_usage = `Usage: rescript convert <options> [files]
rescript convert -- it converts the current directory
Note this subcommand will remove old reason/ml files and 
create new ReScript files, make sure your work is saved first.
`;

var child_process = require("child_process");
var path = require("path");
var fs = require("fs");

/**
 * @type {arg.boolref}
 */
var formatProject = { val: undefined };

/**
 * @type{arg.specs}
 */
var specs = [
  [
    "-all",
    { kind: "Unit", data: { kind: "Unit_set", data: formatProject } },
    "Formatting the whole project ",
  ],
];

/**
 *
 * @param {string} file
 */
function shouldConvert(file) {
  return [".ml", ".mli", ".re", ".rei"].some((x) => file.endsWith(x));
}

/**
 *
 * @param {string} file
 * @param {string} bsc_exe
 * assume the file is convertible
 */
function handleOneFile(file, bsc_exe) {
  // console.log(`processing ${arg}`);
  var nextExt = file.endsWith("i") ? ".resi" : ".res";
  child_process.execFile(
    bsc_exe,
    ["-o", file.substr(0, file.lastIndexOf(".")) + nextExt, "-format", file],
    (error, stdout, stderr) => {
      if (error === null) {
        // todo
        fs.unlink(file, () => {
          //ignore
        });
      } else {
        // todo error handling
        console.error(`Error when converting ${file}`);
        console.log(stderr);
      }
    }
  );
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

    var format_project = formatProject.val;

    if (format_project) {
      if (files.length !== 0) {
        console.error("convert -all can not be in use with other flags");
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
      for (let file of files) {
        if (shouldConvert(file)) {
          handleOneFile(file, bsc_exe);
        }
      }
    } else {
      for (let i = 0; i < files.length; ++i) {
        let file = files[i];
        if (!shouldConvert(file)) {
          console.error(`don't know what do with ${file}`);
          process.exit(2);
        }
      }
      files.forEach((file) => {
        handleOneFile(file, bsc_exe);
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
