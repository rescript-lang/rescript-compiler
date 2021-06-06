//@ts-check
var arg = require("./rescript_arg.js");
var dump_usage = `Usage: rescript dump <options> [target]
\`rescript dump\` dumps the information for the target
`;
var child_process = require("child_process");
var path = require("path");
var specs = [];

/**
 * @param {string[]} argv
 * @param {string} bsb_exe
 * @param {string} bsc_exe
 */
function main(argv, bsb_exe, bsc_exe) {
  var target;
  arg.parse_exn(dump_usage, argv, specs, (xs) => {
    if (xs.length !== 1) {
      arg.bad_arg(`Expect only one target, ${xs.length} found` );
    }
    target = xs[0];
  });

  var { ext } = path.parse(target);
  if (ext !== ".cmi") {
    console.error("Only .cmi target allowed");
    process.exit(2);
  }


  var output = child_process.spawnSync(
    bsb_exe,
    ["build", "--", target],
    {
      encoding: "utf-8",
    }
  );
  if (output.status !== 0) {
    console.log(output.stdout);
    console.error(output.stderr);
    process.exit(2);
  }
  output = child_process.spawnSync(
    bsc_exe,
    [path.join("lib", "bs", target)],
    {
      encoding: "utf-8",
    }
  );
  console.log(output.stdout.trimEnd());
  if (output.status !== 0) {
    console.error(output.stderr);
    process.exit(2);
  }
}

exports.main = main;
