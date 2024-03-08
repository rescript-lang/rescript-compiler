// @ts-check

const assert = require("assert");
const child_process = require("child_process");

const cliHelp =
  "Usage: rescript <options> <subcommand>\n" +
  "\n" +
  "`rescript` is equivalent to `rescript build`\n" +
  "\n" +
  "Options:\n" +
  "  -v, -version  display version number\n" +
  "  -h, -help     display help\n" +
  "\n" +
  "Subcommands:\n" +
  "  build\n" +
  "  clean\n" +
  "  format\n" +
  "  convert\n" +
  "  dump\n" +
  "  help\n" +
  "\n" +
  "Run `rescript <subcommand> -h` for subcommand help. Examples:\n" +
  "  rescript build -h\n" +
  "  rescript format -h\n";

const buildHelp =
  "Usage: rescript build <options> -- <ninja_options>\n" +
  "\n" +
  "`rescript build` builds the project with dependencies\n" +
  "\n" +
  "`rescript build -- -h` for Ninja options (internal usage only; unstable)\n" +
  "\n" +
  "Options:\n" +
  "  -w          Watch mode\n" +
  "  -ws         [host]:port set up host & port for WebSocket build notifications\n" +
  "  -verbose    Set the output to be verbose\n" +
  "  -with-deps  *deprecated* This is the default behavior now. This option will be removed in a future release\n";

const cleanHelp =
  "Usage: rescript clean <options>\n" +
  "\n" +
  "`rescript clean` cleans build artifacts\n" +
  "\n" +
  "Options:\n" +
  "  -verbose    Set the output to be verbose\n" +
  "  -with-deps  *deprecated* This is the default behavior now. This option will be removed in a future release\n";

const formatHelp =
  "Usage: rescript format <options> [files]\n" +
  "\n" +
  "`rescript format` formats the current directory\n" +
  "\n" +
  "Options:\n" +
  "  -stdin  [.res|.resi|.ml|.mli] Read the code from stdin and print\n" +
  "          the formatted code to stdout in ReScript syntax\n" +
  "  -all    Format the whole project \n" +
  "  -check  Check formatting for file or the whole project. Use `-all` to check the whole project\n";

const convertHelp =
  "Usage: rescript convert <options> [files]\n" +
  "\n" +
  "`rescript convert` converts the current directory\n" +
  "\n" +
  "**This command removes old OCaml files and creates new ReScript \n" +
  "files. Make sure your work is saved first!**\n" +
  "\n" +
  "Options:\n" +
  "  -all  Convert the whole project\n";

const dumpHelp =
  "Usage: rescript dump <options> [target]\n" +
  "`rescript dump` dumps the information for the target\n";

// Shows build help with --help arg
let out;

console.group("build --help");
out = child_process.execSync(`../../../rescript build --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, buildHelp);
console.groupEnd();

// console.group("build -w --help");
// out = child_process.spawnSync(`../../../rescript`, ["build", "-w", "--help"], {
//   encoding: "utf8",
//   cwd: __dirname,
// });
// assert.equal(out.stdout, buildHelp);
// assert.equal(out.stderr, "");
// assert.equal(out.status, 0);
// console.groupEnd();

console.group("-w --help");
console.log("@@ begin ");
out = child_process.execSync(`../../../rescript -w --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, cliHelp);
console.log("@@ done");
console.groupEnd();

// Shows cli help with --help arg even if there are invalid arguments after it
console.group("--help -w");
console.log("@@ begin ");
out = child_process.execSync(`../../../rescript --help -w`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, cliHelp);
console.log("@@ done");
console.groupEnd();

// Shows build help with -h arg
// console.group("build -h");
// console.log("@@ begin ");
// out = child_process.spawnSync(`../../../rescript`, ["build", "-h"], {
//   encoding: "utf8",
//   cwd: __dirname,
// });
// assert.equal(out.stdout, buildHelp);
// assert.equal(out.stderr, "");
// assert.equal(out.status, 0);
// console.log("@@ done ");
// console.groupEnd();
//
// Exits with build help with unknown arg
console.group("build -foo");
console.log("@@ begin ");
out = child_process.spawnSync(`../../../rescript`, ["build", "-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
assert.equal(out.status, 2);
console.log("@@ done ");
console.groupEnd();

// Shows cli help with --help arg
console.group("--help");
out = child_process.spawnSync(`../../../rescript`, ["--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);
console.groupEnd();

// Shows cli help with -h arg
console.group("-h");
out = child_process.spawnSync(`../../../rescript`, ["-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);
console.groupEnd();

// Shows cli help with help command
console.group("help");
out = child_process.spawnSync(`../../../rescript`, ["help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);
console.groupEnd();

// Exits with cli help with unknown command
console.group("built");
out = child_process.spawnSync(`../../../rescript`, ["built"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, `Error: Unknown command "built".\n` + cliHelp);
assert.equal(out.status, 2);
console.groupEnd();

// Exits with build help with unknown args
console.group("-foo");
out = child_process.spawnSync(`../../../rescript`, ["-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
assert.equal(out.status, 2);
console.groupEnd();

// Shows clean help with --help arg
console.group("clean --help");
out = child_process.execSync(`../../../rescript clean --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, cleanHelp);
console.groupEnd();

// Shows clean help with -h arg
console.group("clean -h");
out = child_process.execSync(`../../../rescript clean -h`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, cleanHelp);
console.groupEnd();

// Exits with clean help with unknown arg
console.group("clean -foo");
out = child_process.spawnSync(`../../../rescript`, ["clean", "-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + cleanHelp);
assert.equal(out.status, 2);
console.groupEnd();

// Shows format help with --help arg
console.group("format --help");
out = child_process.execSync(`../../../rescript format --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, formatHelp);
console.groupEnd();

// Shows format help with -h arg
console.group("format -h");
out = child_process.execSync(`../../../rescript format -h`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, formatHelp);
console.groupEnd();

// Shows convert help with --help arg
console.group("convert --help");
out = child_process.execSync(`../../../rescript convert --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, convertHelp);
console.groupEnd();

// Shows convert help with -h arg
console.group("convert -h");
out = child_process.execSync(`../../../rescript convert -h`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, convertHelp);
console.groupEnd();

// Shows dump help with --help arg
console.group("dump --help");
out = child_process.execSync(`../../../rescript dump --help`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, dumpHelp);
console.groupEnd();

// Shows dump help with -h arg
console.group("dump -h");
out = child_process.execSync(`../../../rescript dump -h`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out, dumpHelp);
console.groupEnd();
