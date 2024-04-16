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
  "  -w           Watch mode\n" +
  "  -ws          [host]:port set up host & port for WebSocket build notifications\n" +
  "  -verbose     Set the output to be verbose\n" +
  "  -with-deps   *deprecated* This is the default behavior now. This option will be removed in a future release\n" +
  '  -warn-error  Warning numbers and whether to turn them into errors, e.g., "+8+32-102"\n';

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
let out = child_process.spawnSync(`../../../rescript`, ["build", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, buildHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// FIXME: Help works incorrectly in watch mode
out = child_process.spawnSync(`../../../rescript`, ["build", "-w", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
// FIXME: Shouldn't have "Start compiling" for help
assert.equal(out.stdout, ">>>> Start compiling\n" + buildHelp);
// FIXME: Don't run the watcher when showing help
assert.match(
  out.stderr,
  new RegExp(
    "Uncaught Exception Error: ENOENT: no such file or directory, watch 'bsconfig.json'\n"
  )
);
// FIXME: Should be 0
assert.equal(out.status, 1);

// FIXME: Has the same problem with `rescript -w`
out = child_process.spawnSync(`../../../rescript`, ["-w", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, ">>>> Start compiling\n" + buildHelp);
assert.match(
  out.stderr,
  new RegExp(
    "Uncaught Exception Error: ENOENT: no such file or directory, watch 'bsconfig.json'\n"
  )
);

// Shows cli help with --help arg even if there are invalid arguments after it
out = child_process.spawnSync(`../../../rescript`, ["--help", "-w"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows build help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["build", "-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, buildHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Exits with build help with unknown arg
out = child_process.spawnSync(`../../../rescript`, ["build", "-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
assert.equal(out.status, 2);

// Shows cli help with --help arg
out = child_process.spawnSync(`../../../rescript`, ["--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows cli help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows cli help with help command
out = child_process.spawnSync(`../../../rescript`, ["help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cliHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Exits with cli help with unknown command
out = child_process.spawnSync(`../../../rescript`, ["built"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, `Error: Unknown command "built".\n` + cliHelp);
assert.equal(out.status, 2);

// Exits with build help with unknown args
out = child_process.spawnSync(`../../../rescript`, ["-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
assert.equal(out.status, 2);

// Shows clean help with --help arg
out = child_process.spawnSync(`../../../rescript`, ["clean", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cleanHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows clean help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["clean", "-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, cleanHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Exits with clean help with unknown arg
out = child_process.spawnSync(`../../../rescript`, ["clean", "-foo"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, "");
assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + cleanHelp);
assert.equal(out.status, 2);

// Shows format help with --help arg
out = child_process.spawnSync(`../../../rescript`, ["format", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, formatHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows format help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["format", "-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, formatHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows convert help with --help arg
out = child_process.spawnSync(`../../../rescript`, ["convert", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, convertHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows convert help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["convert", "-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, convertHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows dump help with --help arg
out = child_process.spawnSync(`../../../rescript`, ["dump", "--help"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, dumpHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);

// Shows dump help with -h arg
out = child_process.spawnSync(`../../../rescript`, ["dump", "-h"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.equal(out.stdout, dumpHelp);
assert.equal(out.stderr, "");
assert.equal(out.status, 0);
