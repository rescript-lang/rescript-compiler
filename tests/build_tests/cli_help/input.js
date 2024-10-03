// @ts-check

const assert = require("assert");
const { exec } = require("../utils.js");

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
  "  -stdin  [.res|.resi] Read the code from stdin and print\n" +
  "          the formatted code to stdout in ReScript syntax\n" +
  "  -all    Format the whole project \n" +
  "  -check  Check formatting for file or the whole project. Use `-all` to check the whole project\n";

const dumpHelp =
  "Usage: rescript dump <options> [target]\n" +
  "`rescript dump` dumps the information for the target\n";

async function test() {
  {
    // Shows build help with --help arg
    const out = await exec(`../../../rescript`, ["build", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, buildHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    const out = await exec(`../../../rescript`, ["build", "-w", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, buildHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    const out = await exec(`../../../rescript`, ["-w", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cliHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows cli help with --help arg even if there are invalid arguments after it
    const out = await exec(`../../../rescript`, ["--help", "-w"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cliHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows build help with -h arg
    const out = await exec(`../../../rescript`, ["build", "-h"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, buildHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Exits with build help with unknown arg
    const out = await exec(`../../../rescript`, ["build", "-foo"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, "");
    assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
    assert.equal(out.status, 2);
  }

  {
    // Shows cli help with --help arg
    const out = await exec(`../../../rescript`, ["--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cliHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows cli help with -h arg
    const out = await exec(`../../../rescript`, ["-h"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cliHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows cli help with -h arg
    const out = await exec(`../../../rescript`, ["help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cliHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Exits with cli help with unknown command
    const out = await exec(`../../../rescript`, ["built"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, "");
    assert.equal(out.stderr, `Error: Unknown command "built".\n` + cliHelp);
    assert.equal(out.status, 2);
  }

  {
    // Exits with build help with unknown args
    const out = await exec(`../../../rescript`, ["-foo"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, "");
    assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + buildHelp);
    assert.equal(out.status, 2);
  }

  {
    // Shows clean help with --help arg
    const out = await exec(`../../../rescript`, ["clean", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cleanHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows clean help with -h arg
    const out = await exec(`../../../rescript`, ["clean", "-h"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, cleanHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Exits with clean help with unknown arg
    const out = await exec(`../../../rescript`, ["clean", "-foo"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, "");
    assert.equal(out.stderr, 'Error: Unknown option "-foo".\n' + cleanHelp);
    assert.equal(out.status, 2);
  }

  {
    // Shows format help with --help arg
    const out = await exec(`../../../rescript`, ["format", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, formatHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows format help with -h arg
    const out = await exec(`../../../rescript`, ["format", "-h"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, formatHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows dump help with --help arg
    const out = await exec(`../../../rescript`, ["dump", "--help"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, dumpHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }

  {
    // Shows dump help with -h arg
    const out = await exec(`../../../rescript`, ["dump", "-h"], {
      cwd: __dirname,
    });
    assert.equal(out.stdout, dumpHelp);
    assert.equal(out.stderr, "");
    assert.equal(out.status, 0);
  }
}

void test();
