// @ts-check

const assert = require("assert");
const child_process = require("child_process");

// Shows compile time for `rescript build` command
let out = child_process.spawnSync(`../../../rescript`, ["build"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.match(
  out.stdout,
  new RegExp(`>>>> Start compiling
Dependency Finished
>>>> Finish compiling \\d+ mseconds`)
);

// Shows compile time for `rescript` command
out = child_process.spawnSync(`../../../rescript`, {
  encoding: "utf8",
  cwd: __dirname,
});
assert.match(
  out.stdout,
  new RegExp(`>>>> Start compiling
Dependency Finished
>>>> Finish compiling \\d+ mseconds`)
);

// Doesn't show compile time for `rescript build -verbose` command
// Because we can't be sure that -verbose is a valid argument
// And bsb won't fail with a usage message.
// It works this way not only for -verbose, but any other arg, including -h/--help/-help
out = child_process.spawnSync(`../../../rescript`, ["build", "-verbose"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.match(
  out.stdout,
  new RegExp(
    `Package stack: test  \nDependency Finished\nninja.exe -C lib/bs \n`
  )
);
