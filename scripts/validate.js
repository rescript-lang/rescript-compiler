//@ts-check

var path = require("path");
var root_dir = path.join(__dirname, "..");
var lib_dir = path.join(root_dir, "lib");

var myVersion = "4.06.1";
var cp = require("child_process");

var ocamlopt = path.join(
  __dirname,
  "..",
  "native",
  myVersion,
  "bin",
  "ocamlopt.opt"
);
var ready = false;
cp.exec(
  `git clean -dfx jscomp/runtime/*.cm* jscomp/stdlib-406/*.cm* jscomp/others/*.cm* `,
  { cwd: root_dir },
  () => {
    console.log(`clean up ready`);
    ready = true;
  }
);

cp.execSync(
  `${ocamlopt} -linscan -I 4.06.1 -g -w -a ../jscomp/stubs/ext_basic_hash_stubs.c 4.06.1/whole_compiler.mli 4.06.1/whole_compiler.ml -o ../darwin/bsc `,
  { cwd: lib_dir, stdio: [0, 1, 2] }
);
function build() {
  if (ready) {
    cp.fork(path.join(__dirname, "ninja.js"), ["build"]);
  } else {
    setImmediate(() => {
      build();
    });
  }
}
build();

