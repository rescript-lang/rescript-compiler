//@ts-check
// usage : node ./script/shake.js bsb
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var bsc = path.join(__dirname, "..", process.platform, "bsc");
var ocamlopt = path.join(
  __dirname,
  "..",
  "native",
  "4.06.1",
  "bin",
  "ocamlopt.opt"
);

var base = process.argv[2];
if (base === undefined) {
  console.error(`please specifiy a base`);
  process.exit(2);
}
var file = `${base}.ml`;
var cwd = path.join(__dirname, "..", "lib", "4.06.1");

// TODO replace it with yours
var esy = `/Users/hongbozhang/git/genType/_esy/default/build/install/default/bin`;

/**
 *
 * @param {string} file
 */
function dsource(file) {
  let tmp = path.join(cwd, "tmp.ml");
  var output = cp.spawnSync(
    `${bsc} -bs-no-builtin-ppx -bs-syntax-only -dsource  -c ${file} 2>${tmp}`,
    {
      cwd,
      encoding: "utf8",
      shell: true
    }
  );
  // check output.status
  if (output.status === 0) {
    //  fs.writeFileSync(path.join(cwd, file), output.stderr);
    fs.copyFileSync(tmp, path.join(cwd, file));
    fs.unlinkSync(tmp);
  } else {
    console.error(`dsource failure`);
    console.error(fs.readFileSync(tmp) + "");
    process.exit(2);
  }

  // fs.copyFileSync(path.join(cwd, tmp), path.join(cwd, file));
}

/**
 *
 * @param {string} file
 * @param {string} msg
 */
function checkDiff(file, msg) {
  var output = cp.spawnSync(`git diff --quiet ${file}`, {
    shell: true,
    encoding: "utf8",
    cwd
  });
  if (output.status !== 0) {
    var output = cp.spawnSync(
      `git add ${file} && git commit -m "${msg} for ${file}"`,
      { shell: true, encoding: "utf8", cwd }
    );
    if (output.status !== 0) {
      console.error(`diff failure for ${file} -- ${msg}`);
      process.exit(2);
    } else {
      console.log(output.stdout);
    }
  } else {
    console.log(`nothing changes to ${file}`);
  }
}
/**
 *
 * @param {string} file
 */
function shake(file) {
  let tmp = path.join(cwd, "tmp.ml");
  var output = cp.spawnSync(
    `${bsc} -bs-no-builtin-ppx -bs-syntax-only -dsource  -ppx ${esy}/deadcodeppx.exe -c ${file} 2>${tmp}`,
    {
      cwd,
      encoding: "utf8",
      shell: true
    }
  );
  if (output.status !== 0) {
    console.error(`shake failure`);
    console.error(fs.readFileSync(tmp)+"");
    process.exit(2);
  } else {
    // fs.writeFileSync(path.join(cwd, file), output.stderr);
    fs.copyFileSync(tmp, path.join(cwd, file));
    fs.unlinkSync(tmp);
  }
}

/**
 *
 * @param {string} file
 */
function attachDead(file) {
  var output = cp.spawnSync(`${ocamlopt} -bin-annot  -c ${file}`, {
    cwd,
    encoding: "utf8",
    shell: true
  });

  var genType = path.join(esy, "genType.exe");

  output = cp.spawnSync(`Write=1 ${genType} -dce-cmt ${base}.cmt`, {
    cwd,
    encoding: "utf8",
    shell: true
  });
  if (output.status !== 0) {
    console.error(`dce failure`);
    console.error(output.stderr);
    process.exit(2);
  }
  // fs.writeFileSync(path.join(cwd,file),output.stderr)
}

// normalize files
dsource(file);
checkDiff(file, `dsource changes`);
attachDead(file);
shake(file);
checkDiff(file, `shake`);
