#!/usr/bin/env node
//@ts-check

var os = require("os");
var fs = require("fs");
var path = require("path");
var cp = require("child_process");
var semver = require("semver");

var jscompDir = path.join(__dirname, "..", "jscomp");
var runtimeDir = path.join(jscompDir, "runtime");
var othersDir = path.join(jscompDir, "others");
var testDir = path.join(jscompDir, "test");

var jsDir = path.join(__dirname, "..", "lib", "js");

var runtimeFiles = fs.readdirSync(runtimeDir, "ascii");
var runtimeMlFiles = runtimeFiles.filter(
  x => !x.startsWith("pervasives") && x.endsWith(".res") && x !== "js.res",
);
var runtimeMliFiles = runtimeFiles.filter(
  x => !x.startsWith("pervasives") && x.endsWith(".resi") && x !== "js.resi",
);
var runtimeSourceFiles = runtimeMlFiles.concat(runtimeMliFiles);
var runtimeJsFiles = [...new Set(runtimeSourceFiles.map(baseName))];

var commonBsFlags = `-no-keep-locs -no-alias-deps -bs-no-version-header -bs-no-check-div-by-zero -nostdlib `;
var js_package = pseudoTarget("js_pkg");
var runtimeTarget = pseudoTarget("runtime");
var othersTarget = pseudoTarget("others");
var {
  absolutePath: my_target,
  bsc_exe,
  ninja_exe: vendorNinjaPath,
} = require("#cli/bin_path");

// Let's enforce a Node version >= 16 to make sure M1 users don't trip up on
// cryptic issues caused by mismatching assembly architectures Node 16 ships
// with a native arm64 binary, and will set process.arch to "arm64" (instead of
// Rosetta emulated "x86")
if (semver.lt(process.version, "16.0.0")) {
  console.error("Requires node version 16 or above... Abort.");
  process.exit(1);
}

exports.vendorNinjaPath = vendorNinjaPath;
/**
 * By default we use vendored,
 * we produce two ninja files which won't overlap
 * one is build.ninja which use  vendored config
 * the other is env.ninja which use binaries from environment
 *
 * In dev mode, files generated for vendor config
 *
 * build.ninja
 * runtime/build.ninja
 * others/build.ninja
 * test/build.ninja
 *
 * files generated for env config
 *
 * env.ninja
 * compilerEnv.ninja (no snapshot since env can not provide snapshot)
 * runtime/env.ninja
 * others/env.ninja
 * test/env.ninja
 *
 * In release mode:
 *
 * release.ninja
 * runtime/release.ninja
 * others/release.ninja
 *
 * Like that our snapshot is so robust that
 * we don't do snapshot in CI, we don't
 * need do test build in CI either
 *
 */

/**
 * @type {string}
 */
var versionString = undefined;

/**
 *
 * @returns {string}
 */
var getVersionString = () => {
  if (versionString === undefined) {
    var searcher = "version";
    try {
      var output = cp.execSync(`ocamldep.opt -version`, {
        encoding: "ascii",
      });
      versionString = output
        .substring(output.indexOf(searcher) + searcher.length)
        .trim();
    } catch (err) {
      console.error(`This error probably came from that you don't have OCaml installed.
Make sure you have the OCaml compiler available in your path.`);
      console.error(err.message);
      process.exit(err.status);
    }
  }
  return versionString;
};

/**
 *
 * @param {string} ninjaCwd
 */
function ruleCC(ninjaCwd) {
  return `
rule cc
    command = $bsc -bs-cmi -bs-cmj $bsc_flags   -I ${ninjaCwd}  $in
    description = $in -> $out
rule cc_cmi
    command = $bsc -bs-read-cmi -bs-cmi -bs-cmj $bsc_flags  -I ${ninjaCwd}  $in
    description = $in -> $out
`;
}
/**
 *
 * @param {string} name
 * @param {string} content
 */
function writeFileAscii(name, content) {
  fs.writeFile(name, content, "ascii", throwIfError);
}

/**
 *
 * @param {string} name
 * @param {string} content
 */
function writeFileSync(name, content) {
  return fs.writeFileSync(name, content, "ascii");
}
/**
 *
 * @param {NodeJS.ErrnoException} err
 */
function throwIfError(err) {
  if (err !== null) {
    throw err;
  }
}
/**
 *
 * @typedef { {kind : "file" , name : string} | {kind : "pseudo" , name : string}} Target
 * @typedef {{key : string, value : string}} Override
 * @typedef { Target[]} Targets
 * @typedef {Map<string,TargetSet>} DepsMap
 */

class TargetSet {
  /**
   *
   * @param {Targets} xs
   */
  constructor(xs = []) {
    this.data = xs;
  }
  /**
   *
   * @param {Target} x
   */
  add(x) {
    var data = this.data;
    var found = false;
    for (var i = 0; i < data.length; ++i) {
      var cur = data[i];
      if (cur.kind === x.kind && cur.name === x.name) {
        found = true;
        break;
      }
    }
    if (!found) {
      this.data.push(x);
    }
    return this;
  }
  /**
   * @returns {Targets} a copy
   *
   */
  toSortedArray() {
    var newData = this.data.concat();
    newData.sort((x, y) => {
      var kindx = x.kind;
      var kindy = y.kind;
      if (kindx > kindy) {
        return 1;
      } else if (kindx < kindy) {
        return -1;
      } else {
        if (x.name > y.name) {
          return 1;
        } else if (x.name < y.name) {
          return -1;
        } else {
          return 0;
        }
      }
    });
    return newData;
  }
  /**
   *
   * @param {(item:Target)=>void} callback
   */
  forEach(callback) {
    this.data.forEach(callback);
  }
}

/**
 *
 * @param {string} target
 * @param {string} dependency
 * @param {DepsMap} depsMap
 */
function updateDepsKVByFile(target, dependency, depsMap) {
  var singleTon = fileTarget(dependency);
  if (depsMap.has(target)) {
    depsMap.get(target).add(singleTon);
  } else {
    depsMap.set(target, new TargetSet([singleTon]));
  }
}

/**
 *
 * @param {string} s
 */
function uncapitalize(s) {
  if (s.length === 0) {
    return s;
  }
  return s[0].toLowerCase() + s.slice(1);
}
/**
 *
 * @param {string} target
 * @param {string[]} dependencies
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByFile(target, dependencies, depsMap) {
  var targets = fileTargets(dependencies);
  if (depsMap.has(target)) {
    var s = depsMap.get(target);
    for (var i = 0; i < targets.length; ++i) {
      s.add(targets[i]);
    }
  } else {
    depsMap.set(target, new TargetSet(targets));
  }
}

/**
 *
 * @param {string} target
 * @param {string[]} modules
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByModule(target, modules, depsMap) {
  if (depsMap.has(target)) {
    let s = depsMap.get(target);
    for (let module of modules) {
      let filename = uncapitalize(module);
      let filenameAsCmi = filename + ".cmi";
      let filenameAsCmj = filename + ".cmj";
      if (target.endsWith(".cmi")) {
        if (depsMap.has(filenameAsCmi) || depsMap.has(filenameAsCmj)) {
          s.add(fileTarget(filenameAsCmi));
        }
      } else if (target.endsWith(".cmj")) {
        if (depsMap.has(filenameAsCmj)) {
          s.add(fileTarget(filenameAsCmj));
        } else if (depsMap.has(filenameAsCmi)) {
          s.add(fileTarget(filenameAsCmi));
        }
      }
    }
  }
}
/**
 *
 * @param {string[]}sources
 * @return {DepsMap}
 */
function createDepsMapWithTargets(sources) {
  /**
   * @type {DepsMap}
   */
  let depsMap = new Map();
  for (let source of sources) {
    let target = sourceToTarget(source);
    depsMap.set(target, new TargetSet([]));
  }
  depsMap.forEach((set, name) => {
    let cmiFile;
    if (
      name.endsWith(".cmj") &&
      depsMap.has((cmiFile = replaceExt(name, ".cmi")))
    ) {
      set.add(fileTarget(cmiFile));
    }
  });
  return depsMap;
}

/**
 *
 * @param {Target} file
 * @param {string} cwd
 */
function targetToString(file, cwd) {
  switch (file.kind) {
    case "file":
      return path.join(cwd, file.name);
    case "pseudo":
      return file.name;
    default:
      throw Error;
  }
}
/**
 *
 * @param {Targets} files
 * @param {string} cwd
 *
 * @returns {string} return a string separated with whitespace
 */
function targetsToString(files, cwd) {
  return files.map(x => targetToString(x, cwd)).join(" ");
}
/**
 *
 * @param {Targets} outputs
 * @param {Targets} inputs
 * @param {Targets} deps
 * @param {Override[]} overrides
 * @param {string} rule
 * @param {string} cwd
 * @return {string}
 */
function ninjaBuild(outputs, inputs, rule, deps, cwd, overrides) {
  var fileOutputs = targetsToString(outputs, cwd);
  var fileInputs = targetsToString(inputs, cwd);
  var stmt = `o ${fileOutputs} : ${rule} ${fileInputs}`;
  // deps.push(pseudoTarget('../lib/bsc'))
  if (deps.length > 0) {
    var fileDeps = targetsToString(deps, cwd);
    stmt += ` | ${fileDeps}`;
  }
  if (overrides.length > 0) {
    stmt +=
      `\n` +
      overrides
        .map(x => {
          return `    ${x.key} = ${x.value}`;
        })
        .join("\n");
  }
  return stmt;
}

/**
 *
 * @param {Target} outputs
 * @param {Targets} inputs
 * @param {string} cwd
 */
function phony(outputs, inputs, cwd) {
  return ninjaBuild([outputs], inputs, "phony", [], cwd, []);
}

/**
 *
 * @param {string | string[]} outputs
 * @param {string | string[]} inputs
 * @param {string | string[]} fileDeps
 * @param {string} rule
 * @param {string} cwd
 * @param {[string,string][]} overrides
 * @param {Target | Targets} extraDeps
 */
function ninjaQuickBuild(
  outputs,
  inputs,
  rule,
  cwd,
  overrides,
  fileDeps,
  extraDeps,
) {
  var os = Array.isArray(outputs)
    ? fileTargets(outputs)
    : [fileTarget(outputs)];
  var is = Array.isArray(inputs) ? fileTargets(inputs) : [fileTarget(inputs)];
  var ds = Array.isArray(fileDeps)
    ? fileTargets(fileDeps)
    : [fileTarget(fileDeps)];
  var dds = Array.isArray(extraDeps) ? extraDeps : [extraDeps];

  return ninjaBuild(
    os,
    is,
    rule,
    ds.concat(dds),
    cwd,
    overrides.map(x => {
      return { key: x[0], value: x[1] };
    }),
  );
}

/**
 * @typedef { (string | string []) } Strings
 * @typedef { [string,string]} KV
 * @typedef { [Strings, Strings,  string, string, KV[], Strings, (Target|Targets)] } BuildList
 * @param {BuildList[]} xs
 * @returns {string}
 */
function ninjaQuickBuildList(xs) {
  return xs
    .map(x => ninjaQuickBuild(x[0], x[1], x[2], x[3], x[4], x[5], x[6]))
    .join("\n");
}

/**
 *
 * @param {string} name
 * @returns {Target}
 */
function fileTarget(name) {
  return { kind: "file", name };
}

/**
 *
 * @param {string} name
 * @returns {Target}
 */
function pseudoTarget(name) {
  return { kind: "pseudo", name };
}

/**
 *
 * @param {string[]} args
 * @returns {Targets}
 */
function fileTargets(args) {
  return args.map(name => fileTarget(name));
}

/**
 *
 * @param {string[]} outputs
 * @param {string[]} inputs
 * @param {DepsMap} depsMap
 * @param {Override[]} overrides
 * @param {Targets} extraDeps
 * @param {string} rule
 * @param {string} cwd
 */
function buildStmt(outputs, inputs, rule, depsMap, cwd, overrides, extraDeps) {
  var os = outputs.map(fileTarget);
  var is = inputs.map(fileTarget);
  var deps = new TargetSet();
  for (var i = 0; i < outputs.length; ++i) {
    var curDeps = depsMap.get(outputs[i]);
    if (curDeps !== undefined) {
      curDeps.forEach(x => deps.add(x));
    }
  }
  extraDeps.forEach(x => deps.add(x));
  return ninjaBuild(os, is, rule, deps.toSortedArray(), cwd, overrides);
}

/**
 *
 * @param {string} x
 */
function replaceCmj(x) {
  return x.trim().replace("cmx", "cmj");
}

/**
 *
 * @param {string} y
 */
function sourceToTarget(y) {
  if (y.endsWith(".res")) {
    return replaceExt(y, ".cmj");
  } else if (y.endsWith(".resi")) {
    return replaceExt(y, ".cmi");
  }
  return y;
}
/**
 *
 * @param {string[]} files
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return {Promise<void>}
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 */
function ocamlDepForBscAsync(files, dir, depsMap) {
  return new Promise((resolve, reject) => {
    var tmpdir = null;
    const mlfiles = []; // convert .res files to temporary .ml files in tmpdir
    files.forEach(f => {
      const { name, ext } = path.parse(f);
      if (ext === ".res" || ext === ".resi") {
        const mlname = ext === ".resi" ? name + ".mli" : name + ".ml";
        if (tmpdir == null) {
          tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), "resToMl"));
        }
        try {
          const mlfile = path.join(tmpdir, mlname);
          cp.execSync(
            `${bsc_exe} -dsource -only-parse -bs-no-builtin-ppx ${f} 2>${mlfile}`,
            {
              cwd: dir,
              encoding: "ascii",
            },
          );
          mlfiles.push(mlfile);
        } catch (err) {
          console.log(err);
        }
      }
    });
    const minusI = tmpdir == null ? "" : `-I ${tmpdir}`;
    cp.exec(
      `ocamldep.opt -allow-approx -one-line ${minusI} -native ${files.join(
        " ",
      )} ${mlfiles.join(" ")}`,
      {
        cwd: dir,
        encoding: "ascii",
      },
      function (error, stdout, stderr) {
        if (tmpdir != null) {
          fs.rmSync(tmpdir, { recursive: true, force: true });
        }
        if (error !== null) {
          return reject(error);
        } else {
          const pairs = stdout.split("\n").map(x => x.split(":"));
          pairs.forEach(x => {
            var deps;
            let source = replaceCmj(path.basename(x[0]));
            if (x[1] !== undefined && (deps = x[1].trim())) {
              deps = deps.split(" ");
              updateDepsKVsByFile(
                source,
                deps.map(x => replaceCmj(path.basename(x))),
                depsMap,
              );
            }
          });
          return resolve();
        }
      },
    );
  });
}

/**
 *
 * @param {string[]} files
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return { Promise<void> []}
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 */
function depModulesForBscAsync(files, dir, depsMap) {
  let resFiles = files.filter(x => x.endsWith(".res") || x.endsWith(".resi"));
  /**
   *
   * @param {(value:void) =>void} resolve
   * @param {(value:any)=>void} reject
   */
  let cb = (resolve, reject) => {
    /**
     * @param {any} error
     * @param {string} stdout
     * @param {string} stderr
     */
    let fn = function (error, stdout, stderr) {
      if (error !== null) {
        return reject(error);
      } else {
        var pairs = stdout.split("\n").map(x => x.split(":"));
        pairs.forEach(x => {
          var modules;
          let source = sourceToTarget(x[0].trim());
          if (x[1] !== undefined && (modules = x[1].trim())) {
            modules = modules.split(" ");
            updateDepsKVsByModule(source, modules, depsMap);
          }
        });
        return resolve();
      }
    };
    return fn;
  };
  let config = {
    cwd: dir,
    encoding: "ascii",
  };
  return [
    new Promise((resolve, reject) => {
      cp.exec(
        `${bsc_exe} -modules -bs-syntax-only ${resFiles.join(" ")}`,
        config,
        cb(resolve, reject),
      );
    }),
  ];
}

/**
 * @typedef {('HAS_RES' | 'HAS_RESI' | 'HAS_BOTH_RES')} FileInfo
 * @param {string[]} sourceFiles
 * @returns {Map<string, FileInfo>}
 * We make a set to ensure that `sourceFiles` are not duplicated
 */
function collectTarget(sourceFiles) {
  /**
   * @type {Map<string,FileInfo>}
   */
  var allTargets = new Map();
  sourceFiles.forEach(x => {
    var { ext, name } = path.parse(x);
    var existExt = allTargets.get(name);
    if (existExt === undefined) {
      if (ext === ".res") {
        allTargets.set(name, "HAS_RES");
      } else if (ext === ".resi") {
        allTargets.set(name, "HAS_RESI");
      }
    } else {
      switch (existExt) {
        case "HAS_RES":
          if (ext === ".resi") {
            allTargets.set(name, "HAS_BOTH_RES");
          }
          break;
        case "HAS_RESI":
          if (ext === ".res") {
            allTargets.set(name, "HAS_BOTH_RES");
          }
          break;
        case "HAS_BOTH_RES":
          break;
      }
    }
  });
  return allTargets;
}

/**
 *
 * @param {Map<string, FileInfo>} allTargets
 * @param {string[]} collIn
 * @returns {string[]} A new copy which is
 *
 */
function scanFileTargets(allTargets, collIn) {
  var coll = collIn.concat();
  allTargets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_RESI":
        coll.push(`${mod}.cmi`);
        break;
      case "HAS_BOTH_RES":
        coll.push(`${mod}.cmi`, `${mod}.cmj`);
        break;
      case "HAS_RES":
        coll.push(`${mod}.cmi`, `${mod}.cmj`);
        break;
    }
  });
  return coll;
}

/**
 *
 * @param {DepsMap} depsMap
 * @param {Map<string,string>} allTargets
 * @param {string} cwd
 * @param {Targets} extraDeps
 * @return {string[]}
 */
function generateNinja(depsMap, allTargets, cwd, extraDeps = []) {
  /**
   * @type {string[]}
   */
  var build_stmts = [];
  allTargets.forEach((x, mod) => {
    let ouptput_cmj = mod + ".cmj";
    let output_cmi = mod + ".cmi";
    let input_res = mod + ".res";
    let input_resi = mod + ".resi";
    /**
     * @type {Override[]}
     */
    var overrides = [];
    // if (mod.endsWith("Labels")) {
    //   overrides.push({ key: "bsc_flags", value: "$bsc_flags -nolabels" });
    // }

    /**
     *
     * @param {string[]} outputs
     * @param {string[]} inputs
     *
     */
    let mk = (outputs, inputs, rule = "cc") => {
      return build_stmts.push(
        buildStmt(outputs, inputs, rule, depsMap, cwd, overrides, extraDeps),
      );
    };
    switch (x) {
      case "HAS_BOTH_RES":
        mk([ouptput_cmj], [input_res], "cc_cmi");
        mk([output_cmi], [input_resi]);
        break;
      case "HAS_RES":
        mk([output_cmi, ouptput_cmj], [input_res]);
        break;
      case "HAS_RESI":
        mk([output_cmi], [input_resi]);
        break;
    }
  });
  return build_stmts;
}

var COMPILIER = bsc_exe;
var BSC_COMPILER = `bsc = ${COMPILIER}`;

async function runtimeNinja(devmode = true) {
  var ninjaCwd = "runtime";
  var compilerTarget = pseudoTarget("$bsc");
  var externalDeps = devmode ? [compilerTarget] : [];
  var ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  var bsc_flags = "bsc_flags";
  /**
   * @type [string,string][]
   */
  var bsc_builtin_overrides = [[bsc_flags, `$${bsc_flags} -nopervasives`]];
  /**
   * @type [string,string][]
   */
  var templateRuntimeRules = `
${bsc_flags} = ${commonBsFlags} -bs-cross-module-opt -make-runtime -unsafe -w -3+50 -warn-error A
${ruleCC(ninjaCwd)}
${ninjaQuickBuildList([
  // we make it still depends on external
  // to enjoy free ride on dev config for compiler-deps

  [
    ["pervasives.cmj", "pervasives.cmi"],
    "pervasives.res",
    "cc",
    ninjaCwd,
    bsc_builtin_overrides,
    [],
    externalDeps,
  ],
  [
    ["js.cmj", "js.cmi"],
    "js.res",
    "cc",
    ninjaCwd,
    bsc_builtin_overrides,
    [],
    externalDeps,
  ],
])}
`;
  /**
   * @type {DepsMap}
   */
  var depsMap = new Map();
  var allTargets = collectTarget([...runtimeMliFiles, ...runtimeMlFiles]);
  var manualDeps = ["pervasives.cmj", "pervasives.cmi", "js.cmj", "js.cmi"];
  var allFileTargetsInRuntime = scanFileTargets(allTargets, manualDeps);
  allTargets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_RESI":
      case "HAS_BOTH_RES":
        updateDepsKVsByFile(mod + ".cmi", manualDeps, depsMap);
        break;
      case "HAS_RES":
        updateDepsKVsByFile(mod + ".cmj", manualDeps, depsMap);
        break;
    }
  });
  // FIXME: in dev mode, it should not rely on reading js file
  // since it may cause a bootstrapping issues
  try {
    await Promise.all([
      runJSCheckAsync(depsMap),
      ocamlDepForBscAsync(runtimeSourceFiles, runtimeDir, depsMap),
    ]);
    var stmts = generateNinja(depsMap, allTargets, ninjaCwd, externalDeps);
    stmts.push(
      phony(runtimeTarget, fileTargets(allFileTargetsInRuntime), ninjaCwd),
    );
    writeFileAscii(
      path.join(runtimeDir, ninjaOutput),
      templateRuntimeRules + stmts.join("\n") + "\n",
    );
  } catch (e) {
    console.log(e);
  }
}

async function othersNinja(devmode = true) {
  var compilerTarget = pseudoTarget("$bsc");
  var externalDeps = [
    compilerTarget,
    fileTarget("belt_internals.cmi"),
    fileTarget("js.cmi"),
  ];
  var ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  var ninjaCwd = "others";

  var templateOthersRules = `
bsc_primitive_flags =  ${commonBsFlags} -bs-cross-module-opt -make-runtime   -nopervasives  -unsafe  -w +50 -warn-error A
bsc_flags = $bsc_primitive_flags -open Belt_internals
${ruleCC(ninjaCwd)}
${ninjaQuickBuildList([
  [
    ["belt.cmj", "belt.cmi"],
    "belt.res",
    "cc",
    ninjaCwd,
    [["bsc_flags", "$bsc_primitive_flags"]],
    [],
    [compilerTarget],
  ],
  [
    ["js.cmj", "js.cmi"],
    "js.res",
    "cc",
    ninjaCwd,
    [["bsc_flags", "$bsc_primitive_flags"]],
    [],
    [compilerTarget],
  ],
  [
    ["belt_internals.cmi"],
    "belt_internals.resi",
    "cc",
    ninjaCwd,
    [["bsc_flags", "$bsc_primitive_flags"]],
    [],
    [compilerTarget],
  ],
])}
`;
  var othersDirFiles = fs.readdirSync(othersDir, "ascii");
  var jsPrefixSourceFiles = othersDirFiles.filter(
    x =>
      x.startsWith("js") &&
      (x.endsWith(".res") || x.endsWith(".resi")) &&
      !x.includes(".pp") &&
      !x.includes("#") &&
      x !== "js.res",
  );
  var othersFiles = othersDirFiles.filter(
    x =>
      !x.startsWith("js") &&
      x !== "belt.res" &&
      x !== "belt_internals.resi" &&
      (x.endsWith(".res") || x.endsWith(".resi")) &&
      !x.includes("#"),
  );
  var jsTargets = collectTarget(jsPrefixSourceFiles);
  var allJsTargets = scanFileTargets(jsTargets, []);
  let jsDepsMap = new Map();
  let depsMap = new Map();
  await Promise.all([
    ocamlDepForBscAsync(jsPrefixSourceFiles, othersDir, jsDepsMap),
    ocamlDepForBscAsync(othersFiles, othersDir, depsMap),
  ]);
  var jsOutput = generateNinja(jsDepsMap, jsTargets, ninjaCwd, externalDeps);
  jsOutput.push(phony(js_package, fileTargets(allJsTargets), ninjaCwd));

  // Note compiling belt.res still try to read
  // belt_xx.cmi we need enforce the order to
  // avoid data race issues
  var beltPackage = fileTarget("belt.cmi");
  var beltTargets = collectTarget(othersFiles);
  depsMap.forEach((s, k) => {
    if (k.startsWith("belt")) {
      s.add(beltPackage);
    }
    s.add(js_package);
  });
  var allOthersTarget = scanFileTargets(beltTargets, []);
  var beltOutput = generateNinja(depsMap, beltTargets, ninjaCwd, externalDeps);
  beltOutput.push(phony(othersTarget, fileTargets(allOthersTarget), ninjaCwd));
  writeFileAscii(
    path.join(othersDir, ninjaOutput),
    templateOthersRules +
      jsOutput.join("\n") +
      "\n" +
      beltOutput.join("\n") +
      "\n",
  );
}

/**
 *
 * @param {string} text
 */
function getDeps(text) {
  /**
   * @type {string[]}
   */
  var deps = [];
  text.replace(
    /(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g,
    function (_, ignore, id) {
      if (!ignore) deps.push(id);
      return ""; // TODO: examine the regex
    },
  );
  return deps;
}

/**
 *
 * @param {string} x
 * @param {string} newExt
 * @example
 *
 * ```js
 * replaceExt('xx.cmj', '.a') // return 'xx.a'
 * ```
 *
 */
function replaceExt(x, newExt) {
  let index = x.lastIndexOf(".");
  if (index < 0) {
    return x;
  }
  return x.slice(0, index) + newExt;
}
/**
 *
 * @param {string} x
 */
function baseName(x) {
  return x.substr(0, x.indexOf("."));
}

/**
 *
 * @returns {Promise<void>}
 */
async function testNinja() {
  var ninjaOutput = "build.ninja";
  var ninjaCwd = `test`;
  var templateTestRules = `
bsc_flags = -bs-cross-module-opt -make-runtime-test -bs-package-output commonjs:jscomp/test  -w -3-6-26-27-29-30-32..40-44-45-52-60-9-106+104 -warn-error A  -I runtime -I others
${ruleCC(ninjaCwd)}
`;
  var testDirFiles = fs.readdirSync(testDir, "ascii");
  //testDirFiles = [];
  var sources = testDirFiles.filter(x => {
    return x.endsWith(".resi") || x.endsWith(".res");
  });

  let depsMap = createDepsMapWithTargets(sources);
  await Promise.all(depModulesForBscAsync(sources, testDir, depsMap));
  var targets = collectTarget(sources);
  var output = generateNinja(depsMap, targets, ninjaCwd, [
    runtimeTarget,
    pseudoTarget("$bsc"),
  ]);
  output.push(
    phony(
      pseudoTarget("test"),
      fileTargets(scanFileTargets(targets, [])),
      ninjaCwd,
    ),
  );
  writeFileAscii(
    path.join(testDir, ninjaOutput),
    templateTestRules + output.join("\n") + "\n",
  );
}

/**
 *
 * @param {DepsMap} depsMap
 */
function runJSCheckAsync(depsMap) {
  return new Promise(resolve => {
    var count = 0;
    var tasks = runtimeJsFiles.length;
    var updateTick = () => {
      count++;
      if (count === tasks) {
        resolve(count);
      }
    };
    runtimeJsFiles.forEach(name => {
      var jsFile = path.join(jsDir, name + ".js");
      fs.readFile(jsFile, "utf8", function (err, fileContent) {
        if (err === null) {
          var deps = getDeps(fileContent).map(x => path.parse(x).name + ".cmj");
          fs.exists(path.join(runtimeDir, name + ".resi"), exist => {
            if (exist) {
              deps.push(name + ".cmi");
            }
            updateDepsKVsByFile(`${name}.cmj`, deps, depsMap);
            updateTick();
          });
        } else {
          // file non exist or reading error ignore
          updateTick();
        }
      });
    });
  });
}

function checkEffect() {
  var jsPaths = runtimeJsFiles.map(x => path.join(jsDir, x + ".js"));
  var effect = jsPaths
    .map(x => {
      return {
        file: x,
        content: fs.readFileSync(x, "utf8"),
      };
    })
    .map(({ file, content: x }) => {
      if (/No side effect|This output is empty/.test(x)) {
        return {
          file,
          effect: "pure",
        };
      } else if (/Not a pure module/.test(x)) {
        return {
          file,
          effect: "false",
        };
      } else {
        return {
          file,
          effect: "unknown",
        };
      }
    })
    .filter(({ effect }) => effect !== "pure")
    .map(({ file, effect }) => {
      return { file: path.basename(file), effect };
    });

  var black_list = new Set(["caml_lexer.js", "caml_parser.js"]);

  var assert = require("assert");
  // @ts-ignore
  assert(
    effect.length === black_list.size &&
      effect.every(x => black_list.has(x.file)),
  );

  console.log(effect);
}

function updateRelease() {
  runtimeNinja(false);
  othersNinja(false);
}

function updateDev() {
  writeFileAscii(
    path.join(jscompDir, "build.ninja"),
    `
${BSC_COMPILER}
ocamllex = ocamllex.opt
subninja runtime/build.ninja
subninja others/build.ninja
subninja test/build.ninja
o all: phony runtime others test
`,
  );
  writeFileAscii(
    path.join(jscompDir, "..", "lib", "build.ninja"),
    `
ocamlopt = ocamlopt.opt
ext = exe
INCL= "4.06.1+BS"
include body.ninja
`,
  );

  runtimeNinja();
  if (fs.existsSync(bsc_exe)) {
    testNinja();
  }
  othersNinja();
}
exports.updateDev = updateDev;
exports.updateRelease = updateRelease;

function main() {
  if (require.main === module) {
    if (process.argv.includes("-check")) {
      checkEffect();
    }

    var subcommand = process.argv[2];
    switch (subcommand) {
      case "build":
        try {
          cp.execFileSync(vendorNinjaPath, ["all"], {
            encoding: "utf8",
            cwd: jscompDir,
            stdio: [0, 1, 2],
          });
        } catch (e) {
          console.log(e.message);
          console.log(`please run "./scripts/ninja.js config" first`);
          process.exit(2);
        }
        break;
      case "clean":
        try {
          cp.execFileSync(vendorNinjaPath, ["-t", "clean"], {
            encoding: "utf8",
            cwd: jscompDir,
            stdio: [0, 1],
          });
        } catch (e) {}
        cp.execSync(
          `git clean -dfx jscomp ${my_target} lib && rm -rf lib/js/*.js && rm -rf lib/es6/*.js`,
          {
            encoding: "utf8",
            cwd: path.join(__dirname, ".."),
            stdio: [0, 1, 2],
          },
        );
        break;
      case "config":
        console.log(`config for the first time may take a while`);
        updateDev();
        updateRelease();

        break;
      case "cleanbuild":
        console.log(`run cleaning first`);
        cp.execSync(`node ${__filename} clean`, {
          cwd: __dirname,
          stdio: [0, 1, 2],
        });
        cp.execSync(`node ${__filename} config`, {
          cwd: __dirname,
          stdio: [0, 1, 2],
        });
        cp.execSync(`node ${__filename} build`, {
          cwd: __dirname,
          stdio: [0, 1, 2],
        });
        break;
      case "help":
        console.log(`supported subcommands:
[exe] config
[exe] build
[exe] cleanbuild
[exe] help
[exe] clean
        `);
        break;
      default:
        if (process.argv.length === 2) {
          updateDev();
          updateRelease();
        } else {
          var dev = process.argv.includes("-dev");
          var release = process.argv.includes("-release");
          var all = process.argv.includes("-all");
          if (all) {
            updateDev();
            updateRelease();
          } else if (dev) {
            updateDev();
          } else if (release) {
            updateRelease();
          }
        }
        break;
    }
  }
}

main();
