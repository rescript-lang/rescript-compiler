#!/usr/bin/env node
//@ts-check

var fs = require("fs");
var path = require("path");
var cp = require("child_process");

var jscompDir = path.join(__dirname, "..", "jscomp");
var runtimeDir = path.join(jscompDir, "runtime");
var othersDir = path.join(jscompDir, "others");
var testDir = path.join(jscompDir, "test");

var jsDir = path.join(__dirname, "..", "lib", "js");

var runtimeFiles = fs.readdirSync(runtimeDir, "ascii");
var runtimeMlFiles = runtimeFiles.filter(
  (x) => !x.startsWith("bs_stdlib_mini") && x.endsWith(".ml") && x !== "js.ml"
);
var runtimeMliFiles = runtimeFiles.filter(
  (x) => !x.startsWith("bs_stdlib_mini") && x.endsWith(".mli") && x !== "js.mli"
);
var runtimeSourceFiles = runtimeMlFiles.concat(runtimeMliFiles);
var runtimeJsFiles = [...new Set(runtimeSourceFiles.map(baseName))];

var commonBsFlags = `-no-keep-locs -no-alias-deps -bs-no-version-header -bs-no-check-div-by-zero -nostdlib `;
var js_package = pseudoTarget("js_pkg");
var runtimeTarget = pseudoTarget("runtime");
var othersTarget = pseudoTarget("others");
var stdlibTarget = pseudoTarget("$stdlib");

var vendorNinjaPath = path.join(__dirname, "..", process.platform, "ninja.exe");

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
 * compiler.ninja
 * snapshot.ninja
 * runtime/build.ninja
 * others/build.ninja
 * $stdlib/build.ninja
 * test/build.ninja
 *
 * files generated for env config
 *
 * env.ninja
 * compilerEnv.ninja (no snapshot since env can not provide snapshot)
 * runtime/env.ninja
 * others/env.ninja
 * $stdlib/env.ninja
 * test/env.ninja
 *
 * In release mode:
 *
 * release.ninja
 * runtime/release.ninja
 * others/release.ninja
 * $stdlib/release.ninja
 *
 * Like that our snapshot is so robust that
 * we don't do snapshot in CI, we don't
 * need do test build in CI either
 *
 */

/**
 * Note this file is not used in ninja file
 * It is used to generate ninja file
 * @returns {string}
 * Note ocamldep.opt has built-in macro handling OCAML_VERSION
 */
var getOcamldepFile = () => {
  return path.join(
    __dirname,
    "..",
    "native",
    require("./buildocaml.js").getVersionPrefix(),
    "bin",
    "ocamldep.opt"
  );
};

/**
 * @type {string}
 */
var versionString = undefined;

var getVersionString = () => {
  if (versionString === undefined) {
    var searcher = "version";
    try {
      var output = cp.execSync(`${getOcamldepFile()} -version`, {
        encoding: "ascii",
      });
      versionString = output
        .substring(output.indexOf(searcher) + searcher.length)
        .trim();
    } catch (err) {
      //
      console.error(`This error  probably came from that you don't have our vendored ocaml installed
      If this is the first time you clone the repo
      try this
      git submodule init && git submodule update
      node ./scripts/buildocaml.js
      `);
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
 * Fixed since it is already vendored
 */
var cppoMonoFile = `../vendor/cppo/cppo_bin.ml`;
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
  return files.map((x) => targetToString(x, cwd)).join(" ");
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
        .map((x) => {
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
  extraDeps
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
    overrides.map((x) => {
      return { key: x[0], value: x[1] };
    })
  );
}

/**
 * @typedef { (string | string []) } Strings
 * @typedef { [string,string]} KV
 * @typedef { [Strings, Strings,  string, string, KV[], Strings, (Target|Targets)] } BuildList
 * @param {BuildList[]} xs
 * @returns {string}
 */
function ninjaQuickBuidList(xs) {
  return xs
    .map((x) => ninjaQuickBuild(x[0], x[1], x[2], x[3], x[4], x[5], x[6]))
    .join("\n");
}

/**
 * @typedef { [string,string,string?]} CppoInput
 * @param {CppoInput[]} xs
 * @param {string} cwd
 * @returns {string}
 */
function cppoList(cwd, xs) {
  return xs
    .map((x) => {
      /**
       * @type {KV[]}
       */
      var variables;
      if (x[2]) {
        variables = [["type", `-D ${x[2]}`]];
      } else {
        variables = [];
      }
      var extraDeps = pseudoTarget(cppoFile);
      return ninjaQuickBuild(
        x[0],
        x[1],
        cppoRuleName,
        cwd,
        variables,
        [],
        extraDeps
      );
    })
    .join("\n");
}
/**
 *
 * @param {string} cwd
 * @param {string[]} xs
 * @returns {string}
 */
function mllList(cwd, xs) {
  return xs
    .map((x) => {
      var output = baseName(x) + ".ml";
      return ninjaQuickBuild(output, x, mllRuleName, cwd, [], [], []);
    })
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
  return args.map((name) => fileTarget(name));
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
      curDeps.forEach((x) => deps.add(x));
    }
  }
  extraDeps.forEach((x) => deps.add(x));
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
  if (y.endsWith(".ml") || y.endsWith(".re") || y.endsWith(".res")) {
    return replaceExt(y, ".cmj");
  } else if (y.endsWith(".mli") || y.endsWith(".rei") || y.endsWith(".resi")) {
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
    cp.exec(
      `${getOcamldepFile()} -allow-approx -one-line -native ${files.join(" ")}`,
      {
        cwd: dir,
        encoding: "ascii",
      },
      function (error, stdout, stderr) {
        if (error !== null) {
          return reject(error);
        } else {
          var pairs = stdout.split("\n").map((x) => x.split(":"));
          pairs.forEach((x) => {
            var deps;
            let source = replaceCmj(x[0]);
            if (x[1] !== undefined && (deps = x[1].trim())) {
              deps = deps.split(" ");
              updateDepsKVsByFile(
                source,
                deps.map((x) => replaceCmj(x)),
                depsMap
              );
            }
          });
          return resolve();
        }
      }
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
  let ocamlFiles = files.filter((x) => x.endsWith(".ml") || x.endsWith(".mli"));
  let reFiles = files.filter((x) => x.endsWith(".re") || x.endsWith(".rei"));
  let resFiles = files.filter((x) => x.endsWith(".res") || x.endsWith(".resi"));
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
        var pairs = stdout.split("\n").map((x) => x.split(":"));
        pairs.forEach((x) => {
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
        `${getOcamldepFile()} -allow-approx -modules -one-line -native ${ocamlFiles.join(
          " "
        )}`,
        config,
        cb(resolve, reject)
      );
    }),

    new Promise((resolve, reject) => {
      cp.exec(
        `${getOcamldepFile()} -pp '../../${
          process.platform
        }/refmt.exe --print=binary' -modules -one-line -native -ml-synonym .re -mli-synonym .rei ${reFiles.join(
          " "
        )}`,
        config,
        cb(resolve, reject)
      );
    }),
    new Promise((resolve, reject) => {
      cp.exec(
        `${getOcamldepFile()} -pp '../../${
          process.platform
        }/bsc.byte -as-pp' -modules -one-line -native -ml-synonym .res -mli-synonym .resi ${resFiles.join(
          " "
        )}`,
        config,
        cb(resolve, reject)
      );
    }),
  ];
}

/**
 * @typedef {('HAS_ML' | 'HAS_MLI' | 'HAS_BOTH' | 'HAS_RE' | 'HAS_RES' | 'HAS_REI' | 'HAS_RESI' | 'HAS_BOTH_RE' | 'HAS_BOTH_RES')} FileInfo
 * @param {string[]} sourceFiles
 * @returns {Map<string, FileInfo>}
 * We make a set to ensure that `sourceFiles` are not duplicated
 */
function collectTarget(sourceFiles) {
  /**
   * @type {Map<string,FileInfo>}
   */
  var allTargets = new Map();
  sourceFiles.forEach((x) => {
    var { ext, name } = path.parse(x);
    var existExt = allTargets.get(name);
    if (existExt === undefined) {
      if (ext === ".ml") {
        allTargets.set(name, "HAS_ML");
      } else if (ext === ".mli") {
        allTargets.set(name, "HAS_MLI");
      } else if (ext === ".re") {
        allTargets.set(name, "HAS_RE");
      } else if (ext === ".res") {
        allTargets.set(name, "HAS_RES");
      } else if (ext === ".rei") {
        allTargets.set(name, "HAS_REI");
      } else if (ext === ".resi") {
        allTargets.set(name, "HAS_RESI");
      }
    } else {
      switch (existExt) {
        case "HAS_ML":
          if (ext === ".mli") {
            allTargets.set(name, "HAS_BOTH");
          }
          break;
        case "HAS_RE":
          if (ext === ".rei") {
            allTargets.set(name, "HAS_BOTH_RE");
          }
          break;
        case "HAS_RES":
          if (ext === ".resi") {
            allTargets.set(name, "HAS_BOTH_RES");
          }
          break;
        case "HAS_MLI":
          if (ext === ".ml") {
            allTargets.set(name, "HAS_BOTH");
          }
          break;
        case "HAS_REI":
          if (ext === ".re") {
            allTargets.set(name, "HAS_BOTH_RE");
          }
          break;
        case "HAS_RESI":
          if (ext === ".res") {
            allTargets.set(name, "HAS_BOTH_RES");
          }
          break;
        case "HAS_BOTH_RE":
        case "HAS_BOTH":
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
      case "HAS_MLI":
      case "HAS_REI":
      case "HAS_RESI":
        coll.push(`${mod}.cmi`);
        break;
      case "HAS_BOTH_RES":
      case "HAS_BOTH_RE":
      case "HAS_BOTH":
        coll.push(`${mod}.cmi`, `${mod}.cmj`);
        break;
      case "HAS_RE":
      case "HAS_RES":
      case "HAS_ML":
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
    let input_ml = mod + ".ml";
    let input_mli = mod + ".mli";
    let input_re = mod + ".re";
    let input_res = mod + ".res";
    let input_rei = mod + ".rei";
    let input_resi = mod + ".resi";
    /**
     * @type {Override[]}
     */
    var overrides = [];
    if (mod.endsWith("Labels")) {
      overrides.push({ key: "bsc_flags", value: "$bsc_flags -nolabels" });
    }

    /**
     *
     * @param {string[]} outputs
     * @param {string[]} inputs
     *
     */
    let mk = (outputs, inputs, rule = "cc") => {
      return build_stmts.push(
        buildStmt(outputs, inputs, rule, depsMap, cwd, overrides, extraDeps)
      );
    };
    switch (x) {
      case "HAS_BOTH":
        mk([ouptput_cmj], [input_ml], "cc_cmi");
        mk([output_cmi], [input_mli]);
        break;
      case "HAS_BOTH_RE":
        mk([ouptput_cmj], [input_re], "cc_cmi");
        mk([output_cmi], [input_rei], "cc");
        break;
      case "HAS_BOTH_RES":
        mk([ouptput_cmj], [input_res], "cc_cmi");
        mk([output_cmi], [input_resi], "cc");
        break;
      case "HAS_RE":
        mk([output_cmi, ouptput_cmj], [input_re], "cc");
        break;
      case "HAS_RES":
        mk([output_cmi, ouptput_cmj], [input_res], "cc");
        break;
      case "HAS_ML":
        mk([output_cmi, ouptput_cmj], [input_ml]);
        break;
      case "HAS_REI":
        mk([output_cmi], [input_rei], "cc");
        break;
      case "HAS_RESI":
        mk([output_cmi], [input_resi], "cc");
        break;
      case "HAS_MLI":
        mk([output_cmi], [input_mli]);
        break;
    }
  });
  return build_stmts;
}

var COMPILIER = `../${process.platform}/bsc.exe`;
var BSC_COMPILER = `bsc = ${COMPILIER}`;
var compilerTarget = pseudoTarget(COMPILIER);

async function runtimeNinja(devmode = true) {
  var ninjaCwd = "runtime";
  var externalDeps = devmode ? [compilerTarget] : [];
  var ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  var templateRuntimeRules = `
bsc_no_open_flags =  ${commonBsFlags} -bs-cross-module-opt -make-runtime  -nopervasives  -unsafe -w +50 -warn-error A
bsc_flags = $bsc_no_open_flags -open Bs_stdlib_mini
${ruleCC(ninjaCwd)}
${ninjaQuickBuidList([
  [
    "bs_stdlib_mini.cmi",
    "bs_stdlib_mini.mli",
    "cc",
    ninjaCwd,
    [["bsc_flags", "-nostdlib -nopervasives"]],
    [],
    externalDeps,
  ],
  [
    ["js.cmj", "js.cmi"],
    "js.ml",
    "cc",
    ninjaCwd,
    [["bsc_flags", "$bsc_no_open_flags"]],
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
  var manualDeps = ["bs_stdlib_mini.cmi", "js.cmj", "js.cmi"];
  var allFileTargetsInRuntime = scanFileTargets(allTargets, manualDeps);
  allTargets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_MLI":
      case "HAS_BOTH":
        updateDepsKVsByFile(mod + ".cmi", manualDeps, depsMap);
        break;
      case "HAS_ML":
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
      phony(runtimeTarget, fileTargets(allFileTargetsInRuntime), ninjaCwd)
    );
    writeFileAscii(
      path.join(runtimeDir, ninjaOutput),
      templateRuntimeRules + stmts.join("\n") + "\n"
    );
  } catch (e) {
    console.log(e);
  }
}

var dTypeString = "TYPE_STRING";

var dTypeInt = "TYPE_INT";

var dTypeFunctor = "TYPE_FUNCTOR";

var dTypeLocalIdent = "TYPE_LOCAL_IDENT";

var dTypeIdent = "TYPE_IDENT";

var dTypePoly = "TYPE_POLY";

var cppoRuleName = `cppo`;
var cppoFile = `./bin/cppo.exe`;

var cppoRule = (flags = "") => `
rule ${cppoRuleName}
    command = ${cppoFile} -V OCAML:${getVersionString()} ${flags} $type $in -o $out
    generator = true
`;

var mllRuleName = `mll`;
var mllRule = `
rule ${mllRuleName}
    command = $ocamllex $in
    generator = true
`;
async function othersNinja(devmode = true) {
  var externalDeps = [runtimeTarget];
  var ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  var ninjaCwd = "others";

  var templateOthersRules = `
bsc_flags = ${commonBsFlags} -bs-cross-module-opt -make-runtime   -nopervasives  -unsafe  -w +50 -warn-error A  -open Bs_stdlib_mini -I ./runtime
${ruleCC(ninjaCwd)}
${
  devmode
    ? `${cppoRule()}
${cppoList(ninjaCwd, [
  ["belt_HashSetString.ml", "hashset.cppo.ml", dTypeString],
  ["belt_HashSetString.mli", "hashset.cppo.mli", dTypeString],
  ["belt_HashSetInt.ml", "hashset.cppo.ml", dTypeInt],
  ["belt_HashSetInt.mli", "hashset.cppo.mli", dTypeInt],
  ["belt_HashMapString.ml", "hashmap.cppo.ml", dTypeString],
  ["belt_HashMapString.mli", "hashmap.cppo.mli", dTypeString],
  ["belt_HashMapInt.ml", "hashmap.cppo.ml", dTypeInt],
  ["belt_HashMapInt.mli", "hashmap.cppo.mli", dTypeInt],
  ["belt_MapString.ml", "map.cppo.ml", dTypeString],
  ["belt_MapString.mli", "map.cppo.mli", dTypeString],
  ["belt_MapInt.ml", "map.cppo.ml", dTypeInt],
  ["belt_MapInt.mli", "map.cppo.mli", dTypeInt],
  ["belt_SetString.ml", "belt_Set.cppo.ml", dTypeString],
  ["belt_SetString.mli", "belt_Set.cppo.mli", dTypeString],
  ["belt_SetInt.ml", "belt_Set.cppo.ml", dTypeInt],
  ["belt_SetInt.mli", "belt_Set.cppo.mli", dTypeInt],
  ["belt_MutableMapString.ml", "mapm.cppo.ml", dTypeString],
  ["belt_MutableMapString.mli", "mapm.cppo.mli", dTypeString],
  ["belt_MutableMapInt.ml", "mapm.cppo.ml", dTypeInt],
  ["belt_MutableMapInt.mli", "mapm.cppo.mli", dTypeInt],
  ["belt_MutableSetString.ml", "setm.cppo.ml", dTypeString],
  ["belt_MutableSetString.mli", "setm.cppo.mli", dTypeString],
  ["belt_MutableSetInt.ml", "setm.cppo.ml", dTypeInt],
  ["belt_MutableSetInt.mli", "setm.cppo.mli", dTypeInt],
  ["belt_SortArrayString.ml", "sort.cppo.ml", dTypeString],
  ["belt_SortArrayString.mli", "sort.cppo.mli", dTypeString],
  ["belt_SortArrayInt.ml", "sort.cppo.ml", dTypeInt],
  ["belt_SortArrayInt.mli", "sort.cppo.mli", dTypeInt],
  ["belt_internalMapString.ml", "internal_map.cppo.ml", dTypeString],
  ["belt_internalMapInt.ml", "internal_map.cppo.ml", dTypeInt],
  ["belt_internalSetString.ml", "internal_set.cppo.ml", dTypeString],
  ["belt_internalSetInt.ml", "internal_set.cppo.ml", dTypeInt],
  ["js_typed_array.ml", "js_typed_array.cppo.ml", ""],
  ["js_typed_array2.ml", "js_typed_array2.cppo.ml", ""],
])}
`
    : `
`
}
${ninjaQuickBuidList([
  [["belt.cmj", "belt.cmi"], "belt.ml", "cc", ninjaCwd, [], [], externalDeps],
  [["node.cmj", "node.cmi"], "node.ml", "cc", ninjaCwd, [], [], externalDeps],
])}
`;
  var othersDirFiles = fs.readdirSync(othersDir, "ascii");
  var jsPrefixSourceFiles = othersDirFiles.filter(
    (x) =>
      x.startsWith("js") &&
      (x.endsWith(".ml") || x.endsWith(".mli")) &&
      !x.includes(".cppo") &&
      !x.includes("#")
  );
  var othersFiles = othersDirFiles.filter(
    (x) =>
      !x.startsWith("js") &&
      x !== "belt.ml" &&
      x !== "node.ml" &&
      (x.endsWith(".ml") || x.endsWith(".mli")) &&
      !x.includes("#") &&
      !x.includes(".cppo") // we have node ..
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

  // Note compiling belt.ml still try to read
  // belt_xx.cmi we need enforce the order to
  // avoid data race issues
  var beltPackage = fileTarget("belt.cmi");
  var nodePackage = fileTarget("node.cmi");
  var beltTargets = collectTarget(othersFiles);
  depsMap.forEach((s, k) => {
    if (k.startsWith("belt")) {
      s.add(beltPackage);
    } else if (k.startsWith("node")) {
      s.add(nodePackage);
    }
    s.add(js_package);
  });
  var allOthersTarget = scanFileTargets(beltTargets, []);
  var beltOutput = generateNinja(depsMap, beltTargets, ninjaCwd, externalDeps);
  beltOutput.push(phony(othersTarget, fileTargets(allOthersTarget), ninjaCwd));
  // ninjaBuild([`belt_HashSetString.ml`,])
  writeFileAscii(
    path.join(othersDir, ninjaOutput),
    templateOthersRules +
      jsOutput.join("\n") +
      "\n" +
      beltOutput.join("\n") +
      "\n"
  );
}
/**
 *
 * @param {boolean} devmode
 * generate build.ninja/release.ninja for stdlib-402
 */
async function stdlibNinja(devmode = true) {
  var stdlibVersion = "stdlib-406";
  var ninjaCwd = stdlibVersion;
  var stdlibDir = path.join(jscompDir, stdlibVersion);
  var externalDeps = [othersTarget];
  var ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  var bsc_flags = "bsc_flags";
  /**
   * @type [string,string][]
   */
  var bsc_builtin_overrides = [[bsc_flags, `$${bsc_flags} -nopervasives`]];
  // It is interesting `-w -a` would generate not great code sometimes
  // deprecations diabled due to string_of_float
  var warnings = "-w -9-3-106 -warn-error A";
  var templateStdlibRules = `
${bsc_flags} = ${commonBsFlags} -bs-cross-module-opt -make-runtime    ${warnings}  -I runtime  -I others
${ruleCC(ninjaCwd)}
${ninjaQuickBuidList([
  [
    "camlinternalFormatBasics.cmi",
    "camlinternalFormatBasics.mli",
    "cc",
    ninjaCwd,
    bsc_builtin_overrides,
    [],
    externalDeps,
  ],
  // we make it still depends on external
  // to enjoy free ride on dev config for compiler-deps

  [
    "camlinternalFormatBasics.cmj",
    "camlinternalFormatBasics.ml",
    "cc_cmi",
    ninjaCwd,
    bsc_builtin_overrides,
    "camlinternalFormatBasics.cmi",
    externalDeps,
  ],
  [
    "pervasives.cmj",
    "pervasives.ml",
    "cc_cmi",
    ninjaCwd,
    bsc_builtin_overrides,
    "pervasives.cmi",
    externalDeps,
  ],
  [
    "pervasives.cmi",
    "pervasives.mli",
    "cc",
    ninjaCwd,
    bsc_builtin_overrides,
    "camlinternalFormatBasics.cmj",
    externalDeps,
  ],
])}
`;
  var stdlibDirFiles = fs.readdirSync(stdlibDir, "ascii");
  var sources = stdlibDirFiles.filter((x) => {
    return (
      !x.startsWith("camlinternalFormatBasics") &&
      !x.startsWith("pervasives") &&
      (x.endsWith(".ml") || x.endsWith(".mli"))
    );
  });
  let depsMap = new Map();
  await ocamlDepForBscAsync(sources, stdlibDir, depsMap);
  var targets = collectTarget(sources);
  var allTargets = scanFileTargets(targets, [
    "camlinternalFormatBasics.cmi",
    "camlinternalFormatBasics.cmj",
    "pervasives.cmi",
    "pervasives.cmj",
  ]);
  targets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_MLI":
      case "HAS_BOTH":
        updateDepsKVByFile(mod + ".cmi", "pervasives.cmj", depsMap);
        break;
      case "HAS_ML":
        updateDepsKVByFile(mod + ".cmj", "pervasives.cmj", depsMap);
        break;
    }
  });
  var output = generateNinja(depsMap, targets, ninjaCwd, externalDeps);
  output.push(phony(stdlibTarget, fileTargets(allTargets), ninjaCwd));

  writeFileAscii(
    path.join(stdlibDir, ninjaOutput),
    templateStdlibRules + output.join("\n") + "\n"
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
    }
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
bsc_flags = -bs-no-version-header  -bs-cross-module-opt -make-runtime-test -bs-package-output commonjs:jscomp/test  -w -3-6-26-27-29-30-32..40-44-45-52-60-9-106+104 -warn-error A  -I runtime -I $stdlib -I others
${ruleCC(ninjaCwd)}


${mllRule}
${mllList(ninjaCwd, [
  "arith_lexer.mll",
  "number_lexer.mll",
  "simple_lexer_test.mll",
])}
`;
  var testDirFiles = fs.readdirSync(testDir, "ascii");
  var sources = testDirFiles.filter((x) => {
    return (
      x.endsWith(".re") ||
      x.endsWith(".rei") ||
      x.endsWith(".resi") ||
      x.endsWith(".res") ||
      ((x.endsWith(".ml") || x.endsWith(".mli")) && !x.endsWith("bspack.ml"))
    );
  });

  let depsMap = createDepsMapWithTargets(sources);
  await Promise.all(depModulesForBscAsync(sources, testDir, depsMap));
  var targets = collectTarget(sources);
  var output = generateNinja(depsMap, targets, ninjaCwd, [stdlibTarget]);
  writeFileAscii(
    path.join(testDir, ninjaOutput),
    templateTestRules + output.join("\n") + "\n"
  );
}

/**
 *
 * @param {DepsMap} depsMap
 */
function runJSCheckAsync(depsMap) {
  return new Promise((resolve) => {
    var count = 0;
    var tasks = runtimeJsFiles.length;
    var updateTick = () => {
      count++;
      if (count === tasks) {
        resolve(count);
      }
    };
    runtimeJsFiles.forEach((name) => {
      var jsFile = path.join(jsDir, name + ".js");
      fs.readFile(jsFile, "utf8", function (err, fileContent) {
        if (err === null) {
          var deps = getDeps(fileContent).map(
            (x) => path.parse(x).name + ".cmj"
          );
          fs.exists(path.join(runtimeDir, name + ".mli"), (exist) => {
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
  var jsPaths = runtimeJsFiles.map((x) => path.join(jsDir, x + ".js"));
  var effect = jsPaths
    .map((x) => {
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

  var black_list = new Set([
    "caml_int32.js",
    "caml_int64.js",
    "caml_lexer.js",
    "caml_parser.js",
  ]);

  var assert = require("assert");
  // @ts-ignore
  assert(
    effect.length === black_list.size &&
      effect.every((x) => black_list.has(x.file))
  );

  console.log(effect);
}

/**
 *
 * @param {string[]} domain
 * @param {Map<string,Set<string>>} dependency_graph
 * @returns {string[]}
 */
function sortFilesByDeps(domain, dependency_graph) {
  /**
   * @type{string[]}
   */
  var result = [];
  var workList = new Set(domain);
  /**
   *
   * @param {Set<string>} visiting
   * @param {string[]} path
   * @param {string} current
   */
  var visit = function (visiting, path, current) {
    if (visiting.has(current)) {
      throw new Error(`cycle: ${path.concat(current).join(" ")}`);
    }
    if (workList.has(current)) {
      visiting.add(current);
      var next = dependency_graph.get(current);
      if (next !== undefined && next.size > 0) {
        next.forEach((x) => {
          visit(visiting, path.concat(current), x);
        });
      }
      visiting.delete(current);
      workList.delete(current);
      result.push(current);
    }
  };
  while (workList.size > 0) {
    visit(new Set(), [], workList.values().next().value);
  }
  return result;
}

function updateRelease() {
  runtimeNinja(false);
  stdlibNinja(false);
  othersNinja(false);
}

function updateDev() {
  writeFileAscii(
    path.join(jscompDir, "build.ninja"),
    `
${getVendorConfigNinja()}
stdlib = stdlib-406
${BSC_COMPILER}      
subninja compiler.ninja
subninja runtime/build.ninja
subninja others/build.ninja
subninja $stdlib/build.ninja
subninja test/build.ninja
o all: phony runtime others $stdlib test
`
  );
  writeFileAscii(
    path.join(jscompDir, "..", "lib", "build.ninja"),
    `
ocamlopt = ocamlopt.opt 
ext = exe
INCL= "4.06.1+BS"
include body.ninja               
`
  );

  preprocessorNinjaSync();
  nativeNinja();
  runtimeNinja();
  stdlibNinja(true);
  testNinja();
  othersNinja();
}
exports.updateDev = updateDev;
exports.updateRelease = updateRelease;

/**
 *
 * @param {string} dir
 */
function readdirSync(dir) {
  return fs.readdirSync(dir, "ascii");
}

/**
 *
 * @param {string} dir
 */
function test(dir) {
  return readdirSync(path.join(jscompDir, dir))
    .filter((x) => {
      return (
        (x.endsWith(".ml") || x.endsWith(".mli")) &&
        !(x.endsWith(".cppo.ml") || x.endsWith(".cppo.mli"))
      );
    })
    .map((x) => path.join(dir, x));
}

/**
 *
 * @param {Set<string>} xs
 * @returns {string}
 */
function setSortedToStringAsNativeDeps(xs) {
  var arr = Array.from(xs).sort();
  // it relies on we have -opaque, so that .cmx is dummy file
  return arr.join(" ").replace(/\.cmx/g, ".cmi");
}

/**
 * @returns {string}
 */
function getVendorConfigNinja() {
  var prefix = `../native/${require("./buildocaml.js").getVersionPrefix()}/bin`;
  return `
ocamlopt = ${prefix}/ocamlopt.opt
ocamllex = ${prefix}/ocamllex.opt
ocamlc = ${prefix}/ocamlc.opt
ocamlmklib = ${prefix}/ocamlmklib
ocaml = ${prefix}/ocaml
`;
}

/**
 * @returns {string}
 */
function getPreprocessorFileName() {
  return "cppoVendor.ninja";
}
/**
 * Built cppo.exe refmt.exe etc for dev purpose
 */
function preprocessorNinjaSync() {
  var refmtMainPath = "../lib/4.06.1";
  var napkinFiles = fs
    .readdirSync(path.join(jscompDir, "..", "syntax", "src"), "ascii")
    .filter((x) => x.endsWith(".ml") || x.endsWith(".mli"));
  var buildNapkinFiles = napkinFiles
    .map((file) => `o napkin/${file} : copy ../syntax/src/${file}`)
    .join("\n");
  var cppoNative = `
${getVendorConfigNinja()}
rule link
    command =  $ocamlopt -g  -I +compiler-libs $flags $libs $in -o $out
rule bytelink
    command =  $ocamlc -g  -I +compiler-libs $flags $libs $in -o $out
o ${cppoFile}: link ${cppoMonoFile}
    libs = unix.cmxa str.cmxa
    generator = true
${cppoRule()}
${cppoList("ext", [
  ["hash_set_string.ml", "hash_set.cppo.ml", dTypeString],
  ["hash_set_int.ml", "hash_set.cppo.ml", dTypeInt],
  ["hash_set_ident.ml", "hash_set.cppo.ml", dTypeIdent],
  ["hash_set.ml", "hash_set.cppo.ml", dTypeFunctor],
  ["hash_set_poly.ml", "hash_set.cppo.ml", dTypePoly],
  ["vec_int.ml", "vec.cppo.ml", dTypeInt],
  ["vec.ml", "vec.cppo.ml", dTypeFunctor],
  ["set_string.ml", "set.cppo.ml", dTypeString],
  ["set_int.ml", "set.cppo.ml", dTypeInt],
  ["set_ident.ml", "set.cppo.ml", dTypeIdent],
  ["map_string.ml", "map.cppo.ml", dTypeString],
  ["map_int.ml", "map.cppo.ml", dTypeInt],
  ["map_ident.ml", "map.cppo.ml", dTypeIdent],
  [
    "ordered_hash_map_local_ident.ml",
    "ordered_hash_map.cppo.ml",
    dTypeLocalIdent,
  ],
  ["hash_string.ml", "hash.cppo.ml", dTypeString],
  ["hash_int.ml", "hash.cppo.ml", dTypeInt],
  ["hash_ident.ml", "hash.cppo.ml", dTypeIdent],
  ["hash.ml", "hash.cppo.ml", dTypeFunctor],
])}
${cppoList("outcome_printer", [
  ["tweaked_reason_oprint.ml", "tweaked_reason_oprint.cppo.ml", ""],
  ["reason_syntax_util.ml", "reason_syntax_util.cppo.ml", ""],
  ["reason_syntax_util.mli", "reason_syntax_util.cppo.mli", ""],
])}
o ../${
    process.platform
  }/refmt.exe: bytelink  ${refmtMainPath}/refmt_main3.mli ${refmtMainPath}/refmt_main3.ml
    libs = ocamlcommon.cma
    flags = -I ${refmtMainPath} -I +compiler-libs -w -40-30-3 -no-alias-deps
    generator = true
o ../${
    process.platform
  }/bsc.byte: bytelink  ${refmtMainPath}/whole_compiler.mli ${refmtMainPath}/whole_compiler.ml
      libs = ocamlcommon.cma
      flags = -custom ./stubs/ext_basic_hash_stubs.c -I ${refmtMainPath} -I +compiler-libs -w -40-30-3 -no-alias-deps
      generator = true
  
rule copy
  command = cp $in $out
  description = $in -> $out    
${buildNapkinFiles}    
`;
  var cppoNinjaFile = getPreprocessorFileName();
  writeFileSync(path.join(jscompDir, cppoNinjaFile), cppoNative);
  cp.execFileSync(vendorNinjaPath, ["-f", cppoNinjaFile, "--verbose"], {
    cwd: jscompDir,
    stdio: [0, 1, 2],
    encoding: "utf8",
  });
}
/**
 * Note don't run `ninja -t clean -g`
 * Since it will remove generated ml file which has
 * an effect on depfile
 */
function nativeNinja() {
  var ninjaOutput = "compiler.ninja";
  var sourceDirs = [
    "stubs",
    "ext",
    "common",
    "js_parser",
    "frontend",
    "depends",
    "core",
    "super_errors",
    "outcome_printer",
    "bsb",
    "bsb_helper",
    "ounit",
    "napkin",
    "ounit_tests",
    "main",
  ];
  var includes = sourceDirs.map((x) => `-I ${x}`).join(" ");

  var templateNative = `
subninja ${getPreprocessorFileName()}
compilerlibs := ../native/4.06.1/lib/ocaml/compiler-libs/ocamlcommon.cmxa
rule optc
    command = $ocamlopt -strict-sequence -safe-string -I +compiler-libs -opaque ${includes} -g -linscan -w A-4-9-40..42-30-48-50 -warn-error A -absname -c $in # $compilerlibs
    description = $out : $in
rule archive
    command = $ocamlopt -a $in -o $out
    description = arcive -> $out
rule link
    command =  $ocamlopt -g  -I +compiler-libs $flags $libs $in -o $out # $compilerlibs
    description = linking -> $out
rule mk_bsversion
    command = node $in
    generator = true
rule gcc
    command = $ocamlopt -ccopt -fPIC -ccopt -O2 -ccopt -o -ccopt $out -c $in
o stubs/ext_basic_hash_stubs.o : gcc  stubs/ext_basic_hash_stubs.c
rule ocamlmklib
    command = $ocamlmklib -v $in -o $name && touch $out

rule mk_keywords
    command = $ocaml $in
    generator = true
o ext/js_reserved_map.ml: mk_keywords ../scripts/build_sorted.ml keywords.list

o stubs/libbs_hash.a stubs/dllbs_hash.so: ocamlmklib stubs/ext_basic_hash_stubs.o
    name = stubs/bs_hash
rule stubslib
    command = $ocamlopt -a $ml -o $out -cclib $clib
o stubs/stubs.cmxa : stubslib stubs/bs_hash_stubs.cmx stubs/libbs_hash.a
    ml = stubs/bs_hash_stubs.cmx
    clib = stubs/libbs_hash.a

rule p4of
    command = node ../ocaml-tree/wasm.js $flags -i $i -o $out
    generator = true
o core/js_fold.ml: p4of core/j.ml
    flags = -fold
o core/js_record_iter.ml: p4of core/j.ml
    flags = -record-iter
o core/js_record_map.ml: p4of core/j.ml
    flags = -record-map
o core/js_record_fold.ml: p4of core/j.ml
    flags = -record-fold

o ../${
    process.platform
  }/bsc.exe: link  js_parser/js_parser.cmxa stubs/stubs.cmxa ext/ext.cmxa napkin/napkin.cmxa common/common.cmxa frontend/frontend.cmxa depends/depends.cmxa super_errors/super_errors.cmxa outcome_printer/outcome_printer.cmxa core/core.cmxa main/rescript_compiler_main.cmx
    libs = ocamlcommon.cmxa
o ../${
    process.platform
  }/bsb.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa bsb/bsb.cmxa main/bsb_main.cmx
    libs = ocamlcommon.cmxa unix.cmxa str.cmxa
o ../${
    process.platform
  }/rescript.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa bsb/bsb.cmxa main/rescript_main.cmx
      libs = ocamlcommon.cmxa unix.cmxa str.cmxa    
o ../${
    process.platform
  }/bsb_helper.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa  bsb/bsb.cmxa bsb_helper/bsb_helper.cmxa main/bsb_helper_main.cmx
    libs = ocamlcommon.cmxa unix.cmxa str.cmxa
o ./bin/bspack.exe: link stubs/stubs.cmxa ext/ext.cmxa ./common/common.cmxa ./frontend/frontend.cmxa depends/depends.cmxa ./main/bspack_main.cmx
    libs = unix.cmxa ocamlcommon.cmxa
    flags = -I ./bin -w -40-30    
o ./bin/cmjdump.exe: link ./stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa frontend/frontend.cmxa depends/depends.cmxa core/core.cmxa main/cmjdump_main.cmx
    libs = ocamlcommon.cmxa    
o ./bin/cmij.exe: link ./stubs/stubs.cmxa ext/ext.cmxa  common/common.cmxa frontend/frontend.cmxa depends/depends.cmxa core/core.cmxa main/cmij_main.cmx
    libs = ocamlcommon.cmxa

rule bspack
    command = ./bin/bspack.exe $flags -bs-main $main -o $out
    depfile = $out.d
    generator = true
o ./bin/tests.exe: link ounit/ounit.cmxa stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa frontend/frontend.cmxa depends/depends.cmxa bsb/bsb.cmxa bsb_helper/bsb_helper.cmxa core/core.cmxa ounit_tests/ounit_tests.cmxa main/ounit_tests_main.cmx
    libs = str.cmxa unix.cmxa ocamlcommon.cmxa

${mllRule}
${mllList("ext", ["ext_json_parse.mll"])}


rule mk_shared
    command = $ocamlopt -I +compiler-libs -shared $flags -o $out $in
o ../odoc_gen/generator.cmxs : mk_shared ../odoc_gen/generator.mli ../odoc_gen/generator.ml
    flags = -I +ocamldoc -I ../odoc_gen -absname
`;
  /**
   * @type { {name : string, libs: string[]}[]}
   */
  var libs = [];
  sourceDirs.forEach((name) => {
    if (name !== "main" && name !== "stubs") {
      libs.push({ name, libs: [] });
    }
  });

  /**
   * @type{string[]}
   */
  var files = [];
  for (let dir of sourceDirs) {
    files = files.concat(test(dir));
  }

  cp.exec(
    `${getOcamldepFile()} -allow-approx -one-line -native ${includes} ${files.join(
      " "
    )}`,
    { cwd: jscompDir, encoding: "ascii" },
    function (error, out) {
      if (error !== null) {
        throw error;
      }
      /**
       * @type {Map<string,Set<string>>}
       */
      var map = new Map();

      var pairs = out.split("\n").map((x) => x.split(":").map((x) => x.trim()));
      pairs.forEach((pair) => {
        /**
         * @type {string[]|string}
         */

        var deps;
        var key = pair[0];
        if (pair[1] !== undefined && (deps = pair[1].trim())) {
          // deps = deps.replace(/.cmx/g, ".cmi");
          deps = deps.split(" ");
          map.set(key, new Set(deps));
        }
        if (key.endsWith("cmx")) {
          libs.forEach((x) => {
            if (path.dirname(key) === x.name) {
              x.libs.push(key);
            }
          });
        }
      });

      // not ocamldep output
      // when no mli exists no deps for cmi otherwise add cmi
      var stmts = pairs.map((pair) => {
        if (pair[0]) {
          var target = pair[0];
          var y = path.parse(target);
          /**
           * @type {Set<string>}
           */
          var deps = map.get(target) || new Set();
          if (y.ext === ".cmx") {
            var intf = path.join(y.dir, y.name + ".cmi");
            var ml = path.join(y.dir, y.name + ".ml");
            return `o ${
              deps.has(intf) ? target : [target, intf].join(" ")
            } : optc ${ml} | ${setSortedToStringAsNativeDeps(deps)}`;
          } else {
            // === 'cmi'
            var mli = path.join(y.dir, y.name + ".mli");
            return `o ${target} : optc ${mli} | ${setSortedToStringAsNativeDeps(
              deps
            )}`;
          }
        }
      });
      libs.forEach((x) => {
        var output = sortFilesByDeps(x.libs, map);
        var name = x.name;
        stmts.push(`o ${name}/${name}.cmxa : archive ${output.join(" ")}`);
      });

      writeFileAscii(
        path.join(jscompDir, ninjaOutput),
        templateNative + stmts.join("\n") + "\n"
      );
    }
  );
}

function main() {
  var emptyCount = 2;
  var isPlayground = false;
  if (require.main === module) {
    if (process.argv.includes("-check")) {
      checkEffect();
    }
    if (process.argv.includes("-playground")) {
      isPlayground = true;
      emptyCount++;
    }

    var subcommand = process.argv[2];
    switch (subcommand) {
      case "build":
        try {
          cp.execFileSync(vendorNinjaPath, ["-k", "1"], {
            encoding: "utf8",
            cwd: jscompDir,
            stdio: [0, 1, 2],
          });
          if (!isPlayground) {
            cp.execFileSync(
              path.join(__dirname, "..", "jscomp", "bin", "cmij.exe"),
              {
                encoding: "utf8",
                cwd: jscompDir,
                stdio: [0, 1, 2],
              }
            );
          }
          cp.execFileSync(vendorNinjaPath, ["-f", "snapshot.ninja"], {
            encoding: "utf8",
            cwd: jscompDir,
            stdio: [0, 1, 2],
          });
        } catch (e) {
          console.log(e.message);
          console.log(`please run "./scripts/ninja.js config" first`);
          process.exit(2);
        }
        cp.execSync(`node ${__filename} config`, {
          cwd: __dirname,
          stdio: [0, 1, 2],
        });
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
          `git clean -dfx jscomp ${process.platform} lib && rm -rf lib/js/*.js && rm -rf lib/es6/*.js`,
          {
            encoding: "utf8",
            cwd: path.join(__dirname, ".."),
            stdio: [0, 1, 2],
          }
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
      case "docs":
        console.log(`building docs`);
        require("./doc_gen").main();
        break;
      case "help":
        console.log(`supported subcommands:
[exe] config        
[exe] build
[exe] cleanbuild
[exe] docs
[exe] help
[exe] clean
        `);
        break;
      default:
        if (process.argv.length === emptyCount) {
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
