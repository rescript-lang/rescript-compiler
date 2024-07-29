#!/usr/bin/env node

// @ts-check

const os = require("node:os");
const fs = require("node:fs");
const path = require("node:path");
const cp = require("node:child_process");
const semver = require("semver");

const {
  absolutePath: my_target,
  bsc_exe,
  ninja_exe: vendorNinjaPath,
} = require("#cli/bin_path");
const {
  compilerRootDir,
  compilerRuntimeDir,
  compilerBuiltinDir,
  compilerTestDir,
  compilerBuiltinCjsOutputDir,
} = require("#internal/paths");

const runtimeFiles = fs.readdirSync(compilerRuntimeDir, "ascii");
const runtimeMlFiles = runtimeFiles.filter(
  x => !x.startsWith("bs_stdlib_mini") && x.endsWith(".res") && x !== "js.res",
);
const runtimeMliFiles = runtimeFiles.filter(
  x =>
    !x.startsWith("bs_stdlib_mini") && x.endsWith(".resi") && x !== "js.resi",
);
const runtimeSourceFiles = runtimeMlFiles.concat(runtimeMliFiles);
const runtimeJsFiles = [...new Set(runtimeSourceFiles.map(baseName))];

const commonBsFlags =
  "-no-keep-locs -no-alias-deps -bs-no-version-header -bs-no-check-div-by-zero -nostdlib ";
const js_package = pseudoTarget("js_pkg");
const runtimeTarget = pseudoTarget("runtime");
const othersTarget = pseudoTarget("others");
const stdlibTarget = pseudoTarget("$stdlib");

// Let's enforce a Node version >= 16 to make sure M1 users don't trip up on
// cryptic issues caused by mismatching assembly architectures Node 16 ships
// with a native arm64 binary, and will set process.arch to "arm64" (instead of
// Rosetta emulated "x86")
if (semver.lt(process.version, "16.0.0")) {
  console.error("Requires node version 16 or above... Abort.");
  process.exit(1);
}

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
 * @type {string | undefined}
 */
let versionString = undefined;

/**
 * @returns {string}
 */
function getVersionString() {
  if (versionString === undefined) {
    const searcher = "version";
    try {
      const output = cp.execSync("ocamldep.opt -version", {
        encoding: "ascii",
      });
      versionString = output
        .substring(output.indexOf(searcher) + searcher.length)
        .trim();
    } catch (err) {
      throw new Error(
        `This error probably came from that you don't have OCaml installed.
Make sure you have the OCaml compiler available in your path.`,
        {
          cause: err,
        },
      );
    }
  }
  return versionString;
}

/**
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
 * @param {string} name
 * @param {string} content
 */
function writeFileAscii(name, content) {
  fs.writeFile(name, content, "ascii", throwIfError);
}

/**
 * @param {string} name
 * @param {string} content
 */
function writeFileSync(name, content) {
  return fs.writeFileSync(name, content, "ascii");
}
/**
 * @param {NodeJS.ErrnoException | null} err
 */
function throwIfError(err) {
  if (err !== null) {
    throw err;
  }
}
/**
 * @typedef {{ kind: "file", name: string } | { kind: "pseudo", name: string }} Target
 * @typedef {{ key: string, value: string }} Override
 * @typedef {Target[]} Targets
 * @typedef {Map<string, TargetSet>} DepsMap
 */

class TargetSet {
  /**
   * @param {Targets} xs
   */
  constructor(xs = []) {
    this.data = xs;
  }

  [Symbol.iterator]() {
    return this.data[Symbol.iterator]();
  }

  /**
   * @param {Target} x
   */
  add(x) {
    const data = this.data;
    let found = false;
    for (const cur of data) {
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
   */
  toSortedArray() {
    const newData = this.data.concat();
    newData.sort((x, y) => {
      const kindx = x.kind;
      const kindy = y.kind;
      if (kindx > kindy) {
        return 1;
      }
      if (kindx < kindy) {
        return -1;
      }
      if (x.name > y.name) {
        return 1;
      }
      if (x.name < y.name) {
        return -1;
      }
      return 0;
    });
    return newData;
  }
}

/**
 * @param {string} target
 * @param {string} dependency
 * @param {DepsMap} depsMap
 */
function updateDepsKVByFile(target, dependency, depsMap) {
  const singleTon = fileTarget(dependency);
  const deps = depsMap.get(target);
  if (deps) {
    deps.add(singleTon);
  } else {
    depsMap.set(target, new TargetSet([singleTon]));
  }
}

/**
 * @param {string} s
 */
function uncapitalize(s) {
  if (s.length === 0) {
    return s;
  }
  return s[0].toLowerCase() + s.slice(1);
}

/**
 * @param {string} target
 * @param {string[]} dependencies
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByFile(target, dependencies, depsMap) {
  const targets = fileTargets(dependencies);
  const deps = depsMap.get(target);
  if (deps) {
    for (const target of targets) {
      deps.add(target);
    }
  } else {
    depsMap.set(target, new TargetSet(targets));
  }
}

/**
 * @param {string} target
 * @param {string[]} modules
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByModule(target, modules, depsMap) {
  const deps = depsMap.get(target);
  if (deps === undefined) {
    return;
  }
  for (const module of modules) {
    const filename = uncapitalize(module);
    const filenameAsCmi = `${filename}.cmi`;
    const filenameAsCmj = `${filename}.cmj`;
    if (target.endsWith(".cmi")) {
      if (depsMap.has(filenameAsCmi) || depsMap.has(filenameAsCmj)) {
        deps.add(fileTarget(filenameAsCmi));
      }
    } else if (target.endsWith(".cmj")) {
      if (depsMap.has(filenameAsCmj)) {
        deps.add(fileTarget(filenameAsCmj));
      } else if (depsMap.has(filenameAsCmi)) {
        deps.add(fileTarget(filenameAsCmi));
      }
    }
  }
}

/**
 * @param {string[]}sources
 * @return {DepsMap}
 */
function createDepsMapWithTargets(sources) {
  /**
   * @type {DepsMap}
   */
  const depsMap = new Map();
  for (const source of sources) {
    const target = sourceToTarget(source);
    depsMap.set(target, new TargetSet([]));
  }
  for (const [name, set] of depsMap) {
    const cmiFile = replaceExt(name, ".cmi");
    if (name.endsWith(".cmj") && depsMap.has(cmiFile)) {
      set.add(fileTarget(cmiFile));
    }
  }
  return depsMap;
}

/**
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
 * @param {Targets} files
 * @param {string} cwd
 *
 * @returns {string} return a string separated with whitespace
 */
function targetsToString(files, cwd) {
  return files.map(x => targetToString(x, cwd)).join(" ");
}
/**
 * @param {Targets} outputs
 * @param {Targets} inputs
 * @param {Targets} deps
 * @param {Override[]} overrides
 * @param {string} rule
 * @param {string} cwd
 * @return {string}
 */
function ninjaBuild(outputs, inputs, rule, deps, cwd, overrides) {
  const fileOutputs = targetsToString(outputs, cwd);
  const fileInputs = targetsToString(inputs, cwd);
  let stmt = `o ${fileOutputs} : ${rule} ${fileInputs}`;
  if (deps.length > 0) {
    const fileDeps = targetsToString(deps, cwd);
    stmt += ` | ${fileDeps}`;
  }
  if (overrides.length > 0) {
    stmt += "\n";
    stmt += overrides
      .map(x => {
        return `    ${x.key} = ${x.value}`;
      })
      .join("\n");
  }
  return stmt;
}

/**
 * @param {Target} outputs
 * @param {Targets} inputs
 * @param {string} cwd
 */
function phony(outputs, inputs, cwd) {
  return ninjaBuild([outputs], inputs, "phony", [], cwd, []);
}

/**
 * @param {string | string[]} outputs
 * @param {string | string[]} inputs
 * @param {string | string[]} fileDeps
 * @param {string} rule
 * @param {string} cwd
 * @param {Array<[key: string, value: string]>} overrides
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
  const os = Array.isArray(outputs)
    ? fileTargets(outputs)
    : [fileTarget(outputs)];
  const is = Array.isArray(inputs) ? fileTargets(inputs) : [fileTarget(inputs)];
  const ds = Array.isArray(fileDeps)
    ? fileTargets(fileDeps)
    : [fileTarget(fileDeps)];
  const dds = Array.isArray(extraDeps) ? extraDeps : [extraDeps];

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
 * @typedef {string | string[]} Strings
 * @typedef {[key: string, value: string]} KV
 * @typedef {[
 *   outputs: Strings,
 *   inputs: Strings,
 *   rule: string,
 *   cwd: string,
 *   overrides: KV[],
 *   deps: Strings,
 *   extraDeps: (Target | Targets)
 * ]} BuildList
 * @param {BuildList[]} xs
 * @returns {string}
 */
function ninjaQuickBuildList(xs) {
  return xs
    .map(x => ninjaQuickBuild(x[0], x[1], x[2], x[3], x[4], x[5], x[6]))
    .join("\n");
}

/**
 * @typedef {[output: string, input: string, string?]} CppoInput
 * @param {CppoInput[]} xs
 * @param {string} cwd
 * @returns {string}
 */
function cppoList(cwd, xs) {
  return xs
    .map(x => {
      /**
       * @type {KV[]}
       */
      let variables;
      if (x[2]) {
        variables = [["type", `-D ${x[2]}`]];
      } else {
        variables = [];
      }
      return ninjaQuickBuild(x[0], x[1], cppoRuleName, cwd, variables, [], []);
    })
    .join("\n");
}

/**
 * @param {string} name
 * @returns {Target}
 */
function fileTarget(name) {
  return { kind: "file", name };
}

/**
 * @param {string} name
 * @returns {Target}
 */
function pseudoTarget(name) {
  return { kind: "pseudo", name };
}

/**
 * @param {string[]} args
 * @returns {Targets}
 */
function fileTargets(args) {
  return args.map(name => fileTarget(name));
}

/**
 * @param {string[]} outputs
 * @param {string[]} inputs
 * @param {DepsMap} depsMap
 * @param {Override[]} overrides
 * @param {Targets} extraDeps
 * @param {string} rule
 * @param {string} cwd
 */
function buildStmt(outputs, inputs, rule, depsMap, cwd, overrides, extraDeps) {
  const os = outputs.map(fileTarget);
  const is = inputs.map(fileTarget);
  const deps = new TargetSet();
  for (const output of outputs) {
    const curDeps = depsMap.get(output);
    if (curDeps !== undefined) {
      for (const dep of curDeps) {
        deps.add(dep);
      }
    }
  }
  for (const dep of extraDeps) {
    deps.add(dep);
  }
  return ninjaBuild(os, is, rule, deps.toSortedArray(), cwd, overrides);
}

/**
 * @param {string} x
 */
function replaceCmj(x) {
  return x.trim().replace("cmx", "cmj");
}

/**
 * @param {string} y
 */
function sourceToTarget(y) {
  if (y.endsWith(".res")) {
    return replaceExt(y, ".cmj");
  }
  if (y.endsWith(".resi")) {
    return replaceExt(y, ".cmi");
  }
  return y;
}

/**
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 *
 * @param {string[]} files
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return {Promise<void>}
 */
function ocamlDepForBscAsync(files, dir, depsMap) {
  return new Promise((resolve, reject) => {
    /** @type {string | null} */
    let tmpdir = null;

    /** @type {string[]} */
    const mlfiles = []; // convert .res files to temporary .ml files in tmpdir

    for (const file of files) {
      const { name, ext } = path.parse(file);
      if (ext === ".res" || ext === ".resi") {
        const mlname = ext === ".resi" ? `${name}.mli` : `${name}.ml`;
        if (tmpdir == null) {
          tmpdir = fs.mkdtempSync(path.join(os.tmpdir(), "resToMl"));
        }
        try {
          const mlfile = path.join(tmpdir, mlname);
          cp.execSync(
            `${bsc_exe} -dsource -only-parse -bs-no-builtin-ppx ${file} 2>${mlfile}`,
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

      const minusI = tmpdir == null ? "" : `-I ${tmpdir}`;
      cp.exec(
        `ocamldep.opt -allow-approx -one-line ${minusI} -native ${files.join(
          " ",
        )} ${mlfiles.join(" ")}`,
        {
          cwd: dir,
          encoding: "ascii",
        },
        (error, stdout, stderr) => {
          if (tmpdir != null) {
            fs.rmSync(tmpdir, { recursive: true, force: true });
          }
          if (error !== null) {
            return reject(error);
          }
          const pairs = stdout.split("\n").map(x => x.split(":"));
          for (const [file, depsOutput] of pairs) {
            if (!depsOutput) continue;
            const source = replaceCmj(path.basename(file));
            const deps = depsOutput.trim().split(" ");
            updateDepsKVsByFile(
              source,
              deps.map(dep => replaceCmj(path.basename(dep))),
              depsMap,
            );
          }
          return resolve();
        },
      );
    }
  });
}

/**
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 *
 * @param {string[]} files
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return {Array<Promise<void>>}
 */
function depModulesForBscAsync(files, dir, depsMap) {
  const resFiles = files.filter(x => x.endsWith(".res") || x.endsWith(".resi"));
  /**
   *
   * @param {(value:void) =>void} resolve
   * @param {(value:any)=>void} reject
   */
  const cb = (resolve, reject) => {
    /**
     * @param {any} error
     * @param {string} stdout
     * @param {string} stderr
     */
    const fn = (error, stdout, stderr) => {
      if (error !== null) {
        return reject(error);
      }
      const pairs = stdout.split("\n").map(x => x.split(":"));
      for (const [sourceFile, modulesOutput] of pairs) {
        if (!modulesOutput) continue;
        const target = sourceToTarget(sourceFile.trim());
        const modules = modulesOutput.trim().split(" ");
        updateDepsKVsByModule(target, modules, depsMap);
      }
      return resolve();
    };
    return fn;
  };
  const config = {
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
 */
function collectTarget(sourceFiles) {
  /**
   * @type {Map<string, FileInfo>}
   */
  const allTargets = new Map();
  for (const file of sourceFiles) {
    const { ext, name } = path.parse(file);
    const existExt = allTargets.get(name);
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
  }
  return allTargets;
}

/**
 * @param {Map<string, FileInfo>} allTargets
 * @param {string[]} collIn
 * @returns {string[]} A new copy which is
 */
function scanFileTargets(allTargets, collIn) {
  const coll = collIn.concat();
  for (const [mod, ext] of allTargets) {
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
  }
  return coll;
}

/**
 * @param {DepsMap} depsMap
 * @param {Map<string, string>} allTargets
 * @param {string} cwd
 * @param {Targets} extraDeps
 * @return {string[]}
 */
function generateNinja(depsMap, allTargets, cwd, extraDeps = []) {
  /**
   * @type {string[]}
   */
  const build_stmts = [];
  allTargets.forEach((x, mod) => {
    const ouptput_cmj = `${mod}.cmj`;
    const output_cmi = `${mod}.cmi`;
    const input_res = `${mod}.res`;
    const input_resi = `${mod}.resi`;
    /**
     * @type {Override[]}
     */
    const overrides = [];

    /**
     * @param {string[]} outputs
     * @param {string[]} inputs
     */
    const mk = (outputs, inputs, rule = "cc") => {
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

const COMPILIER = bsc_exe;
const BSC_COMPILER = `bsc = ${COMPILIER}`;

async function runtimeNinja(devmode = true) {
  const ninjaCwd = "runtime";
  const compilerTarget = pseudoTarget("$bsc");
  const externalDeps = devmode ? [compilerTarget] : [];
  const ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  const templateRuntimeRules = `
bsc_no_open_flags =  ${commonBsFlags} -bs-cross-module-opt -make-runtime  -nopervasives  -unsafe -w +50 -warn-error A
bsc_flags = $bsc_no_open_flags -open Bs_stdlib_mini
${ruleCC(ninjaCwd)}
${ninjaQuickBuildList([
  [
    "bs_stdlib_mini.cmi",
    "bs_stdlib_mini.resi",
    "cc",
    ninjaCwd,
    [["bsc_flags", "-nostdlib -nopervasives"]],
    [],
    externalDeps,
  ],
  [
    ["js.cmj", "js.cmi"],
    "js.res",
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
  const depsMap = new Map();
  const allTargets = collectTarget([...runtimeMliFiles, ...runtimeMlFiles]);
  const manualDeps = ["bs_stdlib_mini.cmi", "js.cmj", "js.cmi"];
  const allFileTargetsInRuntime = scanFileTargets(allTargets, manualDeps);
  allTargets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_RESI":
      case "HAS_BOTH_RES":
        updateDepsKVsByFile(`${mod}.cmi`, manualDeps, depsMap);
        break;
      case "HAS_RES":
        updateDepsKVsByFile(`${mod}.cmj`, manualDeps, depsMap);
        break;
    }
  });
  // FIXME: in dev mode, it should not rely on reading js file
  // since it may cause a bootstrapping issues
  try {
    await Promise.all([
      runJSCheckAsync(depsMap),
      ocamlDepForBscAsync(runtimeSourceFiles, compilerRuntimeDir, depsMap),
    ]);
    const stmts = generateNinja(depsMap, allTargets, ninjaCwd, externalDeps);
    stmts.push(
      phony(runtimeTarget, fileTargets(allFileTargetsInRuntime), ninjaCwd),
    );
    writeFileAscii(
      path.join(compilerRuntimeDir, ninjaOutput),
      `${templateRuntimeRules + stmts.join("\n")}\n`,
    );
  } catch (e) {
    console.log(e);
  }
}

const cppoRuleName = "cppo";
const cppoRule = (flags = "") => `
rule ${cppoRuleName}
    command = cppo -V OCAML:${getVersionString()} ${flags} $type $in -o $out
    generator = true
`;

async function othersNinja(devmode = true) {
  const compilerTarget = pseudoTarget("$bsc");
  const externalDeps = [
    compilerTarget,
    fileTarget("belt_internals.cmi"),
    fileTarget("js.cmi"),
  ];
  const ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  const ninjaCwd = "others";

  const templateOthersRules = `
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
  const othersDirFiles = fs.readdirSync(compilerBuiltinDir, "ascii");
  const jsPrefixSourceFiles = othersDirFiles.filter(
    x =>
      x.startsWith("js") &&
      (x.endsWith(".res") || x.endsWith(".resi")) &&
      !x.includes(".cppo") &&
      !x.includes(".pp") &&
      !x.includes("#") &&
      x !== "js.res",
  );
  const othersFiles = othersDirFiles.filter(
    x =>
      !x.startsWith("js") &&
      x !== "belt.res" &&
      x !== "belt_internals.resi" &&
      (x.endsWith(".res") || x.endsWith(".resi")) &&
      !x.includes("#") &&
      !x.includes(".cppo"),
  );
  const jsTargets = collectTarget(jsPrefixSourceFiles);
  const allJsTargets = scanFileTargets(jsTargets, []);
  const jsDepsMap = new Map();
  const depsMap = new Map();
  await Promise.all([
    ocamlDepForBscAsync(jsPrefixSourceFiles, compilerBuiltinDir, jsDepsMap),
    ocamlDepForBscAsync(othersFiles, compilerBuiltinDir, depsMap),
  ]);
  const jsOutput = generateNinja(jsDepsMap, jsTargets, ninjaCwd, externalDeps);
  jsOutput.push(phony(js_package, fileTargets(allJsTargets), ninjaCwd));

  // Note compiling belt.res still try to read
  // belt_xx.cmi we need enforce the order to
  // avoid data race issues
  const beltPackage = fileTarget("belt.cmi");
  const beltTargets = collectTarget(othersFiles);
  depsMap.forEach((s, k) => {
    if (k.startsWith("belt")) {
      s.add(beltPackage);
    }
    s.add(js_package);
  });
  const allOthersTarget = scanFileTargets(beltTargets, []);
  const beltOutput = generateNinja(
    depsMap,
    beltTargets,
    ninjaCwd,
    externalDeps,
  );
  beltOutput.push(phony(othersTarget, fileTargets(allOthersTarget), ninjaCwd));
  writeFileAscii(
    path.join(compilerBuiltinDir, ninjaOutput),
    `${templateOthersRules + jsOutput.join("\n")}\n${beltOutput.join("\n")}\n`,
  );
}
/**
 * @param {boolean} devmode
 * generate build.ninja/release.ninja for stdlib-402
 */
async function stdlibNinja(devmode = true) {
  const stdlibVersion = "stdlib-406";
  const ninjaCwd = stdlibVersion;
  const stdlibDir = path.join(compilerRootDir, stdlibVersion);
  const compilerTarget = pseudoTarget("$bsc");
  const externalDeps = [compilerTarget, othersTarget];
  const ninjaOutput = devmode ? "build.ninja" : "release.ninja";
  const bsc_flags = "bsc_flags";
  /**
   * @type Array<[string, string]>
   */
  const bsc_builtin_overrides = [[bsc_flags, `$${bsc_flags} -nopervasives`]];
  // It is interesting `-w -a` would generate not great code sometimes
  // deprecations diabled due to string_of_float
  const warnings = "-w -9-3-106 -warn-error A";
  const templateStdlibRules = `
${bsc_flags} = ${commonBsFlags} -bs-cross-module-opt -make-runtime ${warnings} -I others
${ruleCC(ninjaCwd)}
${ninjaQuickBuildList([
  // we make it still depends on external
  // to enjoy free ride on dev config for compiler-deps

  [
    "pervasives.cmj",
    "pervasives.res",
    "cc_cmi",
    ninjaCwd,
    bsc_builtin_overrides,
    "pervasives.cmi",
    externalDeps,
  ],
  [
    "pervasives.cmi",
    "pervasives.resi",
    "cc",
    ninjaCwd,
    bsc_builtin_overrides,
    [],
    externalDeps,
  ],
])}
`;
  const stdlibDirFiles = fs.readdirSync(stdlibDir, "ascii");
  const sources = stdlibDirFiles.filter(x => {
    return (
      !x.startsWith("pervasives.") &&
      (x.endsWith(".res") || x.endsWith(".resi"))
    );
  });
  const depsMap = new Map();
  await ocamlDepForBscAsync(sources, stdlibDir, depsMap);
  const targets = collectTarget(sources);
  const allTargets = scanFileTargets(targets, []);
  targets.forEach((ext, mod) => {
    switch (ext) {
      case "HAS_RESI":
      case "HAS_BOTH_RES":
        updateDepsKVByFile(`${mod}.cmi`, "pervasives.cmj", depsMap);
        break;
      case "HAS_RES":
        updateDepsKVByFile(`${mod}.cmj`, "pervasives.cmj", depsMap);
        break;
    }
  });
  const output = generateNinja(depsMap, targets, ninjaCwd, externalDeps);
  output.push(phony(stdlibTarget, fileTargets(allTargets), ninjaCwd));

  writeFileAscii(
    path.join(stdlibDir, ninjaOutput),
    `${templateStdlibRules + output.join("\n")}\n`,
  );
}

/**
 * @param {string} text
 */
function getDeps(text) {
  /**
   * @type {string[]}
   */
  const deps = [];
  text.replace(
    /(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g,
    (_, ignore, id) => {
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
  const index = x.lastIndexOf(".");
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
  const ninjaOutput = "build.ninja";
  const ninjaCwd = "test";
  const templateTestRules = `
bsc_flags = -bs-cross-module-opt -make-runtime-test -bs-package-output commonjs:jscomp/test  -w -3-6-26-27-29-30-32..40-44-45-52-60-9-106+104 -warn-error A  -I runtime -I $stdlib -I others
${ruleCC(ninjaCwd)}
`;
  const testDirFiles = fs.readdirSync(compilerTestDir, "ascii");
  const sources = testDirFiles.filter(x => {
    return x.endsWith(".resi") || x.endsWith(".res");
  });

  const depsMap = createDepsMapWithTargets(sources);
  await Promise.all(depModulesForBscAsync(sources, compilerTestDir, depsMap));
  const targets = collectTarget(sources);
  const output = generateNinja(depsMap, targets, ninjaCwd, [
    runtimeTarget,
    stdlibTarget,
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
    path.join(compilerTestDir, ninjaOutput),
    `${templateTestRules + output.join("\n")}\n`,
  );
}

/**
 *
 * @param {DepsMap} depsMap
 */
function runJSCheckAsync(depsMap) {
  return new Promise(resolve => {
    let count = 0;
    const tasks = runtimeJsFiles.length;
    const updateTick = () => {
      count++;
      if (count === tasks) {
        resolve(count);
      }
    };
    for (const name of runtimeFiles) {
      const jsFile = path.join(compilerBuiltinCjsOutputDir, `${name}.js`);
      fs.readFile(jsFile, "utf8", (err, fileContent) => {
        if (err === null) {
          const deps = getDeps(fileContent).map(
            x => `${path.parse(x).name}.cmj`,
          );
          fs.exists(path.join(compilerRuntimeDir, `${name}.resi`), exist => {
            if (exist) {
              deps.push(`${name}.cmi`);
            }
            updateDepsKVsByFile(`${name}.cmj`, deps, depsMap);
            updateTick();
          });
        } else {
          // file non exist or reading error ignore
          updateTick();
        }
      });
    }
  });
}

function checkEffect() {
  const jsPaths = runtimeJsFiles.map(x =>
    path.join(compilerBuiltinCjsOutputDir, `${x}.js`),
  );
  const effect = jsPaths
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
      }
      if (/Not a pure module/.test(x)) {
        return {
          file,
          effect: "false",
        };
      }
      return {
        file,
        effect: "unknown",
      };
    })
    .filter(({ effect }) => effect !== "pure")
    .map(({ file, effect }) => {
      return { file: path.basename(file), effect };
    });

  const black_list = new Set(["caml_lexer.js", "caml_parser.js"]);

  const assert = require("node:assert");
  assert(
    effect.length === black_list.size &&
      effect.every(x => black_list.has(x.file)),
  );

  console.log(effect);
}

function updateRelease() {
  runtimeNinja(false);
  stdlibNinja(false);
  othersNinja(false);
}

function updateDev() {
  writeFileAscii(
    path.join(compilerRootDir, "build.ninja"),
    `
stdlib = stdlib-406
${BSC_COMPILER}
ocamllex = ocamllex.opt
subninja runtime/build.ninja
subninja others/build.ninja
subninja $stdlib/build.ninja
subninja test/build.ninja
o all: phony runtime others $stdlib test
`,
  );
  writeFileAscii(
    path.join(compilerRootDir, "..", "lib", "build.ninja"),
    `
ocamlopt = ocamlopt.opt
ext = exe
INCL= "4.06.1+BS"
include body.ninja
`,
  );

  preprocessorNinjaSync(); // This is needed so that ocamldep makes sense
  runtimeNinja();
  stdlibNinja(true);
  if (fs.existsSync(bsc_exe)) {
    testNinja();
  }
  othersNinja();
}
exports.updateDev = updateDev;
exports.updateRelease = updateRelease;

function preprocessorNinjaSync() {
  const dTypeString = "TYPE_STRING";
  const dTypeInt = "TYPE_INT";

  const cppoNative = `
${cppoRule("-n")}
${cppoList("others", [
  ["belt_HashSetString.res", "hashset.cppo.res", dTypeString],
  ["belt_HashSetString.resi", "hashset.cppo.resi", dTypeString],
  ["belt_HashSetInt.res", "hashset.cppo.res", dTypeInt],
  ["belt_HashSetInt.resi", "hashset.cppo.resi", dTypeInt],
  ["belt_HashMapString.res", "hashmap.cppo.res", dTypeString],
  ["belt_HashMapString.resi", "hashmap.cppo.resi", dTypeString],
  ["belt_HashMapInt.res", "hashmap.cppo.res", dTypeInt],
  ["belt_HashMapInt.resi", "hashmap.cppo.resi", dTypeInt],
  ["belt_MapString.res", "map.cppo.res", dTypeString],
  ["belt_MapString.resi", "map.cppo.resi", dTypeString],
  ["belt_MapInt.res", "map.cppo.res", dTypeInt],
  ["belt_MapInt.resi", "map.cppo.resi", dTypeInt],
  ["belt_SetString.res", "belt_Set.cppo.res", dTypeString],
  ["belt_SetString.resi", "belt_Set.cppo.resi", dTypeString],
  ["belt_SetInt.res", "belt_Set.cppo.res", dTypeInt],
  ["belt_SetInt.resi", "belt_Set.cppo.resi", dTypeInt],
  ["belt_MutableMapString.res", "mapm.cppo.res", dTypeString],
  ["belt_MutableMapString.resi", "mapm.cppo.resi", dTypeString],
  ["belt_MutableMapInt.res", "mapm.cppo.res", dTypeInt],
  ["belt_MutableMapInt.resi", "mapm.cppo.resi", dTypeInt],
  ["belt_MutableSetString.res", "setm.cppo.res", dTypeString],
  ["belt_MutableSetString.resi", "setm.cppo.resi", dTypeString],
  ["belt_MutableSetInt.res", "setm.cppo.res", dTypeInt],
  ["belt_MutableSetInt.resi", "setm.cppo.resi", dTypeInt],
  ["belt_SortArrayString.res", "sort.cppo.res", dTypeString],
  ["belt_SortArrayString.resi", "sort.cppo.resi", dTypeString],
  ["belt_SortArrayInt.res", "sort.cppo.res", dTypeInt],
  ["belt_SortArrayInt.resi", "sort.cppo.resi", dTypeInt],
  ["belt_internalMapString.res", "internal_map.cppo.res", dTypeString],
  ["belt_internalMapInt.res", "internal_map.cppo.res", dTypeInt],
  ["belt_internalSetString.res", "internal_set.cppo.res", dTypeString],
  ["belt_internalSetInt.res", "internal_set.cppo.res", dTypeInt],
])}

rule copy
  command = cp $in $out
  description = $in -> $out
`;
  const cppoNinjaFile = "cppoVendor.ninja";
  writeFileSync(path.join(compilerRootDir, cppoNinjaFile), cppoNative);
  cp.execFileSync(vendorNinjaPath, ["-f", cppoNinjaFile, "--verbose", "-v"], {
    cwd: compilerRootDir,
    stdio: [0, 1, 2],
    encoding: "utf8",
  });
}

function main() {
  if (require.main === module) {
    if (process.argv.includes("-check")) {
      checkEffect();
    }

    const subcommand = process.argv[2];
    switch (subcommand) {
      case "build":
        try {
          cp.execFileSync(vendorNinjaPath, ["all"], {
            encoding: "utf8",
            cwd: compilerRootDir,
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
            cwd: compilerRootDir,
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
        console.log("config for the first time may take a while");
        updateDev();
        updateRelease();

        break;
      case "cleanbuild":
        console.log("run cleaning first");
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
          const dev = process.argv.includes("-dev");
          const release = process.argv.includes("-release");
          const all = process.argv.includes("-all");
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
