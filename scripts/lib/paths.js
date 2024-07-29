// @ts-check

const path = require("node:path");

/**
 * The project root path
 */
const projectDir = path.resolve(__dirname, "..", "..");

/**
 * path: `<projectDir>/jscomp/`
 */
const compilerRootDir = path.resolve(projectDir, "jscomp");

/**
 * path: `<projectDir>/jscomp/runtime`
 */
const compilerRuntimeDir = path.resolve(compilerRootDir, "runtime");

/**
 * path: `<projectDir>/jscomp/others/`
 */
const compilerBuiltinDir = path.resolve(compilerRootDir, "others");

/**
 * path: `<projectDir>/lib/js/`
 */
const compilerBuiltinCjsOutputDir = path.resolve(projectDir, "lib", "js");

/**
 * path: `<projectDir>/lib/es6/`
 */
const compilerBuiltinMjsOutputDir = path.resolve(projectDir, "lib", "es6");

/**
 * path: `<projectDir>/jscomp/test/`
 */
const compilerTestDir = path.resolve(compilerRootDir, "test");

/**
 * path: `<projectDir>/jscomp/build_tests/`
 */
const compilerBuildTestDir = path.resolve(compilerRootDir, "build_tests");

/**
 * path: `<projectDir>/jscomp/common/bs_version.ml`
 */
const compilerVersionFile = path.resolve(
  compilerRootDir,
  "common",
  "bs_version.ml",
);

/**
 * path: `<projectDir>/_build/install/default/bin/`
 */
const duneBinDir = path.resolve(
  projectDir,
  "_build",
  "install",
  "default",
  "bin",
);

/**
 * path: `<projectDir>/jscomp/gentype_tests/typescript-react-example/`
 */
const gentypeExampleDir = path.resolve(
  compilerRootDir,
  "gentype_tests",
  "typescript-react-example",
);

module.exports = {
  projectDir,
  compilerRootDir,
  compilerRuntimeDir,
  compilerBuiltinDir,
  compilerBuiltinCjsOutputDir,
  compilerBuiltinMjsOutputDir,
  compilerTestDir,
  compilerBuildTestDir,
  compilerVersionFile,
  duneBinDir,
  gentypeExampleDir,
};
