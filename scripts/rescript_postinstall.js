//@ts-check
const child_process = require("child_process");
const fs = require("fs");

const bsc_exe = require("./bin_path").bsc_exe;
const ninja_exe = require("./bin_path").ninja_exe;

function checkNinja() {
  if (!fs.existsSync(ninja_exe)) {
    throw new Error(
      `No ninja binary found for this platform. ${ninja_exe} does not exist.`
    );
  }

  try {
    return String(child_process.execFileSync(ninja_exe, ["--version"])).trim();
  } catch (e) {
    throw new Error(
      `Error getting ninja version. The ninja binary at ${ninja_exe} may not be compatible with this platform: ${e}`
    );
  }
}

function checkCompiler() {
  if (!fs.existsSync(bsc_exe)) {
    throw new Error(
      `No ReScript compiler binary found for this platform. ${bsc_exe} does not exist.`
    );
  }

  try {
    return String(child_process.execFileSync(bsc_exe, ["-v"])).trim();
  } catch (e) {
    throw new Error(
      `Error getting ReScript compiler version. The compiler binary at ${bsc_exe} may not be compatible with this platform: ${e}`
    );
  }
}

var ninjaVersion = checkNinja();
var compilerVersion = checkCompiler();

console.log(`${compilerVersion} (ninja ${ninjaVersion})`);
