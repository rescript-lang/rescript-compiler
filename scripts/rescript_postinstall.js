//@ts-check
var child_process = require("child_process");
var fs = require("fs");

var supported_os = ["darwin", "freebsd", "linux", "openbsd", "win32"];
if (supported_os.indexOf(process.platform) < 0) {
  throw new Error("Not supported platform" + process.platform);
}

var bsc_exe = require("./bin_path").bsc_exe;

var ninja_bin_output = require("./bin_path").ninja_exe;

function checkNinja() {
  var vendor_ninja_version = "1.9.0.git";

  // sanity check to make sure the binary actually runs. Used for Linux. Too many variants
  /**
   *
   * @param {string} binary_path
   */
  function test_ninja_compatible(binary_path) {
    var version;
    try {
      version = child_process
        .execSync(JSON.stringify(binary_path) + " --version", {
          encoding: "utf8",
          stdio: ["pipe", "pipe", "ignore"], // execSync outputs to stdout even if we catch the error. Silent it here
        })
        .trim();
    } catch (e) {
      console.log("ninja not compatible?", String(e));
      return false;
    }
    return version === vendor_ninja_version;
  }

  if (
    fs.existsSync(ninja_bin_output) &&
    test_ninja_compatible(ninja_bin_output)
  ) {
    console.log(
      "ninja binary is already cached and installed: ",
      ninja_bin_output
    );
  } else {
    throw new Error("No matching ninja binary found");
  }
}

function checkCompiler() {
  try {
    var version = String(child_process.execFileSync(bsc_exe, ["-v"]));
    console.log("ReScript compiler version:", version);
  } catch (e) {
    throw new Error("No working ReScript compiler");
  }
}

checkNinja();

checkCompiler();
